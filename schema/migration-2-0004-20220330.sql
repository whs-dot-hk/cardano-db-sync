-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 4 THEN
    EXECUTE 'ALTER TABLE "stake_registration" ADD COLUMN "tx_id" INT8 NOT NULL' ;
    EXECUTE 'ALTER TABLE "stake_registration" DROP CONSTRAINT "unique_stake_registration"' ;
    EXECUTE 'ALTER TABLE "stake_registration" ADD CONSTRAINT "unique_stake_registration" UNIQUE("tx_id","cert_index")' ;
    EXECUTE 'ALTER TABLE "stake_deregistration" ADD COLUMN "tx_id" INT8 NOT NULL' ;
    EXECUTE 'ALTER TABLE "stake_deregistration" DROP CONSTRAINT "unique_stake_deregistration"' ;
    EXECUTE 'ALTER TABLE "stake_deregistration" ADD CONSTRAINT "unique_stake_deregistration" UNIQUE("tx_id","cert_index")' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
