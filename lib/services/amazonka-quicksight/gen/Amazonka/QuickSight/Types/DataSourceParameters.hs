{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.QuickSight.Types.DataSourceParameters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DataSourceParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.AmazonElasticsearchParameters
import Amazonka.QuickSight.Types.AmazonOpenSearchParameters
import Amazonka.QuickSight.Types.AthenaParameters
import Amazonka.QuickSight.Types.AuroraParameters
import Amazonka.QuickSight.Types.AuroraPostgreSqlParameters
import Amazonka.QuickSight.Types.AwsIotAnalyticsParameters
import Amazonka.QuickSight.Types.DatabricksParameters
import Amazonka.QuickSight.Types.ExasolParameters
import Amazonka.QuickSight.Types.JiraParameters
import Amazonka.QuickSight.Types.MariaDbParameters
import Amazonka.QuickSight.Types.MySqlParameters
import Amazonka.QuickSight.Types.OracleParameters
import Amazonka.QuickSight.Types.PostgreSqlParameters
import Amazonka.QuickSight.Types.PrestoParameters
import Amazonka.QuickSight.Types.RdsParameters
import Amazonka.QuickSight.Types.RedshiftParameters
import Amazonka.QuickSight.Types.S3Parameters
import Amazonka.QuickSight.Types.ServiceNowParameters
import Amazonka.QuickSight.Types.SnowflakeParameters
import Amazonka.QuickSight.Types.SparkParameters
import Amazonka.QuickSight.Types.SqlServerParameters
import Amazonka.QuickSight.Types.TeradataParameters
import Amazonka.QuickSight.Types.TwitterParameters

-- | The parameters that Amazon QuickSight uses to connect to your underlying
-- data source. This is a variant type structure. For this structure to be
-- valid, only one of the attributes can be non-null.
--
-- /See:/ 'newDataSourceParameters' smart constructor.
data DataSourceParameters = DataSourceParameters'
  { -- | The parameters for OpenSearch.
    amazonElasticsearchParameters :: Prelude.Maybe AmazonElasticsearchParameters,
    -- | The parameters for OpenSearch.
    amazonOpenSearchParameters :: Prelude.Maybe AmazonOpenSearchParameters,
    -- | The parameters for Amazon Athena.
    athenaParameters :: Prelude.Maybe AthenaParameters,
    -- | The parameters for Amazon Aurora MySQL.
    auroraParameters :: Prelude.Maybe AuroraParameters,
    -- | The parameters for Amazon Aurora.
    auroraPostgreSqlParameters :: Prelude.Maybe AuroraPostgreSqlParameters,
    -- | The parameters for IoT Analytics.
    awsIotAnalyticsParameters :: Prelude.Maybe AwsIotAnalyticsParameters,
    -- | The required parameters that are needed to connect to a Databricks data
    -- source.
    databricksParameters :: Prelude.Maybe DatabricksParameters,
    -- | The parameters for Exasol.
    exasolParameters :: Prelude.Maybe ExasolParameters,
    -- | The parameters for Jira.
    jiraParameters :: Prelude.Maybe JiraParameters,
    -- | The parameters for MariaDB.
    mariaDbParameters :: Prelude.Maybe MariaDbParameters,
    -- | The parameters for MySQL.
    mySqlParameters :: Prelude.Maybe MySqlParameters,
    -- | The parameters for Oracle.
    oracleParameters :: Prelude.Maybe OracleParameters,
    -- | The parameters for PostgreSQL.
    postgreSqlParameters :: Prelude.Maybe PostgreSqlParameters,
    -- | The parameters for Presto.
    prestoParameters :: Prelude.Maybe PrestoParameters,
    -- | The parameters for Amazon RDS.
    rdsParameters :: Prelude.Maybe RdsParameters,
    -- | The parameters for Amazon Redshift.
    redshiftParameters :: Prelude.Maybe RedshiftParameters,
    -- | The parameters for S3.
    s3Parameters :: Prelude.Maybe S3Parameters,
    -- | The parameters for ServiceNow.
    serviceNowParameters :: Prelude.Maybe ServiceNowParameters,
    -- | The parameters for Snowflake.
    snowflakeParameters :: Prelude.Maybe SnowflakeParameters,
    -- | The parameters for Spark.
    sparkParameters :: Prelude.Maybe SparkParameters,
    -- | The parameters for SQL Server.
    sqlServerParameters :: Prelude.Maybe SqlServerParameters,
    -- | The parameters for Teradata.
    teradataParameters :: Prelude.Maybe TeradataParameters,
    -- | The parameters for Twitter.
    twitterParameters :: Prelude.Maybe TwitterParameters
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataSourceParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'amazonElasticsearchParameters', 'dataSourceParameters_amazonElasticsearchParameters' - The parameters for OpenSearch.
--
-- 'amazonOpenSearchParameters', 'dataSourceParameters_amazonOpenSearchParameters' - The parameters for OpenSearch.
--
-- 'athenaParameters', 'dataSourceParameters_athenaParameters' - The parameters for Amazon Athena.
--
-- 'auroraParameters', 'dataSourceParameters_auroraParameters' - The parameters for Amazon Aurora MySQL.
--
-- 'auroraPostgreSqlParameters', 'dataSourceParameters_auroraPostgreSqlParameters' - The parameters for Amazon Aurora.
--
-- 'awsIotAnalyticsParameters', 'dataSourceParameters_awsIotAnalyticsParameters' - The parameters for IoT Analytics.
--
-- 'databricksParameters', 'dataSourceParameters_databricksParameters' - The required parameters that are needed to connect to a Databricks data
-- source.
--
-- 'exasolParameters', 'dataSourceParameters_exasolParameters' - The parameters for Exasol.
--
-- 'jiraParameters', 'dataSourceParameters_jiraParameters' - The parameters for Jira.
--
-- 'mariaDbParameters', 'dataSourceParameters_mariaDbParameters' - The parameters for MariaDB.
--
-- 'mySqlParameters', 'dataSourceParameters_mySqlParameters' - The parameters for MySQL.
--
-- 'oracleParameters', 'dataSourceParameters_oracleParameters' - The parameters for Oracle.
--
-- 'postgreSqlParameters', 'dataSourceParameters_postgreSqlParameters' - The parameters for PostgreSQL.
--
-- 'prestoParameters', 'dataSourceParameters_prestoParameters' - The parameters for Presto.
--
-- 'rdsParameters', 'dataSourceParameters_rdsParameters' - The parameters for Amazon RDS.
--
-- 'redshiftParameters', 'dataSourceParameters_redshiftParameters' - The parameters for Amazon Redshift.
--
-- 's3Parameters', 'dataSourceParameters_s3Parameters' - The parameters for S3.
--
-- 'serviceNowParameters', 'dataSourceParameters_serviceNowParameters' - The parameters for ServiceNow.
--
-- 'snowflakeParameters', 'dataSourceParameters_snowflakeParameters' - The parameters for Snowflake.
--
-- 'sparkParameters', 'dataSourceParameters_sparkParameters' - The parameters for Spark.
--
-- 'sqlServerParameters', 'dataSourceParameters_sqlServerParameters' - The parameters for SQL Server.
--
-- 'teradataParameters', 'dataSourceParameters_teradataParameters' - The parameters for Teradata.
--
-- 'twitterParameters', 'dataSourceParameters_twitterParameters' - The parameters for Twitter.
newDataSourceParameters ::
  DataSourceParameters
newDataSourceParameters =
  DataSourceParameters'
    { amazonElasticsearchParameters =
        Prelude.Nothing,
      amazonOpenSearchParameters = Prelude.Nothing,
      athenaParameters = Prelude.Nothing,
      auroraParameters = Prelude.Nothing,
      auroraPostgreSqlParameters = Prelude.Nothing,
      awsIotAnalyticsParameters = Prelude.Nothing,
      databricksParameters = Prelude.Nothing,
      exasolParameters = Prelude.Nothing,
      jiraParameters = Prelude.Nothing,
      mariaDbParameters = Prelude.Nothing,
      mySqlParameters = Prelude.Nothing,
      oracleParameters = Prelude.Nothing,
      postgreSqlParameters = Prelude.Nothing,
      prestoParameters = Prelude.Nothing,
      rdsParameters = Prelude.Nothing,
      redshiftParameters = Prelude.Nothing,
      s3Parameters = Prelude.Nothing,
      serviceNowParameters = Prelude.Nothing,
      snowflakeParameters = Prelude.Nothing,
      sparkParameters = Prelude.Nothing,
      sqlServerParameters = Prelude.Nothing,
      teradataParameters = Prelude.Nothing,
      twitterParameters = Prelude.Nothing
    }

-- | The parameters for OpenSearch.
dataSourceParameters_amazonElasticsearchParameters :: Lens.Lens' DataSourceParameters (Prelude.Maybe AmazonElasticsearchParameters)
dataSourceParameters_amazonElasticsearchParameters = Lens.lens (\DataSourceParameters' {amazonElasticsearchParameters} -> amazonElasticsearchParameters) (\s@DataSourceParameters' {} a -> s {amazonElasticsearchParameters = a} :: DataSourceParameters)

-- | The parameters for OpenSearch.
dataSourceParameters_amazonOpenSearchParameters :: Lens.Lens' DataSourceParameters (Prelude.Maybe AmazonOpenSearchParameters)
dataSourceParameters_amazonOpenSearchParameters = Lens.lens (\DataSourceParameters' {amazonOpenSearchParameters} -> amazonOpenSearchParameters) (\s@DataSourceParameters' {} a -> s {amazonOpenSearchParameters = a} :: DataSourceParameters)

-- | The parameters for Amazon Athena.
dataSourceParameters_athenaParameters :: Lens.Lens' DataSourceParameters (Prelude.Maybe AthenaParameters)
dataSourceParameters_athenaParameters = Lens.lens (\DataSourceParameters' {athenaParameters} -> athenaParameters) (\s@DataSourceParameters' {} a -> s {athenaParameters = a} :: DataSourceParameters)

-- | The parameters for Amazon Aurora MySQL.
dataSourceParameters_auroraParameters :: Lens.Lens' DataSourceParameters (Prelude.Maybe AuroraParameters)
dataSourceParameters_auroraParameters = Lens.lens (\DataSourceParameters' {auroraParameters} -> auroraParameters) (\s@DataSourceParameters' {} a -> s {auroraParameters = a} :: DataSourceParameters)

-- | The parameters for Amazon Aurora.
dataSourceParameters_auroraPostgreSqlParameters :: Lens.Lens' DataSourceParameters (Prelude.Maybe AuroraPostgreSqlParameters)
dataSourceParameters_auroraPostgreSqlParameters = Lens.lens (\DataSourceParameters' {auroraPostgreSqlParameters} -> auroraPostgreSqlParameters) (\s@DataSourceParameters' {} a -> s {auroraPostgreSqlParameters = a} :: DataSourceParameters)

-- | The parameters for IoT Analytics.
dataSourceParameters_awsIotAnalyticsParameters :: Lens.Lens' DataSourceParameters (Prelude.Maybe AwsIotAnalyticsParameters)
dataSourceParameters_awsIotAnalyticsParameters = Lens.lens (\DataSourceParameters' {awsIotAnalyticsParameters} -> awsIotAnalyticsParameters) (\s@DataSourceParameters' {} a -> s {awsIotAnalyticsParameters = a} :: DataSourceParameters)

-- | The required parameters that are needed to connect to a Databricks data
-- source.
dataSourceParameters_databricksParameters :: Lens.Lens' DataSourceParameters (Prelude.Maybe DatabricksParameters)
dataSourceParameters_databricksParameters = Lens.lens (\DataSourceParameters' {databricksParameters} -> databricksParameters) (\s@DataSourceParameters' {} a -> s {databricksParameters = a} :: DataSourceParameters)

-- | The parameters for Exasol.
dataSourceParameters_exasolParameters :: Lens.Lens' DataSourceParameters (Prelude.Maybe ExasolParameters)
dataSourceParameters_exasolParameters = Lens.lens (\DataSourceParameters' {exasolParameters} -> exasolParameters) (\s@DataSourceParameters' {} a -> s {exasolParameters = a} :: DataSourceParameters)

-- | The parameters for Jira.
dataSourceParameters_jiraParameters :: Lens.Lens' DataSourceParameters (Prelude.Maybe JiraParameters)
dataSourceParameters_jiraParameters = Lens.lens (\DataSourceParameters' {jiraParameters} -> jiraParameters) (\s@DataSourceParameters' {} a -> s {jiraParameters = a} :: DataSourceParameters)

-- | The parameters for MariaDB.
dataSourceParameters_mariaDbParameters :: Lens.Lens' DataSourceParameters (Prelude.Maybe MariaDbParameters)
dataSourceParameters_mariaDbParameters = Lens.lens (\DataSourceParameters' {mariaDbParameters} -> mariaDbParameters) (\s@DataSourceParameters' {} a -> s {mariaDbParameters = a} :: DataSourceParameters)

-- | The parameters for MySQL.
dataSourceParameters_mySqlParameters :: Lens.Lens' DataSourceParameters (Prelude.Maybe MySqlParameters)
dataSourceParameters_mySqlParameters = Lens.lens (\DataSourceParameters' {mySqlParameters} -> mySqlParameters) (\s@DataSourceParameters' {} a -> s {mySqlParameters = a} :: DataSourceParameters)

-- | The parameters for Oracle.
dataSourceParameters_oracleParameters :: Lens.Lens' DataSourceParameters (Prelude.Maybe OracleParameters)
dataSourceParameters_oracleParameters = Lens.lens (\DataSourceParameters' {oracleParameters} -> oracleParameters) (\s@DataSourceParameters' {} a -> s {oracleParameters = a} :: DataSourceParameters)

-- | The parameters for PostgreSQL.
dataSourceParameters_postgreSqlParameters :: Lens.Lens' DataSourceParameters (Prelude.Maybe PostgreSqlParameters)
dataSourceParameters_postgreSqlParameters = Lens.lens (\DataSourceParameters' {postgreSqlParameters} -> postgreSqlParameters) (\s@DataSourceParameters' {} a -> s {postgreSqlParameters = a} :: DataSourceParameters)

-- | The parameters for Presto.
dataSourceParameters_prestoParameters :: Lens.Lens' DataSourceParameters (Prelude.Maybe PrestoParameters)
dataSourceParameters_prestoParameters = Lens.lens (\DataSourceParameters' {prestoParameters} -> prestoParameters) (\s@DataSourceParameters' {} a -> s {prestoParameters = a} :: DataSourceParameters)

-- | The parameters for Amazon RDS.
dataSourceParameters_rdsParameters :: Lens.Lens' DataSourceParameters (Prelude.Maybe RdsParameters)
dataSourceParameters_rdsParameters = Lens.lens (\DataSourceParameters' {rdsParameters} -> rdsParameters) (\s@DataSourceParameters' {} a -> s {rdsParameters = a} :: DataSourceParameters)

-- | The parameters for Amazon Redshift.
dataSourceParameters_redshiftParameters :: Lens.Lens' DataSourceParameters (Prelude.Maybe RedshiftParameters)
dataSourceParameters_redshiftParameters = Lens.lens (\DataSourceParameters' {redshiftParameters} -> redshiftParameters) (\s@DataSourceParameters' {} a -> s {redshiftParameters = a} :: DataSourceParameters)

-- | The parameters for S3.
dataSourceParameters_s3Parameters :: Lens.Lens' DataSourceParameters (Prelude.Maybe S3Parameters)
dataSourceParameters_s3Parameters = Lens.lens (\DataSourceParameters' {s3Parameters} -> s3Parameters) (\s@DataSourceParameters' {} a -> s {s3Parameters = a} :: DataSourceParameters)

-- | The parameters for ServiceNow.
dataSourceParameters_serviceNowParameters :: Lens.Lens' DataSourceParameters (Prelude.Maybe ServiceNowParameters)
dataSourceParameters_serviceNowParameters = Lens.lens (\DataSourceParameters' {serviceNowParameters} -> serviceNowParameters) (\s@DataSourceParameters' {} a -> s {serviceNowParameters = a} :: DataSourceParameters)

-- | The parameters for Snowflake.
dataSourceParameters_snowflakeParameters :: Lens.Lens' DataSourceParameters (Prelude.Maybe SnowflakeParameters)
dataSourceParameters_snowflakeParameters = Lens.lens (\DataSourceParameters' {snowflakeParameters} -> snowflakeParameters) (\s@DataSourceParameters' {} a -> s {snowflakeParameters = a} :: DataSourceParameters)

-- | The parameters for Spark.
dataSourceParameters_sparkParameters :: Lens.Lens' DataSourceParameters (Prelude.Maybe SparkParameters)
dataSourceParameters_sparkParameters = Lens.lens (\DataSourceParameters' {sparkParameters} -> sparkParameters) (\s@DataSourceParameters' {} a -> s {sparkParameters = a} :: DataSourceParameters)

-- | The parameters for SQL Server.
dataSourceParameters_sqlServerParameters :: Lens.Lens' DataSourceParameters (Prelude.Maybe SqlServerParameters)
dataSourceParameters_sqlServerParameters = Lens.lens (\DataSourceParameters' {sqlServerParameters} -> sqlServerParameters) (\s@DataSourceParameters' {} a -> s {sqlServerParameters = a} :: DataSourceParameters)

-- | The parameters for Teradata.
dataSourceParameters_teradataParameters :: Lens.Lens' DataSourceParameters (Prelude.Maybe TeradataParameters)
dataSourceParameters_teradataParameters = Lens.lens (\DataSourceParameters' {teradataParameters} -> teradataParameters) (\s@DataSourceParameters' {} a -> s {teradataParameters = a} :: DataSourceParameters)

-- | The parameters for Twitter.
dataSourceParameters_twitterParameters :: Lens.Lens' DataSourceParameters (Prelude.Maybe TwitterParameters)
dataSourceParameters_twitterParameters = Lens.lens (\DataSourceParameters' {twitterParameters} -> twitterParameters) (\s@DataSourceParameters' {} a -> s {twitterParameters = a} :: DataSourceParameters)

instance Data.FromJSON DataSourceParameters where
  parseJSON =
    Data.withObject
      "DataSourceParameters"
      ( \x ->
          DataSourceParameters'
            Prelude.<$> (x Data..:? "AmazonElasticsearchParameters")
            Prelude.<*> (x Data..:? "AmazonOpenSearchParameters")
            Prelude.<*> (x Data..:? "AthenaParameters")
            Prelude.<*> (x Data..:? "AuroraParameters")
            Prelude.<*> (x Data..:? "AuroraPostgreSqlParameters")
            Prelude.<*> (x Data..:? "AwsIotAnalyticsParameters")
            Prelude.<*> (x Data..:? "DatabricksParameters")
            Prelude.<*> (x Data..:? "ExasolParameters")
            Prelude.<*> (x Data..:? "JiraParameters")
            Prelude.<*> (x Data..:? "MariaDbParameters")
            Prelude.<*> (x Data..:? "MySqlParameters")
            Prelude.<*> (x Data..:? "OracleParameters")
            Prelude.<*> (x Data..:? "PostgreSqlParameters")
            Prelude.<*> (x Data..:? "PrestoParameters")
            Prelude.<*> (x Data..:? "RdsParameters")
            Prelude.<*> (x Data..:? "RedshiftParameters")
            Prelude.<*> (x Data..:? "S3Parameters")
            Prelude.<*> (x Data..:? "ServiceNowParameters")
            Prelude.<*> (x Data..:? "SnowflakeParameters")
            Prelude.<*> (x Data..:? "SparkParameters")
            Prelude.<*> (x Data..:? "SqlServerParameters")
            Prelude.<*> (x Data..:? "TeradataParameters")
            Prelude.<*> (x Data..:? "TwitterParameters")
      )

instance Prelude.Hashable DataSourceParameters where
  hashWithSalt _salt DataSourceParameters' {..} =
    _salt
      `Prelude.hashWithSalt` amazonElasticsearchParameters
      `Prelude.hashWithSalt` amazonOpenSearchParameters
      `Prelude.hashWithSalt` athenaParameters
      `Prelude.hashWithSalt` auroraParameters
      `Prelude.hashWithSalt` auroraPostgreSqlParameters
      `Prelude.hashWithSalt` awsIotAnalyticsParameters
      `Prelude.hashWithSalt` databricksParameters
      `Prelude.hashWithSalt` exasolParameters
      `Prelude.hashWithSalt` jiraParameters
      `Prelude.hashWithSalt` mariaDbParameters
      `Prelude.hashWithSalt` mySqlParameters
      `Prelude.hashWithSalt` oracleParameters
      `Prelude.hashWithSalt` postgreSqlParameters
      `Prelude.hashWithSalt` prestoParameters
      `Prelude.hashWithSalt` rdsParameters
      `Prelude.hashWithSalt` redshiftParameters
      `Prelude.hashWithSalt` s3Parameters
      `Prelude.hashWithSalt` serviceNowParameters
      `Prelude.hashWithSalt` snowflakeParameters
      `Prelude.hashWithSalt` sparkParameters
      `Prelude.hashWithSalt` sqlServerParameters
      `Prelude.hashWithSalt` teradataParameters
      `Prelude.hashWithSalt` twitterParameters

instance Prelude.NFData DataSourceParameters where
  rnf DataSourceParameters' {..} =
    Prelude.rnf amazonElasticsearchParameters
      `Prelude.seq` Prelude.rnf amazonOpenSearchParameters
      `Prelude.seq` Prelude.rnf athenaParameters
      `Prelude.seq` Prelude.rnf auroraParameters
      `Prelude.seq` Prelude.rnf auroraPostgreSqlParameters
      `Prelude.seq` Prelude.rnf awsIotAnalyticsParameters
      `Prelude.seq` Prelude.rnf databricksParameters
      `Prelude.seq` Prelude.rnf exasolParameters
      `Prelude.seq` Prelude.rnf jiraParameters
      `Prelude.seq` Prelude.rnf mariaDbParameters
      `Prelude.seq` Prelude.rnf mySqlParameters
      `Prelude.seq` Prelude.rnf oracleParameters
      `Prelude.seq` Prelude.rnf postgreSqlParameters
      `Prelude.seq` Prelude.rnf prestoParameters
      `Prelude.seq` Prelude.rnf rdsParameters
      `Prelude.seq` Prelude.rnf redshiftParameters
      `Prelude.seq` Prelude.rnf s3Parameters
      `Prelude.seq` Prelude.rnf serviceNowParameters
      `Prelude.seq` Prelude.rnf snowflakeParameters
      `Prelude.seq` Prelude.rnf sparkParameters
      `Prelude.seq` Prelude.rnf
        sqlServerParameters
      `Prelude.seq` Prelude.rnf
        teradataParameters
      `Prelude.seq` Prelude.rnf
        twitterParameters

instance Data.ToJSON DataSourceParameters where
  toJSON DataSourceParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AmazonElasticsearchParameters" Data..=)
              Prelude.<$> amazonElasticsearchParameters,
            ("AmazonOpenSearchParameters" Data..=)
              Prelude.<$> amazonOpenSearchParameters,
            ("AthenaParameters" Data..=)
              Prelude.<$> athenaParameters,
            ("AuroraParameters" Data..=)
              Prelude.<$> auroraParameters,
            ("AuroraPostgreSqlParameters" Data..=)
              Prelude.<$> auroraPostgreSqlParameters,
            ("AwsIotAnalyticsParameters" Data..=)
              Prelude.<$> awsIotAnalyticsParameters,
            ("DatabricksParameters" Data..=)
              Prelude.<$> databricksParameters,
            ("ExasolParameters" Data..=)
              Prelude.<$> exasolParameters,
            ("JiraParameters" Data..=)
              Prelude.<$> jiraParameters,
            ("MariaDbParameters" Data..=)
              Prelude.<$> mariaDbParameters,
            ("MySqlParameters" Data..=)
              Prelude.<$> mySqlParameters,
            ("OracleParameters" Data..=)
              Prelude.<$> oracleParameters,
            ("PostgreSqlParameters" Data..=)
              Prelude.<$> postgreSqlParameters,
            ("PrestoParameters" Data..=)
              Prelude.<$> prestoParameters,
            ("RdsParameters" Data..=) Prelude.<$> rdsParameters,
            ("RedshiftParameters" Data..=)
              Prelude.<$> redshiftParameters,
            ("S3Parameters" Data..=) Prelude.<$> s3Parameters,
            ("ServiceNowParameters" Data..=)
              Prelude.<$> serviceNowParameters,
            ("SnowflakeParameters" Data..=)
              Prelude.<$> snowflakeParameters,
            ("SparkParameters" Data..=)
              Prelude.<$> sparkParameters,
            ("SqlServerParameters" Data..=)
              Prelude.<$> sqlServerParameters,
            ("TeradataParameters" Data..=)
              Prelude.<$> teradataParameters,
            ("TwitterParameters" Data..=)
              Prelude.<$> twitterParameters
          ]
      )
