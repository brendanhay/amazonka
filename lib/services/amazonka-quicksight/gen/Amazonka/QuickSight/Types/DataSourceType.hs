{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.QuickSight.Types.DataSourceType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DataSourceType
  ( DataSourceType
      ( ..,
        DataSourceType_ADOBE_ANALYTICS,
        DataSourceType_AMAZON_ELASTICSEARCH,
        DataSourceType_AMAZON_OPENSEARCH,
        DataSourceType_ATHENA,
        DataSourceType_AURORA,
        DataSourceType_AURORA_POSTGRESQL,
        DataSourceType_AWS_IOT_ANALYTICS,
        DataSourceType_DATABRICKS,
        DataSourceType_EXASOL,
        DataSourceType_GITHUB,
        DataSourceType_JIRA,
        DataSourceType_MARIADB,
        DataSourceType_MYSQL,
        DataSourceType_ORACLE,
        DataSourceType_POSTGRESQL,
        DataSourceType_PRESTO,
        DataSourceType_REDSHIFT,
        DataSourceType_S3,
        DataSourceType_SALESFORCE,
        DataSourceType_SERVICENOW,
        DataSourceType_SNOWFLAKE,
        DataSourceType_SPARK,
        DataSourceType_SQLSERVER,
        DataSourceType_TERADATA,
        DataSourceType_TIMESTREAM,
        DataSourceType_TWITTER
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype DataSourceType = DataSourceType'
  { fromDataSourceType ::
      Core.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern DataSourceType_ADOBE_ANALYTICS :: DataSourceType
pattern DataSourceType_ADOBE_ANALYTICS = DataSourceType' "ADOBE_ANALYTICS"

pattern DataSourceType_AMAZON_ELASTICSEARCH :: DataSourceType
pattern DataSourceType_AMAZON_ELASTICSEARCH = DataSourceType' "AMAZON_ELASTICSEARCH"

pattern DataSourceType_AMAZON_OPENSEARCH :: DataSourceType
pattern DataSourceType_AMAZON_OPENSEARCH = DataSourceType' "AMAZON_OPENSEARCH"

pattern DataSourceType_ATHENA :: DataSourceType
pattern DataSourceType_ATHENA = DataSourceType' "ATHENA"

pattern DataSourceType_AURORA :: DataSourceType
pattern DataSourceType_AURORA = DataSourceType' "AURORA"

pattern DataSourceType_AURORA_POSTGRESQL :: DataSourceType
pattern DataSourceType_AURORA_POSTGRESQL = DataSourceType' "AURORA_POSTGRESQL"

pattern DataSourceType_AWS_IOT_ANALYTICS :: DataSourceType
pattern DataSourceType_AWS_IOT_ANALYTICS = DataSourceType' "AWS_IOT_ANALYTICS"

pattern DataSourceType_DATABRICKS :: DataSourceType
pattern DataSourceType_DATABRICKS = DataSourceType' "DATABRICKS"

pattern DataSourceType_EXASOL :: DataSourceType
pattern DataSourceType_EXASOL = DataSourceType' "EXASOL"

pattern DataSourceType_GITHUB :: DataSourceType
pattern DataSourceType_GITHUB = DataSourceType' "GITHUB"

pattern DataSourceType_JIRA :: DataSourceType
pattern DataSourceType_JIRA = DataSourceType' "JIRA"

pattern DataSourceType_MARIADB :: DataSourceType
pattern DataSourceType_MARIADB = DataSourceType' "MARIADB"

pattern DataSourceType_MYSQL :: DataSourceType
pattern DataSourceType_MYSQL = DataSourceType' "MYSQL"

pattern DataSourceType_ORACLE :: DataSourceType
pattern DataSourceType_ORACLE = DataSourceType' "ORACLE"

pattern DataSourceType_POSTGRESQL :: DataSourceType
pattern DataSourceType_POSTGRESQL = DataSourceType' "POSTGRESQL"

pattern DataSourceType_PRESTO :: DataSourceType
pattern DataSourceType_PRESTO = DataSourceType' "PRESTO"

pattern DataSourceType_REDSHIFT :: DataSourceType
pattern DataSourceType_REDSHIFT = DataSourceType' "REDSHIFT"

pattern DataSourceType_S3 :: DataSourceType
pattern DataSourceType_S3 = DataSourceType' "S3"

pattern DataSourceType_SALESFORCE :: DataSourceType
pattern DataSourceType_SALESFORCE = DataSourceType' "SALESFORCE"

pattern DataSourceType_SERVICENOW :: DataSourceType
pattern DataSourceType_SERVICENOW = DataSourceType' "SERVICENOW"

pattern DataSourceType_SNOWFLAKE :: DataSourceType
pattern DataSourceType_SNOWFLAKE = DataSourceType' "SNOWFLAKE"

pattern DataSourceType_SPARK :: DataSourceType
pattern DataSourceType_SPARK = DataSourceType' "SPARK"

pattern DataSourceType_SQLSERVER :: DataSourceType
pattern DataSourceType_SQLSERVER = DataSourceType' "SQLSERVER"

pattern DataSourceType_TERADATA :: DataSourceType
pattern DataSourceType_TERADATA = DataSourceType' "TERADATA"

pattern DataSourceType_TIMESTREAM :: DataSourceType
pattern DataSourceType_TIMESTREAM = DataSourceType' "TIMESTREAM"

pattern DataSourceType_TWITTER :: DataSourceType
pattern DataSourceType_TWITTER = DataSourceType' "TWITTER"

{-# COMPLETE
  DataSourceType_ADOBE_ANALYTICS,
  DataSourceType_AMAZON_ELASTICSEARCH,
  DataSourceType_AMAZON_OPENSEARCH,
  DataSourceType_ATHENA,
  DataSourceType_AURORA,
  DataSourceType_AURORA_POSTGRESQL,
  DataSourceType_AWS_IOT_ANALYTICS,
  DataSourceType_DATABRICKS,
  DataSourceType_EXASOL,
  DataSourceType_GITHUB,
  DataSourceType_JIRA,
  DataSourceType_MARIADB,
  DataSourceType_MYSQL,
  DataSourceType_ORACLE,
  DataSourceType_POSTGRESQL,
  DataSourceType_PRESTO,
  DataSourceType_REDSHIFT,
  DataSourceType_S3,
  DataSourceType_SALESFORCE,
  DataSourceType_SERVICENOW,
  DataSourceType_SNOWFLAKE,
  DataSourceType_SPARK,
  DataSourceType_SQLSERVER,
  DataSourceType_TERADATA,
  DataSourceType_TIMESTREAM,
  DataSourceType_TWITTER,
  DataSourceType'
  #-}
