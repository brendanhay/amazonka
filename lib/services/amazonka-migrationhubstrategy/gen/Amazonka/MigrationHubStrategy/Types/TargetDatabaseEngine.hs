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
-- Module      : Amazonka.MigrationHubStrategy.Types.TargetDatabaseEngine
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.TargetDatabaseEngine
  ( TargetDatabaseEngine
      ( ..,
        TargetDatabaseEngine_AWS_PostgreSQL,
        TargetDatabaseEngine_Amazon_Aurora,
        TargetDatabaseEngine_Db2_LUW,
        TargetDatabaseEngine_MariaDB,
        TargetDatabaseEngine_Microsoft_SQL_Server,
        TargetDatabaseEngine_MongoDB,
        TargetDatabaseEngine_MySQL,
        TargetDatabaseEngine_None_specified,
        TargetDatabaseEngine_Oracle_Database,
        TargetDatabaseEngine_SAP
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TargetDatabaseEngine = TargetDatabaseEngine'
  { fromTargetDatabaseEngine ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern TargetDatabaseEngine_AWS_PostgreSQL :: TargetDatabaseEngine
pattern TargetDatabaseEngine_AWS_PostgreSQL = TargetDatabaseEngine' "AWS PostgreSQL"

pattern TargetDatabaseEngine_Amazon_Aurora :: TargetDatabaseEngine
pattern TargetDatabaseEngine_Amazon_Aurora = TargetDatabaseEngine' "Amazon Aurora"

pattern TargetDatabaseEngine_Db2_LUW :: TargetDatabaseEngine
pattern TargetDatabaseEngine_Db2_LUW = TargetDatabaseEngine' "Db2 LUW"

pattern TargetDatabaseEngine_MariaDB :: TargetDatabaseEngine
pattern TargetDatabaseEngine_MariaDB = TargetDatabaseEngine' "MariaDB"

pattern TargetDatabaseEngine_Microsoft_SQL_Server :: TargetDatabaseEngine
pattern TargetDatabaseEngine_Microsoft_SQL_Server = TargetDatabaseEngine' "Microsoft SQL Server"

pattern TargetDatabaseEngine_MongoDB :: TargetDatabaseEngine
pattern TargetDatabaseEngine_MongoDB = TargetDatabaseEngine' "MongoDB"

pattern TargetDatabaseEngine_MySQL :: TargetDatabaseEngine
pattern TargetDatabaseEngine_MySQL = TargetDatabaseEngine' "MySQL"

pattern TargetDatabaseEngine_None_specified :: TargetDatabaseEngine
pattern TargetDatabaseEngine_None_specified = TargetDatabaseEngine' "None specified"

pattern TargetDatabaseEngine_Oracle_Database :: TargetDatabaseEngine
pattern TargetDatabaseEngine_Oracle_Database = TargetDatabaseEngine' "Oracle Database"

pattern TargetDatabaseEngine_SAP :: TargetDatabaseEngine
pattern TargetDatabaseEngine_SAP = TargetDatabaseEngine' "SAP"

{-# COMPLETE
  TargetDatabaseEngine_AWS_PostgreSQL,
  TargetDatabaseEngine_Amazon_Aurora,
  TargetDatabaseEngine_Db2_LUW,
  TargetDatabaseEngine_MariaDB,
  TargetDatabaseEngine_Microsoft_SQL_Server,
  TargetDatabaseEngine_MongoDB,
  TargetDatabaseEngine_MySQL,
  TargetDatabaseEngine_None_specified,
  TargetDatabaseEngine_Oracle_Database,
  TargetDatabaseEngine_SAP,
  TargetDatabaseEngine'
  #-}
