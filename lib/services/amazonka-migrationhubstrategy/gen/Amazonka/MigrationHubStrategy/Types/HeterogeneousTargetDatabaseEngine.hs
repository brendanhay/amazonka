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
-- Module      : Amazonka.MigrationHubStrategy.Types.HeterogeneousTargetDatabaseEngine
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.HeterogeneousTargetDatabaseEngine
  ( HeterogeneousTargetDatabaseEngine
      ( ..,
        HeterogeneousTargetDatabaseEngine_AWS_PostgreSQL,
        HeterogeneousTargetDatabaseEngine_Amazon_Aurora,
        HeterogeneousTargetDatabaseEngine_Db2_LUW,
        HeterogeneousTargetDatabaseEngine_MariaDB,
        HeterogeneousTargetDatabaseEngine_Microsoft_SQL_Server,
        HeterogeneousTargetDatabaseEngine_MongoDB,
        HeterogeneousTargetDatabaseEngine_MySQL,
        HeterogeneousTargetDatabaseEngine_None_specified,
        HeterogeneousTargetDatabaseEngine_Oracle_Database,
        HeterogeneousTargetDatabaseEngine_SAP
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype HeterogeneousTargetDatabaseEngine = HeterogeneousTargetDatabaseEngine'
  { fromHeterogeneousTargetDatabaseEngine ::
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

pattern HeterogeneousTargetDatabaseEngine_AWS_PostgreSQL :: HeterogeneousTargetDatabaseEngine
pattern HeterogeneousTargetDatabaseEngine_AWS_PostgreSQL = HeterogeneousTargetDatabaseEngine' "AWS PostgreSQL"

pattern HeterogeneousTargetDatabaseEngine_Amazon_Aurora :: HeterogeneousTargetDatabaseEngine
pattern HeterogeneousTargetDatabaseEngine_Amazon_Aurora = HeterogeneousTargetDatabaseEngine' "Amazon Aurora"

pattern HeterogeneousTargetDatabaseEngine_Db2_LUW :: HeterogeneousTargetDatabaseEngine
pattern HeterogeneousTargetDatabaseEngine_Db2_LUW = HeterogeneousTargetDatabaseEngine' "Db2 LUW"

pattern HeterogeneousTargetDatabaseEngine_MariaDB :: HeterogeneousTargetDatabaseEngine
pattern HeterogeneousTargetDatabaseEngine_MariaDB = HeterogeneousTargetDatabaseEngine' "MariaDB"

pattern HeterogeneousTargetDatabaseEngine_Microsoft_SQL_Server :: HeterogeneousTargetDatabaseEngine
pattern HeterogeneousTargetDatabaseEngine_Microsoft_SQL_Server = HeterogeneousTargetDatabaseEngine' "Microsoft SQL Server"

pattern HeterogeneousTargetDatabaseEngine_MongoDB :: HeterogeneousTargetDatabaseEngine
pattern HeterogeneousTargetDatabaseEngine_MongoDB = HeterogeneousTargetDatabaseEngine' "MongoDB"

pattern HeterogeneousTargetDatabaseEngine_MySQL :: HeterogeneousTargetDatabaseEngine
pattern HeterogeneousTargetDatabaseEngine_MySQL = HeterogeneousTargetDatabaseEngine' "MySQL"

pattern HeterogeneousTargetDatabaseEngine_None_specified :: HeterogeneousTargetDatabaseEngine
pattern HeterogeneousTargetDatabaseEngine_None_specified = HeterogeneousTargetDatabaseEngine' "None specified"

pattern HeterogeneousTargetDatabaseEngine_Oracle_Database :: HeterogeneousTargetDatabaseEngine
pattern HeterogeneousTargetDatabaseEngine_Oracle_Database = HeterogeneousTargetDatabaseEngine' "Oracle Database"

pattern HeterogeneousTargetDatabaseEngine_SAP :: HeterogeneousTargetDatabaseEngine
pattern HeterogeneousTargetDatabaseEngine_SAP = HeterogeneousTargetDatabaseEngine' "SAP"

{-# COMPLETE
  HeterogeneousTargetDatabaseEngine_AWS_PostgreSQL,
  HeterogeneousTargetDatabaseEngine_Amazon_Aurora,
  HeterogeneousTargetDatabaseEngine_Db2_LUW,
  HeterogeneousTargetDatabaseEngine_MariaDB,
  HeterogeneousTargetDatabaseEngine_Microsoft_SQL_Server,
  HeterogeneousTargetDatabaseEngine_MongoDB,
  HeterogeneousTargetDatabaseEngine_MySQL,
  HeterogeneousTargetDatabaseEngine_None_specified,
  HeterogeneousTargetDatabaseEngine_Oracle_Database,
  HeterogeneousTargetDatabaseEngine_SAP,
  HeterogeneousTargetDatabaseEngine'
  #-}
