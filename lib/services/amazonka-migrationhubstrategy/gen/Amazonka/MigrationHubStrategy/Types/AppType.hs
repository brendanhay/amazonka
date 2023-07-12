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
-- Module      : Amazonka.MigrationHubStrategy.Types.AppType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.AppType
  ( AppType
      ( ..,
        AppType_Cassandra,
        AppType_DB2,
        AppType_DotNetFramework,
        AppType_Dotnet,
        AppType_DotnetCore,
        AppType_IBM_WebSphere,
        AppType_IIS,
        AppType_JBoss,
        AppType_Java,
        AppType_Maria_DB,
        AppType_Mongo_DB,
        AppType_MySQL,
        AppType_Oracle,
        AppType_Oracle_WebLogic,
        AppType_Other,
        AppType_PostgreSQLServer,
        AppType_SQLServer,
        AppType_Spring,
        AppType_Sybase,
        AppType_Tomcat,
        AppType_Unknown,
        AppType_Visual_Basic
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AppType = AppType' {fromAppType :: Data.Text}
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

pattern AppType_Cassandra :: AppType
pattern AppType_Cassandra = AppType' "Cassandra"

pattern AppType_DB2 :: AppType
pattern AppType_DB2 = AppType' "DB2"

pattern AppType_DotNetFramework :: AppType
pattern AppType_DotNetFramework = AppType' "DotNetFramework"

pattern AppType_Dotnet :: AppType
pattern AppType_Dotnet = AppType' "Dotnet"

pattern AppType_DotnetCore :: AppType
pattern AppType_DotnetCore = AppType' "DotnetCore"

pattern AppType_IBM_WebSphere :: AppType
pattern AppType_IBM_WebSphere = AppType' "IBM WebSphere"

pattern AppType_IIS :: AppType
pattern AppType_IIS = AppType' "IIS"

pattern AppType_JBoss :: AppType
pattern AppType_JBoss = AppType' "JBoss"

pattern AppType_Java :: AppType
pattern AppType_Java = AppType' "Java"

pattern AppType_Maria_DB :: AppType
pattern AppType_Maria_DB = AppType' "Maria DB"

pattern AppType_Mongo_DB :: AppType
pattern AppType_Mongo_DB = AppType' "Mongo DB"

pattern AppType_MySQL :: AppType
pattern AppType_MySQL = AppType' "MySQL"

pattern AppType_Oracle :: AppType
pattern AppType_Oracle = AppType' "Oracle"

pattern AppType_Oracle_WebLogic :: AppType
pattern AppType_Oracle_WebLogic = AppType' "Oracle WebLogic"

pattern AppType_Other :: AppType
pattern AppType_Other = AppType' "Other"

pattern AppType_PostgreSQLServer :: AppType
pattern AppType_PostgreSQLServer = AppType' "PostgreSQLServer"

pattern AppType_SQLServer :: AppType
pattern AppType_SQLServer = AppType' "SQLServer"

pattern AppType_Spring :: AppType
pattern AppType_Spring = AppType' "Spring"

pattern AppType_Sybase :: AppType
pattern AppType_Sybase = AppType' "Sybase"

pattern AppType_Tomcat :: AppType
pattern AppType_Tomcat = AppType' "Tomcat"

pattern AppType_Unknown :: AppType
pattern AppType_Unknown = AppType' "Unknown"

pattern AppType_Visual_Basic :: AppType
pattern AppType_Visual_Basic = AppType' "Visual Basic"

{-# COMPLETE
  AppType_Cassandra,
  AppType_DB2,
  AppType_DotNetFramework,
  AppType_Dotnet,
  AppType_DotnetCore,
  AppType_IBM_WebSphere,
  AppType_IIS,
  AppType_JBoss,
  AppType_Java,
  AppType_Maria_DB,
  AppType_Mongo_DB,
  AppType_MySQL,
  AppType_Oracle,
  AppType_Oracle_WebLogic,
  AppType_Other,
  AppType_PostgreSQLServer,
  AppType_SQLServer,
  AppType_Spring,
  AppType_Sybase,
  AppType_Tomcat,
  AppType_Unknown,
  AppType_Visual_Basic,
  AppType'
  #-}
