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
-- Module      : Amazonka.MigrationHubStrategy.Types.TransformationToolName
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.TransformationToolName
  ( TransformationToolName
      ( ..,
        TransformationToolName_App2Container,
        TransformationToolName_Application_Migration_Service,
        TransformationToolName_Database_Migration_Service,
        TransformationToolName_End_of_Support_Migration,
        TransformationToolName_In_Place_Operating_System_Upgrade,
        TransformationToolName_Native_SQL_Server_Backup_Restore,
        TransformationToolName_Porting_Assistant_For__NET,
        TransformationToolName_Schema_Conversion_Tool,
        TransformationToolName_Strategy_Recommendation_Support,
        TransformationToolName_Windows_Web_Application_Migration_Assistant
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TransformationToolName = TransformationToolName'
  { fromTransformationToolName ::
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

pattern TransformationToolName_App2Container :: TransformationToolName
pattern TransformationToolName_App2Container = TransformationToolName' "App2Container"

pattern TransformationToolName_Application_Migration_Service :: TransformationToolName
pattern TransformationToolName_Application_Migration_Service = TransformationToolName' "Application Migration Service"

pattern TransformationToolName_Database_Migration_Service :: TransformationToolName
pattern TransformationToolName_Database_Migration_Service = TransformationToolName' "Database Migration Service"

pattern TransformationToolName_End_of_Support_Migration :: TransformationToolName
pattern TransformationToolName_End_of_Support_Migration = TransformationToolName' "End of Support Migration"

pattern TransformationToolName_In_Place_Operating_System_Upgrade :: TransformationToolName
pattern TransformationToolName_In_Place_Operating_System_Upgrade = TransformationToolName' "In Place Operating System Upgrade"

pattern TransformationToolName_Native_SQL_Server_Backup_Restore :: TransformationToolName
pattern TransformationToolName_Native_SQL_Server_Backup_Restore = TransformationToolName' "Native SQL Server Backup/Restore"

pattern TransformationToolName_Porting_Assistant_For__NET :: TransformationToolName
pattern TransformationToolName_Porting_Assistant_For__NET = TransformationToolName' "Porting Assistant For .NET"

pattern TransformationToolName_Schema_Conversion_Tool :: TransformationToolName
pattern TransformationToolName_Schema_Conversion_Tool = TransformationToolName' "Schema Conversion Tool"

pattern TransformationToolName_Strategy_Recommendation_Support :: TransformationToolName
pattern TransformationToolName_Strategy_Recommendation_Support = TransformationToolName' "Strategy Recommendation Support"

pattern TransformationToolName_Windows_Web_Application_Migration_Assistant :: TransformationToolName
pattern TransformationToolName_Windows_Web_Application_Migration_Assistant = TransformationToolName' "Windows Web Application Migration Assistant"

{-# COMPLETE
  TransformationToolName_App2Container,
  TransformationToolName_Application_Migration_Service,
  TransformationToolName_Database_Migration_Service,
  TransformationToolName_End_of_Support_Migration,
  TransformationToolName_In_Place_Operating_System_Upgrade,
  TransformationToolName_Native_SQL_Server_Backup_Restore,
  TransformationToolName_Porting_Assistant_For__NET,
  TransformationToolName_Schema_Conversion_Tool,
  TransformationToolName_Strategy_Recommendation_Support,
  TransformationToolName_Windows_Web_Application_Migration_Assistant,
  TransformationToolName'
  #-}
