{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.MaintenanceWindowTaskType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.MaintenanceWindowTaskType
  ( MaintenanceWindowTaskType
      ( MaintenanceWindowTaskType',
        MaintenanceWindowTaskTypeRunCommand,
        MaintenanceWindowTaskTypeAutomation,
        MaintenanceWindowTaskTypeStepFunctions,
        MaintenanceWindowTaskTypeLambda,
        fromMaintenanceWindowTaskType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype MaintenanceWindowTaskType = MaintenanceWindowTaskType'
  { fromMaintenanceWindowTaskType ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern MaintenanceWindowTaskTypeRunCommand :: MaintenanceWindowTaskType
pattern MaintenanceWindowTaskTypeRunCommand = MaintenanceWindowTaskType' "RUN_COMMAND"

pattern MaintenanceWindowTaskTypeAutomation :: MaintenanceWindowTaskType
pattern MaintenanceWindowTaskTypeAutomation = MaintenanceWindowTaskType' "AUTOMATION"

pattern MaintenanceWindowTaskTypeStepFunctions :: MaintenanceWindowTaskType
pattern MaintenanceWindowTaskTypeStepFunctions = MaintenanceWindowTaskType' "STEP_FUNCTIONS"

pattern MaintenanceWindowTaskTypeLambda :: MaintenanceWindowTaskType
pattern MaintenanceWindowTaskTypeLambda = MaintenanceWindowTaskType' "LAMBDA"

{-# COMPLETE
  MaintenanceWindowTaskTypeRunCommand,
  MaintenanceWindowTaskTypeAutomation,
  MaintenanceWindowTaskTypeStepFunctions,
  MaintenanceWindowTaskTypeLambda,
  MaintenanceWindowTaskType'
  #-}
