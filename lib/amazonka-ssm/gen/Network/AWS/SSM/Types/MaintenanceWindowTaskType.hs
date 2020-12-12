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
        Automation,
        Lambda,
        RunCommand,
        StepFunctions
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype MaintenanceWindowTaskType = MaintenanceWindowTaskType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern Automation :: MaintenanceWindowTaskType
pattern Automation = MaintenanceWindowTaskType' "AUTOMATION"

pattern Lambda :: MaintenanceWindowTaskType
pattern Lambda = MaintenanceWindowTaskType' "LAMBDA"

pattern RunCommand :: MaintenanceWindowTaskType
pattern RunCommand = MaintenanceWindowTaskType' "RUN_COMMAND"

pattern StepFunctions :: MaintenanceWindowTaskType
pattern StepFunctions = MaintenanceWindowTaskType' "STEP_FUNCTIONS"

{-# COMPLETE
  Automation,
  Lambda,
  RunCommand,
  StepFunctions,
  MaintenanceWindowTaskType'
  #-}
