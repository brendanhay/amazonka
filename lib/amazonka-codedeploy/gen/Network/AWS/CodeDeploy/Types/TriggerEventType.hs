{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.TriggerEventType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Types.TriggerEventType
  ( TriggerEventType
    ( TriggerEventType'
    , TriggerEventTypeDeploymentStart
    , TriggerEventTypeDeploymentSuccess
    , TriggerEventTypeDeploymentFailure
    , TriggerEventTypeDeploymentStop
    , TriggerEventTypeDeploymentRollback
    , TriggerEventTypeDeploymentReady
    , TriggerEventTypeInstanceStart
    , TriggerEventTypeInstanceSuccess
    , TriggerEventTypeInstanceFailure
    , TriggerEventTypeInstanceReady
    , fromTriggerEventType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype TriggerEventType = TriggerEventType'{fromTriggerEventType
                                             :: Core.Text}
                             deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                             Core.Generic)
                             deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                               Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                               Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                               Core.FromText, Core.ToByteString, Core.ToQuery,
                                               Core.ToHeader)

pattern TriggerEventTypeDeploymentStart :: TriggerEventType
pattern TriggerEventTypeDeploymentStart = TriggerEventType' "DeploymentStart"

pattern TriggerEventTypeDeploymentSuccess :: TriggerEventType
pattern TriggerEventTypeDeploymentSuccess = TriggerEventType' "DeploymentSuccess"

pattern TriggerEventTypeDeploymentFailure :: TriggerEventType
pattern TriggerEventTypeDeploymentFailure = TriggerEventType' "DeploymentFailure"

pattern TriggerEventTypeDeploymentStop :: TriggerEventType
pattern TriggerEventTypeDeploymentStop = TriggerEventType' "DeploymentStop"

pattern TriggerEventTypeDeploymentRollback :: TriggerEventType
pattern TriggerEventTypeDeploymentRollback = TriggerEventType' "DeploymentRollback"

pattern TriggerEventTypeDeploymentReady :: TriggerEventType
pattern TriggerEventTypeDeploymentReady = TriggerEventType' "DeploymentReady"

pattern TriggerEventTypeInstanceStart :: TriggerEventType
pattern TriggerEventTypeInstanceStart = TriggerEventType' "InstanceStart"

pattern TriggerEventTypeInstanceSuccess :: TriggerEventType
pattern TriggerEventTypeInstanceSuccess = TriggerEventType' "InstanceSuccess"

pattern TriggerEventTypeInstanceFailure :: TriggerEventType
pattern TriggerEventTypeInstanceFailure = TriggerEventType' "InstanceFailure"

pattern TriggerEventTypeInstanceReady :: TriggerEventType
pattern TriggerEventTypeInstanceReady = TriggerEventType' "InstanceReady"

{-# COMPLETE 
  TriggerEventTypeDeploymentStart,

  TriggerEventTypeDeploymentSuccess,

  TriggerEventTypeDeploymentFailure,

  TriggerEventTypeDeploymentStop,

  TriggerEventTypeDeploymentRollback,

  TriggerEventTypeDeploymentReady,

  TriggerEventTypeInstanceStart,

  TriggerEventTypeInstanceSuccess,

  TriggerEventTypeInstanceFailure,

  TriggerEventTypeInstanceReady,
  TriggerEventType'
  #-}
