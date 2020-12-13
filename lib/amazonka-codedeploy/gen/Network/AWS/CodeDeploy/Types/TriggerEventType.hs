{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.TriggerEventType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.TriggerEventType
  ( TriggerEventType
      ( TriggerEventType',
        DeploymentStart,
        DeploymentSuccess,
        DeploymentFailure,
        DeploymentStop,
        DeploymentRollback,
        DeploymentReady,
        InstanceStart,
        InstanceSuccess,
        InstanceFailure,
        InstanceReady
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype TriggerEventType = TriggerEventType' Lude.Text
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

pattern DeploymentStart :: TriggerEventType
pattern DeploymentStart = TriggerEventType' "DeploymentStart"

pattern DeploymentSuccess :: TriggerEventType
pattern DeploymentSuccess = TriggerEventType' "DeploymentSuccess"

pattern DeploymentFailure :: TriggerEventType
pattern DeploymentFailure = TriggerEventType' "DeploymentFailure"

pattern DeploymentStop :: TriggerEventType
pattern DeploymentStop = TriggerEventType' "DeploymentStop"

pattern DeploymentRollback :: TriggerEventType
pattern DeploymentRollback = TriggerEventType' "DeploymentRollback"

pattern DeploymentReady :: TriggerEventType
pattern DeploymentReady = TriggerEventType' "DeploymentReady"

pattern InstanceStart :: TriggerEventType
pattern InstanceStart = TriggerEventType' "InstanceStart"

pattern InstanceSuccess :: TriggerEventType
pattern InstanceSuccess = TriggerEventType' "InstanceSuccess"

pattern InstanceFailure :: TriggerEventType
pattern InstanceFailure = TriggerEventType' "InstanceFailure"

pattern InstanceReady :: TriggerEventType
pattern InstanceReady = TriggerEventType' "InstanceReady"

{-# COMPLETE
  DeploymentStart,
  DeploymentSuccess,
  DeploymentFailure,
  DeploymentStop,
  DeploymentRollback,
  DeploymentReady,
  InstanceStart,
  InstanceSuccess,
  InstanceFailure,
  InstanceReady,
  TriggerEventType'
  #-}
