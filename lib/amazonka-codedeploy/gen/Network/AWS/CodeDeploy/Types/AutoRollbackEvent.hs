-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.AutoRollbackEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.AutoRollbackEvent
  ( AutoRollbackEvent
      ( AutoRollbackEvent',
        AREDeploymentFailure,
        AREDeploymentStopOnAlarm,
        AREDeploymentStopOnRequest
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype AutoRollbackEvent = AutoRollbackEvent' Lude.Text
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

pattern AREDeploymentFailure :: AutoRollbackEvent
pattern AREDeploymentFailure = AutoRollbackEvent' "DEPLOYMENT_FAILURE"

pattern AREDeploymentStopOnAlarm :: AutoRollbackEvent
pattern AREDeploymentStopOnAlarm = AutoRollbackEvent' "DEPLOYMENT_STOP_ON_ALARM"

pattern AREDeploymentStopOnRequest :: AutoRollbackEvent
pattern AREDeploymentStopOnRequest = AutoRollbackEvent' "DEPLOYMENT_STOP_ON_REQUEST"

{-# COMPLETE
  AREDeploymentFailure,
  AREDeploymentStopOnAlarm,
  AREDeploymentStopOnRequest,
  AutoRollbackEvent'
  #-}
