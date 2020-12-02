{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.AutoRollbackEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.AutoRollbackEvent where

import Network.AWS.Prelude

data AutoRollbackEvent
  = AREDeploymentFailure
  | AREDeploymentStopOnAlarm
  | AREDeploymentStopOnRequest
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText AutoRollbackEvent where
  parser =
    takeLowerText >>= \case
      "deployment_failure" -> pure AREDeploymentFailure
      "deployment_stop_on_alarm" -> pure AREDeploymentStopOnAlarm
      "deployment_stop_on_request" -> pure AREDeploymentStopOnRequest
      e ->
        fromTextError $
          "Failure parsing AutoRollbackEvent from value: '" <> e
            <> "'. Accepted values: deployment_failure, deployment_stop_on_alarm, deployment_stop_on_request"

instance ToText AutoRollbackEvent where
  toText = \case
    AREDeploymentFailure -> "DEPLOYMENT_FAILURE"
    AREDeploymentStopOnAlarm -> "DEPLOYMENT_STOP_ON_ALARM"
    AREDeploymentStopOnRequest -> "DEPLOYMENT_STOP_ON_REQUEST"

instance Hashable AutoRollbackEvent

instance NFData AutoRollbackEvent

instance ToByteString AutoRollbackEvent

instance ToQuery AutoRollbackEvent

instance ToHeader AutoRollbackEvent

instance ToJSON AutoRollbackEvent where
  toJSON = toJSONText

instance FromJSON AutoRollbackEvent where
  parseJSON = parseJSONText "AutoRollbackEvent"
