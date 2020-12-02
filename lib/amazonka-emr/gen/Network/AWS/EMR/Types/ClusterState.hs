{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.ClusterState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.ClusterState where

import Network.AWS.Prelude

data ClusterState
  = CSBootstrapping
  | CSRunning
  | CSStarting
  | CSTerminated
  | CSTerminatedWithErrors
  | CSTerminating
  | CSWaiting
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

instance FromText ClusterState where
  parser =
    takeLowerText >>= \case
      "bootstrapping" -> pure CSBootstrapping
      "running" -> pure CSRunning
      "starting" -> pure CSStarting
      "terminated" -> pure CSTerminated
      "terminated_with_errors" -> pure CSTerminatedWithErrors
      "terminating" -> pure CSTerminating
      "waiting" -> pure CSWaiting
      e ->
        fromTextError $
          "Failure parsing ClusterState from value: '" <> e
            <> "'. Accepted values: bootstrapping, running, starting, terminated, terminated_with_errors, terminating, waiting"

instance ToText ClusterState where
  toText = \case
    CSBootstrapping -> "BOOTSTRAPPING"
    CSRunning -> "RUNNING"
    CSStarting -> "STARTING"
    CSTerminated -> "TERMINATED"
    CSTerminatedWithErrors -> "TERMINATED_WITH_ERRORS"
    CSTerminating -> "TERMINATING"
    CSWaiting -> "WAITING"

instance Hashable ClusterState

instance NFData ClusterState

instance ToByteString ClusterState

instance ToQuery ClusterState

instance ToHeader ClusterState

instance ToJSON ClusterState where
  toJSON = toJSONText

instance FromJSON ClusterState where
  parseJSON = parseJSONText "ClusterState"
