{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.DeploymentWaitType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.DeploymentWaitType where

import Network.AWS.Prelude

data DeploymentWaitType
  = ReadyWait
  | TerminationWait
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

instance FromText DeploymentWaitType where
  parser =
    takeLowerText >>= \case
      "ready_wait" -> pure ReadyWait
      "termination_wait" -> pure TerminationWait
      e ->
        fromTextError $
          "Failure parsing DeploymentWaitType from value: '" <> e
            <> "'. Accepted values: ready_wait, termination_wait"

instance ToText DeploymentWaitType where
  toText = \case
    ReadyWait -> "READY_WAIT"
    TerminationWait -> "TERMINATION_WAIT"

instance Hashable DeploymentWaitType

instance NFData DeploymentWaitType

instance ToByteString DeploymentWaitType

instance ToQuery DeploymentWaitType

instance ToHeader DeploymentWaitType

instance ToJSON DeploymentWaitType where
  toJSON = toJSONText
