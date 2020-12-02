{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.RealtimeEndpointStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.RealtimeEndpointStatus where

import Network.AWS.Prelude

data RealtimeEndpointStatus
  = Failed
  | None
  | Ready
  | Updating
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

instance FromText RealtimeEndpointStatus where
  parser =
    takeLowerText >>= \case
      "failed" -> pure Failed
      "none" -> pure None
      "ready" -> pure Ready
      "updating" -> pure Updating
      e ->
        fromTextError $
          "Failure parsing RealtimeEndpointStatus from value: '" <> e
            <> "'. Accepted values: failed, none, ready, updating"

instance ToText RealtimeEndpointStatus where
  toText = \case
    Failed -> "FAILED"
    None -> "NONE"
    Ready -> "READY"
    Updating -> "UPDATING"

instance Hashable RealtimeEndpointStatus

instance NFData RealtimeEndpointStatus

instance ToByteString RealtimeEndpointStatus

instance ToQuery RealtimeEndpointStatus

instance ToHeader RealtimeEndpointStatus

instance FromJSON RealtimeEndpointStatus where
  parseJSON = parseJSONText "RealtimeEndpointStatus"
