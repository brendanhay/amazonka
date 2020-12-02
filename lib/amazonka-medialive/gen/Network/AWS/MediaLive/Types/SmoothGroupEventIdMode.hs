{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.SmoothGroupEventIdMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.SmoothGroupEventIdMode where

import Network.AWS.Prelude

-- | Smooth Group Event Id Mode
data SmoothGroupEventIdMode
  = NoEventId
  | UseConfigured
  | UseTimestamp
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

instance FromText SmoothGroupEventIdMode where
  parser =
    takeLowerText >>= \case
      "no_event_id" -> pure NoEventId
      "use_configured" -> pure UseConfigured
      "use_timestamp" -> pure UseTimestamp
      e ->
        fromTextError $
          "Failure parsing SmoothGroupEventIdMode from value: '" <> e
            <> "'. Accepted values: no_event_id, use_configured, use_timestamp"

instance ToText SmoothGroupEventIdMode where
  toText = \case
    NoEventId -> "NO_EVENT_ID"
    UseConfigured -> "USE_CONFIGURED"
    UseTimestamp -> "USE_TIMESTAMP"

instance Hashable SmoothGroupEventIdMode

instance NFData SmoothGroupEventIdMode

instance ToByteString SmoothGroupEventIdMode

instance ToQuery SmoothGroupEventIdMode

instance ToHeader SmoothGroupEventIdMode

instance ToJSON SmoothGroupEventIdMode where
  toJSON = toJSONText

instance FromJSON SmoothGroupEventIdMode where
  parseJSON = parseJSONText "SmoothGroupEventIdMode"
