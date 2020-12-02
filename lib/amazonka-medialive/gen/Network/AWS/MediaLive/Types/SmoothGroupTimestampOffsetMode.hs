{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.SmoothGroupTimestampOffsetMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.SmoothGroupTimestampOffsetMode where

import Network.AWS.Prelude

-- | Smooth Group Timestamp Offset Mode
data SmoothGroupTimestampOffsetMode
  = UseConfiguredOffset
  | UseEventStartDate
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

instance FromText SmoothGroupTimestampOffsetMode where
  parser =
    takeLowerText >>= \case
      "use_configured_offset" -> pure UseConfiguredOffset
      "use_event_start_date" -> pure UseEventStartDate
      e ->
        fromTextError $
          "Failure parsing SmoothGroupTimestampOffsetMode from value: '" <> e
            <> "'. Accepted values: use_configured_offset, use_event_start_date"

instance ToText SmoothGroupTimestampOffsetMode where
  toText = \case
    UseConfiguredOffset -> "USE_CONFIGURED_OFFSET"
    UseEventStartDate -> "USE_EVENT_START_DATE"

instance Hashable SmoothGroupTimestampOffsetMode

instance NFData SmoothGroupTimestampOffsetMode

instance ToByteString SmoothGroupTimestampOffsetMode

instance ToQuery SmoothGroupTimestampOffsetMode

instance ToHeader SmoothGroupTimestampOffsetMode

instance ToJSON SmoothGroupTimestampOffsetMode where
  toJSON = toJSONText

instance FromJSON SmoothGroupTimestampOffsetMode where
  parseJSON = parseJSONText "SmoothGroupTimestampOffsetMode"
