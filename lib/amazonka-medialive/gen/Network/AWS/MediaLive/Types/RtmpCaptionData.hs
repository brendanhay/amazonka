{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.RtmpCaptionData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.RtmpCaptionData where

import Network.AWS.Prelude

-- | Rtmp Caption Data
data RtmpCaptionData
  = All
  | FIELD1608
  | FIELD1AndFIELD2608
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

instance FromText RtmpCaptionData where
  parser =
    takeLowerText >>= \case
      "all" -> pure All
      "field1_608" -> pure FIELD1608
      "field1_and_field2_608" -> pure FIELD1AndFIELD2608
      e ->
        fromTextError $
          "Failure parsing RtmpCaptionData from value: '" <> e
            <> "'. Accepted values: all, field1_608, field1_and_field2_608"

instance ToText RtmpCaptionData where
  toText = \case
    All -> "ALL"
    FIELD1608 -> "FIELD1_608"
    FIELD1AndFIELD2608 -> "FIELD1_AND_FIELD2_608"

instance Hashable RtmpCaptionData

instance NFData RtmpCaptionData

instance ToByteString RtmpCaptionData

instance ToQuery RtmpCaptionData

instance ToHeader RtmpCaptionData

instance ToJSON RtmpCaptionData where
  toJSON = toJSONText

instance FromJSON RtmpCaptionData where
  parseJSON = parseJSONText "RtmpCaptionData"
