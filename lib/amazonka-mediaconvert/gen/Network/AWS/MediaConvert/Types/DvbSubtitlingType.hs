{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DvbSubtitlingType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DvbSubtitlingType where

import Network.AWS.Prelude

-- | Specify whether your DVB subtitles are standard or for hearing impaired. Choose hearing impaired if your subtitles include audio descriptions and dialogue. Choose standard if your subtitles include only dialogue.
data DvbSubtitlingType
  = HearingImpaired
  | Standard
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

instance FromText DvbSubtitlingType where
  parser =
    takeLowerText >>= \case
      "hearing_impaired" -> pure HearingImpaired
      "standard" -> pure Standard
      e ->
        fromTextError $
          "Failure parsing DvbSubtitlingType from value: '" <> e
            <> "'. Accepted values: hearing_impaired, standard"

instance ToText DvbSubtitlingType where
  toText = \case
    HearingImpaired -> "HEARING_IMPAIRED"
    Standard -> "STANDARD"

instance Hashable DvbSubtitlingType

instance NFData DvbSubtitlingType

instance ToByteString DvbSubtitlingType

instance ToQuery DvbSubtitlingType

instance ToHeader DvbSubtitlingType

instance ToJSON DvbSubtitlingType where
  toJSON = toJSONText

instance FromJSON DvbSubtitlingType where
  parseJSON = parseJSONText "DvbSubtitlingType"
