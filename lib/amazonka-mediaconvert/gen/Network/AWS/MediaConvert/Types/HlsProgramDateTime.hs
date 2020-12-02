{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.HlsProgramDateTime
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.HlsProgramDateTime where

import Network.AWS.Prelude

-- | Includes or excludes EXT-X-PROGRAM-DATE-TIME tag in .m3u8 manifest files. The value is calculated as follows: either the program date and time are initialized using the input timecode source, or the time is initialized using the input timecode source and the date is initialized using the timestamp_offset.
data HlsProgramDateTime
  = HPDTExclude
  | HPDTInclude
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

instance FromText HlsProgramDateTime where
  parser =
    takeLowerText >>= \case
      "exclude" -> pure HPDTExclude
      "include" -> pure HPDTInclude
      e ->
        fromTextError $
          "Failure parsing HlsProgramDateTime from value: '" <> e
            <> "'. Accepted values: exclude, include"

instance ToText HlsProgramDateTime where
  toText = \case
    HPDTExclude -> "EXCLUDE"
    HPDTInclude -> "INCLUDE"

instance Hashable HlsProgramDateTime

instance NFData HlsProgramDateTime

instance ToByteString HlsProgramDateTime

instance ToQuery HlsProgramDateTime

instance ToHeader HlsProgramDateTime

instance ToJSON HlsProgramDateTime where
  toJSON = toJSONText

instance FromJSON HlsProgramDateTime where
  parseJSON = parseJSONText "HlsProgramDateTime"
