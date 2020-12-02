{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DolbyVisionProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DolbyVisionProfile where

import Network.AWS.Prelude

-- | In the current MediaConvert implementation, the Dolby Vision profile is always 5 (PROFILE_5). Therefore, all of your inputs must contain Dolby Vision frame interleaved data.
data DolbyVisionProfile = Profile5
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

instance FromText DolbyVisionProfile where
  parser =
    takeLowerText >>= \case
      "profile_5" -> pure Profile5
      e ->
        fromTextError $
          "Failure parsing DolbyVisionProfile from value: '" <> e
            <> "'. Accepted values: profile_5"

instance ToText DolbyVisionProfile where
  toText = \case
    Profile5 -> "PROFILE_5"

instance Hashable DolbyVisionProfile

instance NFData DolbyVisionProfile

instance ToByteString DolbyVisionProfile

instance ToQuery DolbyVisionProfile

instance ToHeader DolbyVisionProfile

instance ToJSON DolbyVisionProfile where
  toJSON = toJSONText

instance FromJSON DolbyVisionProfile where
  parseJSON = parseJSONText "DolbyVisionProfile"
