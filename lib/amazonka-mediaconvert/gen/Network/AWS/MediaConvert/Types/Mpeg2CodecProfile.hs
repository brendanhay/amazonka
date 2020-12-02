{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mpeg2CodecProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Mpeg2CodecProfile where

import Network.AWS.Prelude

-- | Use Profile (Mpeg2CodecProfile) to set the MPEG-2 profile for the video output.
data Mpeg2CodecProfile
  = Main
  | Profile422
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

instance FromText Mpeg2CodecProfile where
  parser =
    takeLowerText >>= \case
      "main" -> pure Main
      "profile_422" -> pure Profile422
      e ->
        fromTextError $
          "Failure parsing Mpeg2CodecProfile from value: '" <> e
            <> "'. Accepted values: main, profile_422"

instance ToText Mpeg2CodecProfile where
  toText = \case
    Main -> "MAIN"
    Profile422 -> "PROFILE_422"

instance Hashable Mpeg2CodecProfile

instance NFData Mpeg2CodecProfile

instance ToByteString Mpeg2CodecProfile

instance ToQuery Mpeg2CodecProfile

instance ToHeader Mpeg2CodecProfile

instance ToJSON Mpeg2CodecProfile where
  toJSON = toJSONText

instance FromJSON Mpeg2CodecProfile where
  parseJSON = parseJSONText "Mpeg2CodecProfile"
