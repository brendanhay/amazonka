{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AacCodecProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AacCodecProfile where

import Network.AWS.Prelude

-- | AAC Profile.
data AacCodecProfile
  = ACPHEV1
  | ACPHEV2
  | ACPLC
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

instance FromText AacCodecProfile where
  parser =
    takeLowerText >>= \case
      "hev1" -> pure ACPHEV1
      "hev2" -> pure ACPHEV2
      "lc" -> pure ACPLC
      e ->
        fromTextError $
          "Failure parsing AacCodecProfile from value: '" <> e
            <> "'. Accepted values: hev1, hev2, lc"

instance ToText AacCodecProfile where
  toText = \case
    ACPHEV1 -> "HEV1"
    ACPHEV2 -> "HEV2"
    ACPLC -> "LC"

instance Hashable AacCodecProfile

instance NFData AacCodecProfile

instance ToByteString AacCodecProfile

instance ToQuery AacCodecProfile

instance ToHeader AacCodecProfile

instance ToJSON AacCodecProfile where
  toJSON = toJSONText

instance FromJSON AacCodecProfile where
  parseJSON = parseJSONText "AacCodecProfile"
