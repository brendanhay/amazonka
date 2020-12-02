{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AacProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AacProfile where

import Network.AWS.Prelude

-- | Aac Profile
data AacProfile
  = APHEV1
  | APHEV2
  | APLC
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

instance FromText AacProfile where
  parser =
    takeLowerText >>= \case
      "hev1" -> pure APHEV1
      "hev2" -> pure APHEV2
      "lc" -> pure APLC
      e ->
        fromTextError $
          "Failure parsing AacProfile from value: '" <> e
            <> "'. Accepted values: hev1, hev2, lc"

instance ToText AacProfile where
  toText = \case
    APHEV1 -> "HEV1"
    APHEV2 -> "HEV2"
    APLC -> "LC"

instance Hashable AacProfile

instance NFData AacProfile

instance ToByteString AacProfile

instance ToQuery AacProfile

instance ToHeader AacProfile

instance ToJSON AacProfile where
  toJSON = toJSONText

instance FromJSON AacProfile where
  parseJSON = parseJSONText "AacProfile"
