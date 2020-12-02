{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AudioChannelTag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AudioChannelTag where

import Network.AWS.Prelude

-- | You can add a tag for this mono-channel audio track to mimic its placement in a multi-channel layout.  For example, if this track is the left surround channel, choose Left surround (LS).
data AudioChannelTag
  = C
  | CS
  | L
  | LC
  | LS
  | Lfe
  | Lsd
  | R
  | RC
  | RS
  | Rsd
  | Tcs
  | Vhc
  | Vhl
  | Vhr
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

instance FromText AudioChannelTag where
  parser =
    takeLowerText >>= \case
      "c" -> pure C
      "cs" -> pure CS
      "l" -> pure L
      "lc" -> pure LC
      "ls" -> pure LS
      "lfe" -> pure Lfe
      "lsd" -> pure Lsd
      "r" -> pure R
      "rc" -> pure RC
      "rs" -> pure RS
      "rsd" -> pure Rsd
      "tcs" -> pure Tcs
      "vhc" -> pure Vhc
      "vhl" -> pure Vhl
      "vhr" -> pure Vhr
      e ->
        fromTextError $
          "Failure parsing AudioChannelTag from value: '" <> e
            <> "'. Accepted values: c, cs, l, lc, ls, lfe, lsd, r, rc, rs, rsd, tcs, vhc, vhl, vhr"

instance ToText AudioChannelTag where
  toText = \case
    C -> "C"
    CS -> "CS"
    L -> "L"
    LC -> "LC"
    LS -> "LS"
    Lfe -> "LFE"
    Lsd -> "LSD"
    R -> "R"
    RC -> "RC"
    RS -> "RS"
    Rsd -> "RSD"
    Tcs -> "TCS"
    Vhc -> "VHC"
    Vhl -> "VHL"
    Vhr -> "VHR"

instance Hashable AudioChannelTag

instance NFData AudioChannelTag

instance ToByteString AudioChannelTag

instance ToQuery AudioChannelTag

instance ToHeader AudioChannelTag

instance ToJSON AudioChannelTag where
  toJSON = toJSONText

instance FromJSON AudioChannelTag where
  parseJSON = parseJSONText "AudioChannelTag"
