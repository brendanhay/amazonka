{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ProresCodecProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.ProresCodecProfile where

import Network.AWS.Prelude

-- | Use Profile (ProResCodecProfile) to specifiy the type of Apple ProRes codec to use for this output.
data ProresCodecProfile
  = AppleProres422
  | AppleProres422Hq
  | AppleProres422LT
  | AppleProres422Proxy
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

instance FromText ProresCodecProfile where
  parser =
    takeLowerText >>= \case
      "apple_prores_422" -> pure AppleProres422
      "apple_prores_422_hq" -> pure AppleProres422Hq
      "apple_prores_422_lt" -> pure AppleProres422LT
      "apple_prores_422_proxy" -> pure AppleProres422Proxy
      e ->
        fromTextError $
          "Failure parsing ProresCodecProfile from value: '" <> e
            <> "'. Accepted values: apple_prores_422, apple_prores_422_hq, apple_prores_422_lt, apple_prores_422_proxy"

instance ToText ProresCodecProfile where
  toText = \case
    AppleProres422 -> "APPLE_PRORES_422"
    AppleProres422Hq -> "APPLE_PRORES_422_HQ"
    AppleProres422LT -> "APPLE_PRORES_422_LT"
    AppleProres422Proxy -> "APPLE_PRORES_422_PROXY"

instance Hashable ProresCodecProfile

instance NFData ProresCodecProfile

instance ToByteString ProresCodecProfile

instance ToQuery ProresCodecProfile

instance ToHeader ProresCodecProfile

instance ToJSON ProresCodecProfile where
  toJSON = toJSONText

instance FromJSON ProresCodecProfile where
  parseJSON = parseJSONText "ProresCodecProfile"
