{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsStreamInfResolution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsStreamInfResolution where

import Network.AWS.Prelude

-- | Hls Stream Inf Resolution
data HlsStreamInfResolution
  = Exclude
  | Include
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

instance FromText HlsStreamInfResolution where
  parser =
    takeLowerText >>= \case
      "exclude" -> pure Exclude
      "include" -> pure Include
      e ->
        fromTextError $
          "Failure parsing HlsStreamInfResolution from value: '" <> e
            <> "'. Accepted values: exclude, include"

instance ToText HlsStreamInfResolution where
  toText = \case
    Exclude -> "EXCLUDE"
    Include -> "INCLUDE"

instance Hashable HlsStreamInfResolution

instance NFData HlsStreamInfResolution

instance ToByteString HlsStreamInfResolution

instance ToQuery HlsStreamInfResolution

instance ToHeader HlsStreamInfResolution

instance ToJSON HlsStreamInfResolution where
  toJSON = toJSONText

instance FromJSON HlsStreamInfResolution where
  parseJSON = parseJSONText "HlsStreamInfResolution"
