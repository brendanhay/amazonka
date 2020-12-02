{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264SpatialAq
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H264SpatialAq where

import Network.AWS.Prelude

-- | H264 Spatial Aq
data H264SpatialAq
  = HSADisabled
  | HSAEnabled
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

instance FromText H264SpatialAq where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure HSADisabled
      "enabled" -> pure HSAEnabled
      e ->
        fromTextError $
          "Failure parsing H264SpatialAq from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText H264SpatialAq where
  toText = \case
    HSADisabled -> "DISABLED"
    HSAEnabled -> "ENABLED"

instance Hashable H264SpatialAq

instance NFData H264SpatialAq

instance ToByteString H264SpatialAq

instance ToQuery H264SpatialAq

instance ToHeader H264SpatialAq

instance ToJSON H264SpatialAq where
  toJSON = toJSONText

instance FromJSON H264SpatialAq where
  parseJSON = parseJSONText "H264SpatialAq"
