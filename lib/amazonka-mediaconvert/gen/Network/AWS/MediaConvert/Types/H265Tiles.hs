{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265Tiles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H265Tiles where

import Network.AWS.Prelude

-- | Enable use of tiles, allowing horizontal as well as vertical subdivision of the encoded pictures.
data H265Tiles
  = HTDisabled
  | HTEnabled
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

instance FromText H265Tiles where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure HTDisabled
      "enabled" -> pure HTEnabled
      e ->
        fromTextError $
          "Failure parsing H265Tiles from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText H265Tiles where
  toText = \case
    HTDisabled -> "DISABLED"
    HTEnabled -> "ENABLED"

instance Hashable H265Tiles

instance NFData H265Tiles

instance ToByteString H265Tiles

instance ToQuery H265Tiles

instance ToHeader H265Tiles

instance ToJSON H265Tiles where
  toJSON = toJSONText

instance FromJSON H265Tiles where
  parseJSON = parseJSONText "H265Tiles"
