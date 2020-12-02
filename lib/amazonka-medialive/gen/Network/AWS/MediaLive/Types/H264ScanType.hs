{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264ScanType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H264ScanType where

import Network.AWS.Prelude

-- | H264 Scan Type
data H264ScanType
  = HSTInterlaced
  | HSTProgressive
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

instance FromText H264ScanType where
  parser =
    takeLowerText >>= \case
      "interlaced" -> pure HSTInterlaced
      "progressive" -> pure HSTProgressive
      e ->
        fromTextError $
          "Failure parsing H264ScanType from value: '" <> e
            <> "'. Accepted values: interlaced, progressive"

instance ToText H264ScanType where
  toText = \case
    HSTInterlaced -> "INTERLACED"
    HSTProgressive -> "PROGRESSIVE"

instance Hashable H264ScanType

instance NFData H264ScanType

instance ToByteString H264ScanType

instance ToQuery H264ScanType

instance ToHeader H264ScanType

instance ToJSON H264ScanType where
  toJSON = toJSONText

instance FromJSON H264ScanType where
  parseJSON = parseJSONText "H264ScanType"
