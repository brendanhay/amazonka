{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Mpeg2ScanType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Mpeg2ScanType where

import Network.AWS.Prelude

-- | Mpeg2 Scan Type
data Mpeg2ScanType
  = MSTInterlaced
  | MSTProgressive
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

instance FromText Mpeg2ScanType where
  parser =
    takeLowerText >>= \case
      "interlaced" -> pure MSTInterlaced
      "progressive" -> pure MSTProgressive
      e ->
        fromTextError $
          "Failure parsing Mpeg2ScanType from value: '" <> e
            <> "'. Accepted values: interlaced, progressive"

instance ToText Mpeg2ScanType where
  toText = \case
    MSTInterlaced -> "INTERLACED"
    MSTProgressive -> "PROGRESSIVE"

instance Hashable Mpeg2ScanType

instance NFData Mpeg2ScanType

instance ToByteString Mpeg2ScanType

instance ToQuery Mpeg2ScanType

instance ToHeader Mpeg2ScanType

instance ToJSON Mpeg2ScanType where
  toJSON = toJSONText

instance FromJSON Mpeg2ScanType where
  parseJSON = parseJSONText "Mpeg2ScanType"
