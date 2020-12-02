{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H265ScanType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H265ScanType where

import Network.AWS.Prelude

-- | H265 Scan Type
data H265ScanType
  = Interlaced
  | Progressive
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

instance FromText H265ScanType where
  parser =
    takeLowerText >>= \case
      "interlaced" -> pure Interlaced
      "progressive" -> pure Progressive
      e ->
        fromTextError $
          "Failure parsing H265ScanType from value: '" <> e
            <> "'. Accepted values: interlaced, progressive"

instance ToText H265ScanType where
  toText = \case
    Interlaced -> "INTERLACED"
    Progressive -> "PROGRESSIVE"

instance Hashable H265ScanType

instance NFData H265ScanType

instance ToByteString H265ScanType

instance ToQuery H265ScanType

instance ToHeader H265ScanType

instance ToJSON H265ScanType where
  toJSON = toJSONText

instance FromJSON H265ScanType where
  parseJSON = parseJSONText "H265ScanType"
