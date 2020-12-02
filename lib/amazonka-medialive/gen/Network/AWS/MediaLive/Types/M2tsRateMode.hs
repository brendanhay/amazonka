{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.M2tsRateMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.M2tsRateMode where

import Network.AWS.Prelude

-- | M2ts Rate Mode
data M2tsRateMode
  = MRMCbr
  | MRMVbr
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

instance FromText M2tsRateMode where
  parser =
    takeLowerText >>= \case
      "cbr" -> pure MRMCbr
      "vbr" -> pure MRMVbr
      e ->
        fromTextError $
          "Failure parsing M2tsRateMode from value: '" <> e
            <> "'. Accepted values: cbr, vbr"

instance ToText M2tsRateMode where
  toText = \case
    MRMCbr -> "CBR"
    MRMVbr -> "VBR"

instance Hashable M2tsRateMode

instance NFData M2tsRateMode

instance ToByteString M2tsRateMode

instance ToQuery M2tsRateMode

instance ToHeader M2tsRateMode

instance ToJSON M2tsRateMode where
  toJSON = toJSONText

instance FromJSON M2tsRateMode where
  parseJSON = parseJSONText "M2tsRateMode"
