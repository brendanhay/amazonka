{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Mpeg2DisplayRatio
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Mpeg2DisplayRatio where

import Network.AWS.Prelude

-- | Mpeg2 Display Ratio
data Mpeg2DisplayRatio
  = DISPLAYRATIO16X9
  | DISPLAYRATIO4X3
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

instance FromText Mpeg2DisplayRatio where
  parser =
    takeLowerText >>= \case
      "displayratio16x9" -> pure DISPLAYRATIO16X9
      "displayratio4x3" -> pure DISPLAYRATIO4X3
      e ->
        fromTextError $
          "Failure parsing Mpeg2DisplayRatio from value: '" <> e
            <> "'. Accepted values: displayratio16x9, displayratio4x3"

instance ToText Mpeg2DisplayRatio where
  toText = \case
    DISPLAYRATIO16X9 -> "DISPLAYRATIO16X9"
    DISPLAYRATIO4X3 -> "DISPLAYRATIO4X3"

instance Hashable Mpeg2DisplayRatio

instance NFData Mpeg2DisplayRatio

instance ToByteString Mpeg2DisplayRatio

instance ToQuery Mpeg2DisplayRatio

instance ToHeader Mpeg2DisplayRatio

instance ToJSON Mpeg2DisplayRatio where
  toJSON = toJSONText

instance FromJSON Mpeg2DisplayRatio where
  parseJSON = parseJSONText "Mpeg2DisplayRatio"
