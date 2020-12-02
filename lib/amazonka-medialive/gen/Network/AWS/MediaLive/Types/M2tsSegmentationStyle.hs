{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.M2tsSegmentationStyle
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.M2tsSegmentationStyle where

import Network.AWS.Prelude

-- | M2ts Segmentation Style
data M2tsSegmentationStyle
  = MaintainCadence
  | ResetCadence
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

instance FromText M2tsSegmentationStyle where
  parser =
    takeLowerText >>= \case
      "maintain_cadence" -> pure MaintainCadence
      "reset_cadence" -> pure ResetCadence
      e ->
        fromTextError $
          "Failure parsing M2tsSegmentationStyle from value: '" <> e
            <> "'. Accepted values: maintain_cadence, reset_cadence"

instance ToText M2tsSegmentationStyle where
  toText = \case
    MaintainCadence -> "MAINTAIN_CADENCE"
    ResetCadence -> "RESET_CADENCE"

instance Hashable M2tsSegmentationStyle

instance NFData M2tsSegmentationStyle

instance ToByteString M2tsSegmentationStyle

instance ToQuery M2tsSegmentationStyle

instance ToHeader M2tsSegmentationStyle

instance ToJSON M2tsSegmentationStyle where
  toJSON = toJSONText

instance FromJSON M2tsSegmentationStyle where
  parseJSON = parseJSONText "M2tsSegmentationStyle"
