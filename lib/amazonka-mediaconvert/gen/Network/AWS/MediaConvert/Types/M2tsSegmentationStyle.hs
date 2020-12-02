{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.M2tsSegmentationStyle
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.M2tsSegmentationStyle where

import Network.AWS.Prelude

-- | The segmentation style parameter controls how segmentation markers are inserted into the transport stream. With avails, it is possible that segments may be truncated, which can influence where future segmentation markers are inserted. When a segmentation style of "reset_cadence" is selected and a segment is truncated due to an avail, we will reset the segmentation cadence. This means the subsequent segment will have a duration of of $segmentation_time seconds. When a segmentation style of "maintain_cadence" is selected and a segment is truncated due to an avail, we will not reset the segmentation cadence. This means the subsequent segment will likely be truncated as well. However, all segments after that will have a duration of $segmentation_time seconds. Note that EBP lookahead is a slight exception to this rule.
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
