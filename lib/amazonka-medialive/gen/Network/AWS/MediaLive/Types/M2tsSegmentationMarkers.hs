{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.M2tsSegmentationMarkers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.M2tsSegmentationMarkers where

import Network.AWS.Prelude

-- | M2ts Segmentation Markers
data M2tsSegmentationMarkers
  = MSMEbp
  | MSMEbpLegacy
  | MSMNone
  | MSMPsiSegstart
  | MSMRaiAdapt
  | MSMRaiSegstart
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

instance FromText M2tsSegmentationMarkers where
  parser =
    takeLowerText >>= \case
      "ebp" -> pure MSMEbp
      "ebp_legacy" -> pure MSMEbpLegacy
      "none" -> pure MSMNone
      "psi_segstart" -> pure MSMPsiSegstart
      "rai_adapt" -> pure MSMRaiAdapt
      "rai_segstart" -> pure MSMRaiSegstart
      e ->
        fromTextError $
          "Failure parsing M2tsSegmentationMarkers from value: '" <> e
            <> "'. Accepted values: ebp, ebp_legacy, none, psi_segstart, rai_adapt, rai_segstart"

instance ToText M2tsSegmentationMarkers where
  toText = \case
    MSMEbp -> "EBP"
    MSMEbpLegacy -> "EBP_LEGACY"
    MSMNone -> "NONE"
    MSMPsiSegstart -> "PSI_SEGSTART"
    MSMRaiAdapt -> "RAI_ADAPT"
    MSMRaiSegstart -> "RAI_SEGSTART"

instance Hashable M2tsSegmentationMarkers

instance NFData M2tsSegmentationMarkers

instance ToByteString M2tsSegmentationMarkers

instance ToQuery M2tsSegmentationMarkers

instance ToHeader M2tsSegmentationMarkers

instance ToJSON M2tsSegmentationMarkers where
  toJSON = toJSONText

instance FromJSON M2tsSegmentationMarkers where
  parseJSON = parseJSONText "M2tsSegmentationMarkers"
