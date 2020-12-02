{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265WriteMp4PackagingType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H265WriteMp4PackagingType where

import Network.AWS.Prelude

-- | If the location of parameter set NAL units doesn't matter in your workflow, ignore this setting. Use this setting only with CMAF or DASH outputs, or with standalone file outputs in an MPEG-4 container (MP4 outputs). Choose HVC1 to mark your output as HVC1. This makes your output compliant with the following specification: ISO IECJTC1 SC29 N13798 Text ISO/IEC FDIS 14496-15 3rd Edition. For these outputs, the service stores parameter set NAL units in the sample headers but not in the samples directly. For MP4 outputs, when you choose HVC1, your output video might not work properly with some downstream systems and video players. The service defaults to marking your output as HEV1. For these outputs, the service writes parameter set NAL units directly into the samples.
data H265WriteMp4PackagingType
  = HEV1
  | HVC1
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

instance FromText H265WriteMp4PackagingType where
  parser =
    takeLowerText >>= \case
      "hev1" -> pure HEV1
      "hvc1" -> pure HVC1
      e ->
        fromTextError $
          "Failure parsing H265WriteMp4PackagingType from value: '" <> e
            <> "'. Accepted values: hev1, hvc1"

instance ToText H265WriteMp4PackagingType where
  toText = \case
    HEV1 -> "HEV1"
    HVC1 -> "HVC1"

instance Hashable H265WriteMp4PackagingType

instance NFData H265WriteMp4PackagingType

instance ToByteString H265WriteMp4PackagingType

instance ToQuery H265WriteMp4PackagingType

instance ToHeader H265WriteMp4PackagingType

instance ToJSON H265WriteMp4PackagingType where
  toJSON = toJSONText

instance FromJSON H265WriteMp4PackagingType where
  parseJSON = parseJSONText "H265WriteMp4PackagingType"
