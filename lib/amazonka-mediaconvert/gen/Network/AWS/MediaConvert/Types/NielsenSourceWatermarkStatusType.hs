{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.NielsenSourceWatermarkStatusType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.NielsenSourceWatermarkStatusType where

import Network.AWS.Prelude

-- | Required. Specify whether your source content already contains Nielsen non-linear watermarks. When you set this value to Watermarked (WATERMARKED), the service fails the job. Nielsen requires that you add non-linear watermarking to only clean content that doesn't already  have non-linear Nielsen watermarks.
data NielsenSourceWatermarkStatusType
  = Clean
  | Watermarked
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

instance FromText NielsenSourceWatermarkStatusType where
  parser =
    takeLowerText >>= \case
      "clean" -> pure Clean
      "watermarked" -> pure Watermarked
      e ->
        fromTextError $
          "Failure parsing NielsenSourceWatermarkStatusType from value: '" <> e
            <> "'. Accepted values: clean, watermarked"

instance ToText NielsenSourceWatermarkStatusType where
  toText = \case
    Clean -> "CLEAN"
    Watermarked -> "WATERMARKED"

instance Hashable NielsenSourceWatermarkStatusType

instance NFData NielsenSourceWatermarkStatusType

instance ToByteString NielsenSourceWatermarkStatusType

instance ToQuery NielsenSourceWatermarkStatusType

instance ToHeader NielsenSourceWatermarkStatusType

instance ToJSON NielsenSourceWatermarkStatusType where
  toJSON = toJSONText

instance FromJSON NielsenSourceWatermarkStatusType where
  parseJSON = parseJSONText "NielsenSourceWatermarkStatusType"
