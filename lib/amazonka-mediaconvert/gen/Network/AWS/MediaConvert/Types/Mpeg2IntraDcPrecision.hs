{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mpeg2IntraDcPrecision
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Mpeg2IntraDcPrecision where

import Network.AWS.Prelude

-- | Use Intra DC precision (Mpeg2IntraDcPrecision) to set quantization precision for intra-block DC coefficients. If you choose the value auto, the service will automatically select the precision based on the per-frame compression ratio.
data Mpeg2IntraDcPrecision
  = MIDPAuto
  | MIDPIntraDcPrecision10
  | MIDPIntraDcPrecision11
  | MIDPIntraDcPrecision8
  | MIDPIntraDcPrecision9
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

instance FromText Mpeg2IntraDcPrecision where
  parser =
    takeLowerText >>= \case
      "auto" -> pure MIDPAuto
      "intra_dc_precision_10" -> pure MIDPIntraDcPrecision10
      "intra_dc_precision_11" -> pure MIDPIntraDcPrecision11
      "intra_dc_precision_8" -> pure MIDPIntraDcPrecision8
      "intra_dc_precision_9" -> pure MIDPIntraDcPrecision9
      e ->
        fromTextError $
          "Failure parsing Mpeg2IntraDcPrecision from value: '" <> e
            <> "'. Accepted values: auto, intra_dc_precision_10, intra_dc_precision_11, intra_dc_precision_8, intra_dc_precision_9"

instance ToText Mpeg2IntraDcPrecision where
  toText = \case
    MIDPAuto -> "AUTO"
    MIDPIntraDcPrecision10 -> "INTRA_DC_PRECISION_10"
    MIDPIntraDcPrecision11 -> "INTRA_DC_PRECISION_11"
    MIDPIntraDcPrecision8 -> "INTRA_DC_PRECISION_8"
    MIDPIntraDcPrecision9 -> "INTRA_DC_PRECISION_9"

instance Hashable Mpeg2IntraDcPrecision

instance NFData Mpeg2IntraDcPrecision

instance ToByteString Mpeg2IntraDcPrecision

instance ToQuery Mpeg2IntraDcPrecision

instance ToHeader Mpeg2IntraDcPrecision

instance ToJSON Mpeg2IntraDcPrecision where
  toJSON = toJSONText

instance FromJSON Mpeg2IntraDcPrecision where
  parseJSON = parseJSONText "Mpeg2IntraDcPrecision"
