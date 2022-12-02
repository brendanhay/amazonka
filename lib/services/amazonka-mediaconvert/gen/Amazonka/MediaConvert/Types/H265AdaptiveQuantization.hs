{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MediaConvert.Types.H265AdaptiveQuantization
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.H265AdaptiveQuantization
  ( H265AdaptiveQuantization
      ( ..,
        H265AdaptiveQuantization_AUTO,
        H265AdaptiveQuantization_HIGH,
        H265AdaptiveQuantization_HIGHER,
        H265AdaptiveQuantization_LOW,
        H265AdaptiveQuantization_MAX,
        H265AdaptiveQuantization_MEDIUM,
        H265AdaptiveQuantization_OFF
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | When you set Adaptive Quantization (H265AdaptiveQuantization) to Auto
-- (AUTO), or leave blank, MediaConvert automatically applies quantization
-- to improve the video quality of your output. Set Adaptive Quantization
-- to Low (LOW), Medium (MEDIUM), High (HIGH), Higher (HIGHER), or Max
-- (MAX) to manually control the strength of the quantization filter. When
-- you do, you can specify a value for Spatial Adaptive Quantization
-- (H265SpatialAdaptiveQuantization), Temporal Adaptive Quantization
-- (H265TemporalAdaptiveQuantization), and Flicker Adaptive Quantization
-- (H265FlickerAdaptiveQuantization), to further control the quantization
-- filter. Set Adaptive Quantization to Off (OFF) to apply no quantization
-- to your output.
newtype H265AdaptiveQuantization = H265AdaptiveQuantization'
  { fromH265AdaptiveQuantization ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern H265AdaptiveQuantization_AUTO :: H265AdaptiveQuantization
pattern H265AdaptiveQuantization_AUTO = H265AdaptiveQuantization' "AUTO"

pattern H265AdaptiveQuantization_HIGH :: H265AdaptiveQuantization
pattern H265AdaptiveQuantization_HIGH = H265AdaptiveQuantization' "HIGH"

pattern H265AdaptiveQuantization_HIGHER :: H265AdaptiveQuantization
pattern H265AdaptiveQuantization_HIGHER = H265AdaptiveQuantization' "HIGHER"

pattern H265AdaptiveQuantization_LOW :: H265AdaptiveQuantization
pattern H265AdaptiveQuantization_LOW = H265AdaptiveQuantization' "LOW"

pattern H265AdaptiveQuantization_MAX :: H265AdaptiveQuantization
pattern H265AdaptiveQuantization_MAX = H265AdaptiveQuantization' "MAX"

pattern H265AdaptiveQuantization_MEDIUM :: H265AdaptiveQuantization
pattern H265AdaptiveQuantization_MEDIUM = H265AdaptiveQuantization' "MEDIUM"

pattern H265AdaptiveQuantization_OFF :: H265AdaptiveQuantization
pattern H265AdaptiveQuantization_OFF = H265AdaptiveQuantization' "OFF"

{-# COMPLETE
  H265AdaptiveQuantization_AUTO,
  H265AdaptiveQuantization_HIGH,
  H265AdaptiveQuantization_HIGHER,
  H265AdaptiveQuantization_LOW,
  H265AdaptiveQuantization_MAX,
  H265AdaptiveQuantization_MEDIUM,
  H265AdaptiveQuantization_OFF,
  H265AdaptiveQuantization'
  #-}
