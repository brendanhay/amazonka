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
-- Module      : Amazonka.MediaConvert.Types.H264AdaptiveQuantization
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.H264AdaptiveQuantization
  ( H264AdaptiveQuantization
      ( ..,
        H264AdaptiveQuantization_AUTO,
        H264AdaptiveQuantization_HIGH,
        H264AdaptiveQuantization_HIGHER,
        H264AdaptiveQuantization_LOW,
        H264AdaptiveQuantization_MAX,
        H264AdaptiveQuantization_MEDIUM,
        H264AdaptiveQuantization_OFF
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Keep the default value, Auto (AUTO), for this setting to have
-- MediaConvert automatically apply the best types of quantization for your
-- video content. When you want to apply your quantization settings
-- manually, you must set H264AdaptiveQuantization to a value other than
-- Auto (AUTO). Use this setting to specify the strength of any adaptive
-- quantization filters that you enable. If you don\'t want MediaConvert to
-- do any adaptive quantization in this transcode, set Adaptive
-- quantization (H264AdaptiveQuantization) to Off (OFF). Related settings:
-- The value that you choose here applies to the following settings:
-- H264FlickerAdaptiveQuantization, H264SpatialAdaptiveQuantization, and
-- H264TemporalAdaptiveQuantization.
newtype H264AdaptiveQuantization = H264AdaptiveQuantization'
  { fromH264AdaptiveQuantization ::
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

pattern H264AdaptiveQuantization_AUTO :: H264AdaptiveQuantization
pattern H264AdaptiveQuantization_AUTO = H264AdaptiveQuantization' "AUTO"

pattern H264AdaptiveQuantization_HIGH :: H264AdaptiveQuantization
pattern H264AdaptiveQuantization_HIGH = H264AdaptiveQuantization' "HIGH"

pattern H264AdaptiveQuantization_HIGHER :: H264AdaptiveQuantization
pattern H264AdaptiveQuantization_HIGHER = H264AdaptiveQuantization' "HIGHER"

pattern H264AdaptiveQuantization_LOW :: H264AdaptiveQuantization
pattern H264AdaptiveQuantization_LOW = H264AdaptiveQuantization' "LOW"

pattern H264AdaptiveQuantization_MAX :: H264AdaptiveQuantization
pattern H264AdaptiveQuantization_MAX = H264AdaptiveQuantization' "MAX"

pattern H264AdaptiveQuantization_MEDIUM :: H264AdaptiveQuantization
pattern H264AdaptiveQuantization_MEDIUM = H264AdaptiveQuantization' "MEDIUM"

pattern H264AdaptiveQuantization_OFF :: H264AdaptiveQuantization
pattern H264AdaptiveQuantization_OFF = H264AdaptiveQuantization' "OFF"

{-# COMPLETE
  H264AdaptiveQuantization_AUTO,
  H264AdaptiveQuantization_HIGH,
  H264AdaptiveQuantization_HIGHER,
  H264AdaptiveQuantization_LOW,
  H264AdaptiveQuantization_MAX,
  H264AdaptiveQuantization_MEDIUM,
  H264AdaptiveQuantization_OFF,
  H264AdaptiveQuantization'
  #-}
