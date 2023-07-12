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
-- Module      : Amazonka.MediaConvert.Types.H264FlickerAdaptiveQuantization
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.H264FlickerAdaptiveQuantization
  ( H264FlickerAdaptiveQuantization
      ( ..,
        H264FlickerAdaptiveQuantization_DISABLED,
        H264FlickerAdaptiveQuantization_ENABLED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Only use this setting when you change the default value, AUTO, for the
-- setting H264AdaptiveQuantization. When you keep all defaults, excluding
-- H264AdaptiveQuantization and all other adaptive quantization from your
-- JSON job specification, MediaConvert automatically applies the best
-- types of quantization for your video content. When you set
-- H264AdaptiveQuantization to a value other than AUTO, the default value
-- for H264FlickerAdaptiveQuantization is Disabled (DISABLED). Change this
-- value to Enabled (ENABLED) to reduce I-frame pop. I-frame pop appears as
-- a visual flicker that can arise when the encoder saves bits by copying
-- some macroblocks many times from frame to frame, and then refreshes them
-- at the I-frame. When you enable this setting, the encoder updates these
-- macroblocks slightly more often to smooth out the flicker. To manually
-- enable or disable H264FlickerAdaptiveQuantization, you must set Adaptive
-- quantization (H264AdaptiveQuantization) to a value other than AUTO.
newtype H264FlickerAdaptiveQuantization = H264FlickerAdaptiveQuantization'
  { fromH264FlickerAdaptiveQuantization ::
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

pattern H264FlickerAdaptiveQuantization_DISABLED :: H264FlickerAdaptiveQuantization
pattern H264FlickerAdaptiveQuantization_DISABLED = H264FlickerAdaptiveQuantization' "DISABLED"

pattern H264FlickerAdaptiveQuantization_ENABLED :: H264FlickerAdaptiveQuantization
pattern H264FlickerAdaptiveQuantization_ENABLED = H264FlickerAdaptiveQuantization' "ENABLED"

{-# COMPLETE
  H264FlickerAdaptiveQuantization_DISABLED,
  H264FlickerAdaptiveQuantization_ENABLED,
  H264FlickerAdaptiveQuantization'
  #-}
