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
-- Module      : Amazonka.MediaConvert.Types.HDRToSDRToneMapper
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.HDRToSDRToneMapper
  ( HDRToSDRToneMapper
      ( ..,
        HDRToSDRToneMapper_PRESERVE_DETAILS,
        HDRToSDRToneMapper_VIBRANT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specify how MediaConvert maps brightness and colors from your HDR input
-- to your SDR output. The mode that you select represents a creative
-- choice, with different tradeoffs in the details and tones of your
-- output. To maintain details in bright or saturated areas of your output:
-- Choose Preserve details. For some sources, your SDR output may look less
-- bright and less saturated when compared to your HDR source. MediaConvert
-- automatically applies this mode for HLG sources, regardless of your
-- choice. For a bright and saturated output: Choose Vibrant. We recommend
-- that you choose this mode when any of your source content is HDR10, and
-- for the best results when it is mastered for 1000 nits. You may notice
-- loss of details in bright or saturated areas of your output. HDR to SDR
-- tone mapping has no effect when your input is SDR.
newtype HDRToSDRToneMapper = HDRToSDRToneMapper'
  { fromHDRToSDRToneMapper ::
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

pattern HDRToSDRToneMapper_PRESERVE_DETAILS :: HDRToSDRToneMapper
pattern HDRToSDRToneMapper_PRESERVE_DETAILS = HDRToSDRToneMapper' "PRESERVE_DETAILS"

pattern HDRToSDRToneMapper_VIBRANT :: HDRToSDRToneMapper
pattern HDRToSDRToneMapper_VIBRANT = HDRToSDRToneMapper' "VIBRANT"

{-# COMPLETE
  HDRToSDRToneMapper_PRESERVE_DETAILS,
  HDRToSDRToneMapper_VIBRANT,
  HDRToSDRToneMapper'
  #-}
