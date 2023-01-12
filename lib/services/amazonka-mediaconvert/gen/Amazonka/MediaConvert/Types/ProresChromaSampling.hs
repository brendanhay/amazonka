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
-- Module      : Amazonka.MediaConvert.Types.ProresChromaSampling
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.ProresChromaSampling
  ( ProresChromaSampling
      ( ..,
        ProresChromaSampling_PRESERVE_444_SAMPLING,
        ProresChromaSampling_SUBSAMPLE_TO_422
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | This setting applies only to ProRes 4444 and ProRes 4444 XQ outputs that
-- you create from inputs that use 4:4:4 chroma sampling. Set Preserve
-- 4:4:4 sampling (PRESERVE_444_SAMPLING) to allow outputs to also use
-- 4:4:4 chroma sampling. You must specify a value for this setting when
-- your output codec profile supports 4:4:4 chroma sampling. Related
-- Settings: When you set Chroma sampling to Preserve 4:4:4 sampling
-- (PRESERVE_444_SAMPLING), you must choose an output codec profile that
-- supports 4:4:4 chroma sampling. These values for Profile (CodecProfile)
-- support 4:4:4 chroma sampling: Apple ProRes 4444 (APPLE_PRORES_4444) or
-- Apple ProRes 4444 XQ (APPLE_PRORES_4444_XQ). When you set Chroma
-- sampling to Preserve 4:4:4 sampling, you must disable all video
-- preprocessors except for Nexguard file marker (PartnerWatermarking).
-- When you set Chroma sampling to Preserve 4:4:4 sampling and use
-- framerate conversion, you must set Frame rate conversion algorithm
-- (FramerateConversionAlgorithm) to Drop duplicate (DUPLICATE_DROP).
newtype ProresChromaSampling = ProresChromaSampling'
  { fromProresChromaSampling ::
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

pattern ProresChromaSampling_PRESERVE_444_SAMPLING :: ProresChromaSampling
pattern ProresChromaSampling_PRESERVE_444_SAMPLING = ProresChromaSampling' "PRESERVE_444_SAMPLING"

pattern ProresChromaSampling_SUBSAMPLE_TO_422 :: ProresChromaSampling
pattern ProresChromaSampling_SUBSAMPLE_TO_422 = ProresChromaSampling' "SUBSAMPLE_TO_422"

{-# COMPLETE
  ProresChromaSampling_PRESERVE_444_SAMPLING,
  ProresChromaSampling_SUBSAMPLE_TO_422,
  ProresChromaSampling'
  #-}
