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
-- Module      : Network.AWS.MediaConvert.Types.AvcIntraFramerateConversionAlgorithm
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AvcIntraFramerateConversionAlgorithm
  ( AvcIntraFramerateConversionAlgorithm
      ( ..,
        AvcIntraFramerateConversionAlgorithm_DUPLICATE_DROP,
        AvcIntraFramerateConversionAlgorithm_FRAMEFORMER,
        AvcIntraFramerateConversionAlgorithm_INTERPOLATE
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Choose the method that you want MediaConvert to use when increasing or
-- decreasing the frame rate. We recommend using drop duplicate
-- (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to
-- 30 fps. For numerically complex conversions, you can use interpolate
-- (INTERPOLATE) to avoid stutter. This results in a smooth picture, but
-- might introduce undesirable video artifacts. For complex frame rate
-- conversions, especially if your source video has already been converted
-- from its original cadence, use FrameFormer (FRAMEFORMER) to do
-- motion-compensated interpolation. FrameFormer chooses the best
-- conversion method frame by frame. Note that using FrameFormer increases
-- the transcoding time and incurs a significant add-on cost.
newtype AvcIntraFramerateConversionAlgorithm = AvcIntraFramerateConversionAlgorithm'
  { fromAvcIntraFramerateConversionAlgorithm ::
      Core.Text
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
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern AvcIntraFramerateConversionAlgorithm_DUPLICATE_DROP :: AvcIntraFramerateConversionAlgorithm
pattern AvcIntraFramerateConversionAlgorithm_DUPLICATE_DROP = AvcIntraFramerateConversionAlgorithm' "DUPLICATE_DROP"

pattern AvcIntraFramerateConversionAlgorithm_FRAMEFORMER :: AvcIntraFramerateConversionAlgorithm
pattern AvcIntraFramerateConversionAlgorithm_FRAMEFORMER = AvcIntraFramerateConversionAlgorithm' "FRAMEFORMER"

pattern AvcIntraFramerateConversionAlgorithm_INTERPOLATE :: AvcIntraFramerateConversionAlgorithm
pattern AvcIntraFramerateConversionAlgorithm_INTERPOLATE = AvcIntraFramerateConversionAlgorithm' "INTERPOLATE"

{-# COMPLETE
  AvcIntraFramerateConversionAlgorithm_DUPLICATE_DROP,
  AvcIntraFramerateConversionAlgorithm_FRAMEFORMER,
  AvcIntraFramerateConversionAlgorithm_INTERPOLATE,
  AvcIntraFramerateConversionAlgorithm'
  #-}
