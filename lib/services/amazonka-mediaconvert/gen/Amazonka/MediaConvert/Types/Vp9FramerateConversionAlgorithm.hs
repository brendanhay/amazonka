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
-- Module      : Amazonka.MediaConvert.Types.Vp9FramerateConversionAlgorithm
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.Vp9FramerateConversionAlgorithm
  ( Vp9FramerateConversionAlgorithm
      ( ..,
        Vp9FramerateConversionAlgorithm_DUPLICATE_DROP,
        Vp9FramerateConversionAlgorithm_FRAMEFORMER,
        Vp9FramerateConversionAlgorithm_INTERPOLATE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Choose the method that you want MediaConvert to use when increasing or
-- decreasing the frame rate. For numerically simple conversions, such as
-- 60 fps to 30 fps: We recommend that you keep the default value, Drop
-- duplicate. For numerically complex conversions, to avoid stutter: Choose
-- Interpolate. This results in a smooth picture, but might introduce
-- undesirable video artifacts. For complex frame rate conversions,
-- especially if your source video has already been converted from its
-- original cadence: Choose FrameFormer to do motion-compensated
-- interpolation. FrameFormer uses the best conversion method frame by
-- frame. Note that using FrameFormer increases the transcoding time and
-- incurs a significant add-on cost. When you choose FrameFormer, your
-- input video resolution must be at least 128x96.
newtype Vp9FramerateConversionAlgorithm = Vp9FramerateConversionAlgorithm'
  { fromVp9FramerateConversionAlgorithm ::
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

pattern Vp9FramerateConversionAlgorithm_DUPLICATE_DROP :: Vp9FramerateConversionAlgorithm
pattern Vp9FramerateConversionAlgorithm_DUPLICATE_DROP = Vp9FramerateConversionAlgorithm' "DUPLICATE_DROP"

pattern Vp9FramerateConversionAlgorithm_FRAMEFORMER :: Vp9FramerateConversionAlgorithm
pattern Vp9FramerateConversionAlgorithm_FRAMEFORMER = Vp9FramerateConversionAlgorithm' "FRAMEFORMER"

pattern Vp9FramerateConversionAlgorithm_INTERPOLATE :: Vp9FramerateConversionAlgorithm
pattern Vp9FramerateConversionAlgorithm_INTERPOLATE = Vp9FramerateConversionAlgorithm' "INTERPOLATE"

{-# COMPLETE
  Vp9FramerateConversionAlgorithm_DUPLICATE_DROP,
  Vp9FramerateConversionAlgorithm_FRAMEFORMER,
  Vp9FramerateConversionAlgorithm_INTERPOLATE,
  Vp9FramerateConversionAlgorithm'
  #-}
