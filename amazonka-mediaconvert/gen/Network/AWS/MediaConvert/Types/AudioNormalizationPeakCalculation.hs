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
-- Module      : Network.AWS.MediaConvert.Types.AudioNormalizationPeakCalculation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AudioNormalizationPeakCalculation
  ( AudioNormalizationPeakCalculation
      ( ..,
        AudioNormalizationPeakCalculation_NONE,
        AudioNormalizationPeakCalculation_TRUE_PEAK
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | If set to TRUE_PEAK, calculate and log the TruePeak for each output\'s
-- audio track loudness.
newtype AudioNormalizationPeakCalculation = AudioNormalizationPeakCalculation'
  { fromAudioNormalizationPeakCalculation ::
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

pattern AudioNormalizationPeakCalculation_NONE :: AudioNormalizationPeakCalculation
pattern AudioNormalizationPeakCalculation_NONE = AudioNormalizationPeakCalculation' "NONE"

pattern AudioNormalizationPeakCalculation_TRUE_PEAK :: AudioNormalizationPeakCalculation
pattern AudioNormalizationPeakCalculation_TRUE_PEAK = AudioNormalizationPeakCalculation' "TRUE_PEAK"

{-# COMPLETE
  AudioNormalizationPeakCalculation_NONE,
  AudioNormalizationPeakCalculation_TRUE_PEAK,
  AudioNormalizationPeakCalculation'
  #-}
