{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

-- | If set to TRUE_PEAK, calculate and log the TruePeak for each output\'s
-- audio track loudness.
newtype AudioNormalizationPeakCalculation = AudioNormalizationPeakCalculation'
  { fromAudioNormalizationPeakCalculation ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
