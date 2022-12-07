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
-- Module      : Amazonka.MediaConvert.Types.AudioNormalizationPeakCalculation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.AudioNormalizationPeakCalculation
  ( AudioNormalizationPeakCalculation
      ( ..,
        AudioNormalizationPeakCalculation_NONE,
        AudioNormalizationPeakCalculation_TRUE_PEAK
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | If set to TRUE_PEAK, calculate and log the TruePeak for each output\'s
-- audio track loudness.
newtype AudioNormalizationPeakCalculation = AudioNormalizationPeakCalculation'
  { fromAudioNormalizationPeakCalculation ::
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

pattern AudioNormalizationPeakCalculation_NONE :: AudioNormalizationPeakCalculation
pattern AudioNormalizationPeakCalculation_NONE = AudioNormalizationPeakCalculation' "NONE"

pattern AudioNormalizationPeakCalculation_TRUE_PEAK :: AudioNormalizationPeakCalculation
pattern AudioNormalizationPeakCalculation_TRUE_PEAK = AudioNormalizationPeakCalculation' "TRUE_PEAK"

{-# COMPLETE
  AudioNormalizationPeakCalculation_NONE,
  AudioNormalizationPeakCalculation_TRUE_PEAK,
  AudioNormalizationPeakCalculation'
  #-}
