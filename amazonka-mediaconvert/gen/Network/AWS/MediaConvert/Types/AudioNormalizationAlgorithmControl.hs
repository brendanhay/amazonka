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
-- Module      : Network.AWS.MediaConvert.Types.AudioNormalizationAlgorithmControl
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AudioNormalizationAlgorithmControl
  ( AudioNormalizationAlgorithmControl
      ( ..,
        AudioNormalizationAlgorithmControl_CORRECT_AUDIO,
        AudioNormalizationAlgorithmControl_MEASURE_ONLY
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | When enabled the output audio is corrected using the chosen algorithm.
-- If disabled, the audio will be measured but not adjusted.
newtype AudioNormalizationAlgorithmControl = AudioNormalizationAlgorithmControl'
  { fromAudioNormalizationAlgorithmControl ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern AudioNormalizationAlgorithmControl_CORRECT_AUDIO :: AudioNormalizationAlgorithmControl
pattern AudioNormalizationAlgorithmControl_CORRECT_AUDIO = AudioNormalizationAlgorithmControl' "CORRECT_AUDIO"

pattern AudioNormalizationAlgorithmControl_MEASURE_ONLY :: AudioNormalizationAlgorithmControl
pattern AudioNormalizationAlgorithmControl_MEASURE_ONLY = AudioNormalizationAlgorithmControl' "MEASURE_ONLY"

{-# COMPLETE
  AudioNormalizationAlgorithmControl_CORRECT_AUDIO,
  AudioNormalizationAlgorithmControl_MEASURE_ONLY,
  AudioNormalizationAlgorithmControl'
  #-}
