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

import qualified Network.AWS.Prelude as Prelude

-- | When enabled the output audio is corrected using the chosen algorithm.
-- If disabled, the audio will be measured but not adjusted.
newtype AudioNormalizationAlgorithmControl = AudioNormalizationAlgorithmControl'
  { fromAudioNormalizationAlgorithmControl ::
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

pattern AudioNormalizationAlgorithmControl_CORRECT_AUDIO :: AudioNormalizationAlgorithmControl
pattern AudioNormalizationAlgorithmControl_CORRECT_AUDIO = AudioNormalizationAlgorithmControl' "CORRECT_AUDIO"

pattern AudioNormalizationAlgorithmControl_MEASURE_ONLY :: AudioNormalizationAlgorithmControl
pattern AudioNormalizationAlgorithmControl_MEASURE_ONLY = AudioNormalizationAlgorithmControl' "MEASURE_ONLY"

{-# COMPLETE
  AudioNormalizationAlgorithmControl_CORRECT_AUDIO,
  AudioNormalizationAlgorithmControl_MEASURE_ONLY,
  AudioNormalizationAlgorithmControl'
  #-}
