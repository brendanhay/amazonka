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
-- Module      : Amazonka.MediaConvert.Types.AudioNormalizationAlgorithmControl
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.AudioNormalizationAlgorithmControl
  ( AudioNormalizationAlgorithmControl
      ( ..,
        AudioNormalizationAlgorithmControl_CORRECT_AUDIO,
        AudioNormalizationAlgorithmControl_MEASURE_ONLY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | When enabled the output audio is corrected using the chosen algorithm.
-- If disabled, the audio will be measured but not adjusted.
newtype AudioNormalizationAlgorithmControl = AudioNormalizationAlgorithmControl'
  { fromAudioNormalizationAlgorithmControl ::
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

pattern AudioNormalizationAlgorithmControl_CORRECT_AUDIO :: AudioNormalizationAlgorithmControl
pattern AudioNormalizationAlgorithmControl_CORRECT_AUDIO = AudioNormalizationAlgorithmControl' "CORRECT_AUDIO"

pattern AudioNormalizationAlgorithmControl_MEASURE_ONLY :: AudioNormalizationAlgorithmControl
pattern AudioNormalizationAlgorithmControl_MEASURE_ONLY = AudioNormalizationAlgorithmControl' "MEASURE_ONLY"

{-# COMPLETE
  AudioNormalizationAlgorithmControl_CORRECT_AUDIO,
  AudioNormalizationAlgorithmControl_MEASURE_ONLY,
  AudioNormalizationAlgorithmControl'
  #-}
