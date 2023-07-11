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
-- Module      : Amazonka.MediaConvert.Types.AudioNormalizationLoudnessLogging
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.AudioNormalizationLoudnessLogging
  ( AudioNormalizationLoudnessLogging
      ( ..,
        AudioNormalizationLoudnessLogging_DONT_LOG,
        AudioNormalizationLoudnessLogging_LOG
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | If set to LOG, log each output\'s audio track loudness to a CSV file.
newtype AudioNormalizationLoudnessLogging = AudioNormalizationLoudnessLogging'
  { fromAudioNormalizationLoudnessLogging ::
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

pattern AudioNormalizationLoudnessLogging_DONT_LOG :: AudioNormalizationLoudnessLogging
pattern AudioNormalizationLoudnessLogging_DONT_LOG = AudioNormalizationLoudnessLogging' "DONT_LOG"

pattern AudioNormalizationLoudnessLogging_LOG :: AudioNormalizationLoudnessLogging
pattern AudioNormalizationLoudnessLogging_LOG = AudioNormalizationLoudnessLogging' "LOG"

{-# COMPLETE
  AudioNormalizationLoudnessLogging_DONT_LOG,
  AudioNormalizationLoudnessLogging_LOG,
  AudioNormalizationLoudnessLogging'
  #-}
