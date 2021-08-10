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
-- Module      : Network.AWS.MediaConvert.Types.AudioNormalizationLoudnessLogging
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AudioNormalizationLoudnessLogging
  ( AudioNormalizationLoudnessLogging
      ( ..,
        AudioNormalizationLoudnessLogging_DONT_LOG,
        AudioNormalizationLoudnessLogging_LOG
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | If set to LOG, log each output\'s audio track loudness to a CSV file.
newtype AudioNormalizationLoudnessLogging = AudioNormalizationLoudnessLogging'
  { fromAudioNormalizationLoudnessLogging ::
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

pattern AudioNormalizationLoudnessLogging_DONT_LOG :: AudioNormalizationLoudnessLogging
pattern AudioNormalizationLoudnessLogging_DONT_LOG = AudioNormalizationLoudnessLogging' "DONT_LOG"

pattern AudioNormalizationLoudnessLogging_LOG :: AudioNormalizationLoudnessLogging
pattern AudioNormalizationLoudnessLogging_LOG = AudioNormalizationLoudnessLogging' "LOG"

{-# COMPLETE
  AudioNormalizationLoudnessLogging_DONT_LOG,
  AudioNormalizationLoudnessLogging_LOG,
  AudioNormalizationLoudnessLogging'
  #-}
