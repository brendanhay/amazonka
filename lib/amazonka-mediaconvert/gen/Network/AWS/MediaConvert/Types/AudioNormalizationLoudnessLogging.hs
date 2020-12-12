{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AudioNormalizationLoudnessLogging
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AudioNormalizationLoudnessLogging
  ( AudioNormalizationLoudnessLogging
      ( AudioNormalizationLoudnessLogging',
        DontLog,
        Log
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | If set to LOG, log each output's audio track loudness to a CSV file.
newtype AudioNormalizationLoudnessLogging = AudioNormalizationLoudnessLogging' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern DontLog :: AudioNormalizationLoudnessLogging
pattern DontLog = AudioNormalizationLoudnessLogging' "DONT_LOG"

pattern Log :: AudioNormalizationLoudnessLogging
pattern Log = AudioNormalizationLoudnessLogging' "LOG"

{-# COMPLETE
  DontLog,
  Log,
  AudioNormalizationLoudnessLogging'
  #-}
