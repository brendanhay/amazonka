{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AudioNormalizationAlgorithm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.AudioNormalizationAlgorithm
  ( AudioNormalizationAlgorithm
    ( AudioNormalizationAlgorithm'
    , AudioNormalizationAlgorithmItu17701
    , AudioNormalizationAlgorithmItu17702
    , fromAudioNormalizationAlgorithm
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Audio Normalization Algorithm
newtype AudioNormalizationAlgorithm = AudioNormalizationAlgorithm'{fromAudioNormalizationAlgorithm
                                                                   :: Core.Text}
                                        deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                        Core.Generic)
                                        deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                          Core.ToJSONKey, Core.FromJSONKey,
                                                          Core.ToJSON, Core.FromJSON, Core.ToXML,
                                                          Core.FromXML, Core.ToText, Core.FromText,
                                                          Core.ToByteString, Core.ToQuery,
                                                          Core.ToHeader)

pattern AudioNormalizationAlgorithmItu17701 :: AudioNormalizationAlgorithm
pattern AudioNormalizationAlgorithmItu17701 = AudioNormalizationAlgorithm' "ITU_1770_1"

pattern AudioNormalizationAlgorithmItu17702 :: AudioNormalizationAlgorithm
pattern AudioNormalizationAlgorithmItu17702 = AudioNormalizationAlgorithm' "ITU_1770_2"

{-# COMPLETE 
  AudioNormalizationAlgorithmItu17701,

  AudioNormalizationAlgorithmItu17702,
  AudioNormalizationAlgorithm'
  #-}
