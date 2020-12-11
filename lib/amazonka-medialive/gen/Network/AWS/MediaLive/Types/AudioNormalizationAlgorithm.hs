-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AudioNormalizationAlgorithm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AudioNormalizationAlgorithm
  ( AudioNormalizationAlgorithm
      ( AudioNormalizationAlgorithm',
        Itu17701,
        Itu17702
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Audio Normalization Algorithm
newtype AudioNormalizationAlgorithm = AudioNormalizationAlgorithm' Lude.Text
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

pattern Itu17701 :: AudioNormalizationAlgorithm
pattern Itu17701 = AudioNormalizationAlgorithm' "ITU_1770_1"

pattern Itu17702 :: AudioNormalizationAlgorithm
pattern Itu17702 = AudioNormalizationAlgorithm' "ITU_1770_2"

{-# COMPLETE
  Itu17701,
  Itu17702,
  AudioNormalizationAlgorithm'
  #-}
