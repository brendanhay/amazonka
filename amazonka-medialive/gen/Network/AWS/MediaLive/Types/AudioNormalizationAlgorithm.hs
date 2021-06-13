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
-- Module      : Network.AWS.MediaLive.Types.AudioNormalizationAlgorithm
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AudioNormalizationAlgorithm
  ( AudioNormalizationAlgorithm
      ( ..,
        AudioNormalizationAlgorithm_ITU_1770_1,
        AudioNormalizationAlgorithm_ITU_1770_2
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Audio Normalization Algorithm
newtype AudioNormalizationAlgorithm = AudioNormalizationAlgorithm'
  { fromAudioNormalizationAlgorithm ::
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

pattern AudioNormalizationAlgorithm_ITU_1770_1 :: AudioNormalizationAlgorithm
pattern AudioNormalizationAlgorithm_ITU_1770_1 = AudioNormalizationAlgorithm' "ITU_1770_1"

pattern AudioNormalizationAlgorithm_ITU_1770_2 :: AudioNormalizationAlgorithm
pattern AudioNormalizationAlgorithm_ITU_1770_2 = AudioNormalizationAlgorithm' "ITU_1770_2"

{-# COMPLETE
  AudioNormalizationAlgorithm_ITU_1770_1,
  AudioNormalizationAlgorithm_ITU_1770_2,
  AudioNormalizationAlgorithm'
  #-}
