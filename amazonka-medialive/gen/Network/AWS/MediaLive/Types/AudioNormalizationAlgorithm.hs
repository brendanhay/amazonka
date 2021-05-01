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

import qualified Network.AWS.Prelude as Prelude

-- | Audio Normalization Algorithm
newtype AudioNormalizationAlgorithm = AudioNormalizationAlgorithm'
  { fromAudioNormalizationAlgorithm ::
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

pattern AudioNormalizationAlgorithm_ITU_1770_1 :: AudioNormalizationAlgorithm
pattern AudioNormalizationAlgorithm_ITU_1770_1 = AudioNormalizationAlgorithm' "ITU_1770_1"

pattern AudioNormalizationAlgorithm_ITU_1770_2 :: AudioNormalizationAlgorithm
pattern AudioNormalizationAlgorithm_ITU_1770_2 = AudioNormalizationAlgorithm' "ITU_1770_2"

{-# COMPLETE
  AudioNormalizationAlgorithm_ITU_1770_1,
  AudioNormalizationAlgorithm_ITU_1770_2,
  AudioNormalizationAlgorithm'
  #-}
