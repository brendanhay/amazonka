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
-- Module      : Amazonka.MediaConvert.Types.AudioNormalizationAlgorithm
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.AudioNormalizationAlgorithm
  ( AudioNormalizationAlgorithm
      ( ..,
        AudioNormalizationAlgorithm_ITU_BS_1770_1,
        AudioNormalizationAlgorithm_ITU_BS_1770_2,
        AudioNormalizationAlgorithm_ITU_BS_1770_3,
        AudioNormalizationAlgorithm_ITU_BS_1770_4
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Choose one of the following audio normalization algorithms: ITU-R
-- BS.1770-1: Ungated loudness. A measurement of ungated average loudness
-- for an entire piece of content, suitable for measurement of short-form
-- content under ATSC recommendation A\/85. Supports up to 5.1 audio
-- channels. ITU-R BS.1770-2: Gated loudness. A measurement of gated
-- average loudness compliant with the requirements of EBU-R128. Supports
-- up to 5.1 audio channels. ITU-R BS.1770-3: Modified peak. The same
-- loudness measurement algorithm as 1770-2, with an updated true peak
-- measurement. ITU-R BS.1770-4: Higher channel count. Allows for more
-- audio channels than the other algorithms, including configurations such
-- as 7.1.
newtype AudioNormalizationAlgorithm = AudioNormalizationAlgorithm'
  { fromAudioNormalizationAlgorithm ::
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

pattern AudioNormalizationAlgorithm_ITU_BS_1770_1 :: AudioNormalizationAlgorithm
pattern AudioNormalizationAlgorithm_ITU_BS_1770_1 = AudioNormalizationAlgorithm' "ITU_BS_1770_1"

pattern AudioNormalizationAlgorithm_ITU_BS_1770_2 :: AudioNormalizationAlgorithm
pattern AudioNormalizationAlgorithm_ITU_BS_1770_2 = AudioNormalizationAlgorithm' "ITU_BS_1770_2"

pattern AudioNormalizationAlgorithm_ITU_BS_1770_3 :: AudioNormalizationAlgorithm
pattern AudioNormalizationAlgorithm_ITU_BS_1770_3 = AudioNormalizationAlgorithm' "ITU_BS_1770_3"

pattern AudioNormalizationAlgorithm_ITU_BS_1770_4 :: AudioNormalizationAlgorithm
pattern AudioNormalizationAlgorithm_ITU_BS_1770_4 = AudioNormalizationAlgorithm' "ITU_BS_1770_4"

{-# COMPLETE
  AudioNormalizationAlgorithm_ITU_BS_1770_1,
  AudioNormalizationAlgorithm_ITU_BS_1770_2,
  AudioNormalizationAlgorithm_ITU_BS_1770_3,
  AudioNormalizationAlgorithm_ITU_BS_1770_4,
  AudioNormalizationAlgorithm'
  #-}
