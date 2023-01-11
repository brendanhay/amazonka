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
-- Module      : Amazonka.MediaConvert.Types.H264QualityTuningLevel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.H264QualityTuningLevel
  ( H264QualityTuningLevel
      ( ..,
        H264QualityTuningLevel_MULTI_PASS_HQ,
        H264QualityTuningLevel_SINGLE_PASS,
        H264QualityTuningLevel_SINGLE_PASS_HQ
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Optional. Use Quality tuning level (qualityTuningLevel) to choose how
-- you want to trade off encoding speed for output video quality. The
-- default behavior is faster, lower quality, single-pass encoding.
newtype H264QualityTuningLevel = H264QualityTuningLevel'
  { fromH264QualityTuningLevel ::
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

pattern H264QualityTuningLevel_MULTI_PASS_HQ :: H264QualityTuningLevel
pattern H264QualityTuningLevel_MULTI_PASS_HQ = H264QualityTuningLevel' "MULTI_PASS_HQ"

pattern H264QualityTuningLevel_SINGLE_PASS :: H264QualityTuningLevel
pattern H264QualityTuningLevel_SINGLE_PASS = H264QualityTuningLevel' "SINGLE_PASS"

pattern H264QualityTuningLevel_SINGLE_PASS_HQ :: H264QualityTuningLevel
pattern H264QualityTuningLevel_SINGLE_PASS_HQ = H264QualityTuningLevel' "SINGLE_PASS_HQ"

{-# COMPLETE
  H264QualityTuningLevel_MULTI_PASS_HQ,
  H264QualityTuningLevel_SINGLE_PASS,
  H264QualityTuningLevel_SINGLE_PASS_HQ,
  H264QualityTuningLevel'
  #-}
