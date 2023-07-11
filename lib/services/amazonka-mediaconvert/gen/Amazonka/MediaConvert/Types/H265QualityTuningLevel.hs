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
-- Module      : Amazonka.MediaConvert.Types.H265QualityTuningLevel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.H265QualityTuningLevel
  ( H265QualityTuningLevel
      ( ..,
        H265QualityTuningLevel_MULTI_PASS_HQ,
        H265QualityTuningLevel_SINGLE_PASS,
        H265QualityTuningLevel_SINGLE_PASS_HQ
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Optional. Use Quality tuning level (qualityTuningLevel) to choose how
-- you want to trade off encoding speed for output video quality. The
-- default behavior is faster, lower quality, single-pass encoding.
newtype H265QualityTuningLevel = H265QualityTuningLevel'
  { fromH265QualityTuningLevel ::
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

pattern H265QualityTuningLevel_MULTI_PASS_HQ :: H265QualityTuningLevel
pattern H265QualityTuningLevel_MULTI_PASS_HQ = H265QualityTuningLevel' "MULTI_PASS_HQ"

pattern H265QualityTuningLevel_SINGLE_PASS :: H265QualityTuningLevel
pattern H265QualityTuningLevel_SINGLE_PASS = H265QualityTuningLevel' "SINGLE_PASS"

pattern H265QualityTuningLevel_SINGLE_PASS_HQ :: H265QualityTuningLevel
pattern H265QualityTuningLevel_SINGLE_PASS_HQ = H265QualityTuningLevel' "SINGLE_PASS_HQ"

{-# COMPLETE
  H265QualityTuningLevel_MULTI_PASS_HQ,
  H265QualityTuningLevel_SINGLE_PASS,
  H265QualityTuningLevel_SINGLE_PASS_HQ,
  H265QualityTuningLevel'
  #-}
