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
-- Module      : Network.AWS.MediaConvert.Types.H265QualityTuningLevel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H265QualityTuningLevel
  ( H265QualityTuningLevel
      ( ..,
        H265QualityTuningLevel_MULTI_PASS_HQ,
        H265QualityTuningLevel_SINGLE_PASS,
        H265QualityTuningLevel_SINGLE_PASS_HQ
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Optional. Use Quality tuning level (qualityTuningLevel) to choose how
-- you want to trade off encoding speed for output video quality. The
-- default behavior is faster, lower quality, single-pass encoding.
newtype H265QualityTuningLevel = H265QualityTuningLevel'
  { fromH265QualityTuningLevel ::
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
