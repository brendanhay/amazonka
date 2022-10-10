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
-- Module      : Amazonka.MediaConvert.Types.Vp9QualityTuningLevel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.Vp9QualityTuningLevel
  ( Vp9QualityTuningLevel
      ( ..,
        Vp9QualityTuningLevel_MULTI_PASS,
        Vp9QualityTuningLevel_MULTI_PASS_HQ
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | Optional. Use Quality tuning level (qualityTuningLevel) to choose how
-- you want to trade off encoding speed for output video quality. The
-- default behavior is faster, lower quality, multi-pass encoding.
newtype Vp9QualityTuningLevel = Vp9QualityTuningLevel'
  { fromVp9QualityTuningLevel ::
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

pattern Vp9QualityTuningLevel_MULTI_PASS :: Vp9QualityTuningLevel
pattern Vp9QualityTuningLevel_MULTI_PASS = Vp9QualityTuningLevel' "MULTI_PASS"

pattern Vp9QualityTuningLevel_MULTI_PASS_HQ :: Vp9QualityTuningLevel
pattern Vp9QualityTuningLevel_MULTI_PASS_HQ = Vp9QualityTuningLevel' "MULTI_PASS_HQ"

{-# COMPLETE
  Vp9QualityTuningLevel_MULTI_PASS,
  Vp9QualityTuningLevel_MULTI_PASS_HQ,
  Vp9QualityTuningLevel'
  #-}
