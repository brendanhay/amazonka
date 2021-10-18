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
-- Module      : Network.AWS.MediaConvert.Types.Vp8QualityTuningLevel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Vp8QualityTuningLevel
  ( Vp8QualityTuningLevel
      ( ..,
        Vp8QualityTuningLevel_MULTI_PASS,
        Vp8QualityTuningLevel_MULTI_PASS_HQ
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Optional. Use Quality tuning level (qualityTuningLevel) to choose how
-- you want to trade off encoding speed for output video quality. The
-- default behavior is faster, lower quality, multi-pass encoding.
newtype Vp8QualityTuningLevel = Vp8QualityTuningLevel'
  { fromVp8QualityTuningLevel ::
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

pattern Vp8QualityTuningLevel_MULTI_PASS :: Vp8QualityTuningLevel
pattern Vp8QualityTuningLevel_MULTI_PASS = Vp8QualityTuningLevel' "MULTI_PASS"

pattern Vp8QualityTuningLevel_MULTI_PASS_HQ :: Vp8QualityTuningLevel
pattern Vp8QualityTuningLevel_MULTI_PASS_HQ = Vp8QualityTuningLevel' "MULTI_PASS_HQ"

{-# COMPLETE
  Vp8QualityTuningLevel_MULTI_PASS,
  Vp8QualityTuningLevel_MULTI_PASS_HQ,
  Vp8QualityTuningLevel'
  #-}
