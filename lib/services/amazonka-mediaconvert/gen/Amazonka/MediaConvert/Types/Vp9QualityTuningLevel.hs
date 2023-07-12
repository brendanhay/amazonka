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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Optional. Use Quality tuning level (qualityTuningLevel) to choose how
-- you want to trade off encoding speed for output video quality. The
-- default behavior is faster, lower quality, multi-pass encoding.
newtype Vp9QualityTuningLevel = Vp9QualityTuningLevel'
  { fromVp9QualityTuningLevel ::
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

pattern Vp9QualityTuningLevel_MULTI_PASS :: Vp9QualityTuningLevel
pattern Vp9QualityTuningLevel_MULTI_PASS = Vp9QualityTuningLevel' "MULTI_PASS"

pattern Vp9QualityTuningLevel_MULTI_PASS_HQ :: Vp9QualityTuningLevel
pattern Vp9QualityTuningLevel_MULTI_PASS_HQ = Vp9QualityTuningLevel' "MULTI_PASS_HQ"

{-# COMPLETE
  Vp9QualityTuningLevel_MULTI_PASS,
  Vp9QualityTuningLevel_MULTI_PASS_HQ,
  Vp9QualityTuningLevel'
  #-}
