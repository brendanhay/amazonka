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
-- Module      : Amazonka.MediaConvert.Types.Xavc4kProfileQualityTuningLevel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.Xavc4kProfileQualityTuningLevel
  ( Xavc4kProfileQualityTuningLevel
      ( ..,
        Xavc4kProfileQualityTuningLevel_MULTI_PASS_HQ,
        Xavc4kProfileQualityTuningLevel_SINGLE_PASS,
        Xavc4kProfileQualityTuningLevel_SINGLE_PASS_HQ
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Optional. Use Quality tuning level (qualityTuningLevel) to choose how
-- you want to trade off encoding speed for output video quality. The
-- default behavior is faster, lower quality, single-pass encoding.
newtype Xavc4kProfileQualityTuningLevel = Xavc4kProfileQualityTuningLevel'
  { fromXavc4kProfileQualityTuningLevel ::
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

pattern Xavc4kProfileQualityTuningLevel_MULTI_PASS_HQ :: Xavc4kProfileQualityTuningLevel
pattern Xavc4kProfileQualityTuningLevel_MULTI_PASS_HQ = Xavc4kProfileQualityTuningLevel' "MULTI_PASS_HQ"

pattern Xavc4kProfileQualityTuningLevel_SINGLE_PASS :: Xavc4kProfileQualityTuningLevel
pattern Xavc4kProfileQualityTuningLevel_SINGLE_PASS = Xavc4kProfileQualityTuningLevel' "SINGLE_PASS"

pattern Xavc4kProfileQualityTuningLevel_SINGLE_PASS_HQ :: Xavc4kProfileQualityTuningLevel
pattern Xavc4kProfileQualityTuningLevel_SINGLE_PASS_HQ = Xavc4kProfileQualityTuningLevel' "SINGLE_PASS_HQ"

{-# COMPLETE
  Xavc4kProfileQualityTuningLevel_MULTI_PASS_HQ,
  Xavc4kProfileQualityTuningLevel_SINGLE_PASS,
  Xavc4kProfileQualityTuningLevel_SINGLE_PASS_HQ,
  Xavc4kProfileQualityTuningLevel'
  #-}
