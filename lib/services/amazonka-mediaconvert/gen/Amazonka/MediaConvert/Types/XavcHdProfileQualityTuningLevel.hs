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
-- Module      : Amazonka.MediaConvert.Types.XavcHdProfileQualityTuningLevel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.XavcHdProfileQualityTuningLevel
  ( XavcHdProfileQualityTuningLevel
      ( ..,
        XavcHdProfileQualityTuningLevel_MULTI_PASS_HQ,
        XavcHdProfileQualityTuningLevel_SINGLE_PASS,
        XavcHdProfileQualityTuningLevel_SINGLE_PASS_HQ
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Optional. Use Quality tuning level (qualityTuningLevel) to choose how
-- you want to trade off encoding speed for output video quality. The
-- default behavior is faster, lower quality, single-pass encoding.
newtype XavcHdProfileQualityTuningLevel = XavcHdProfileQualityTuningLevel'
  { fromXavcHdProfileQualityTuningLevel ::
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

pattern XavcHdProfileQualityTuningLevel_MULTI_PASS_HQ :: XavcHdProfileQualityTuningLevel
pattern XavcHdProfileQualityTuningLevel_MULTI_PASS_HQ = XavcHdProfileQualityTuningLevel' "MULTI_PASS_HQ"

pattern XavcHdProfileQualityTuningLevel_SINGLE_PASS :: XavcHdProfileQualityTuningLevel
pattern XavcHdProfileQualityTuningLevel_SINGLE_PASS = XavcHdProfileQualityTuningLevel' "SINGLE_PASS"

pattern XavcHdProfileQualityTuningLevel_SINGLE_PASS_HQ :: XavcHdProfileQualityTuningLevel
pattern XavcHdProfileQualityTuningLevel_SINGLE_PASS_HQ = XavcHdProfileQualityTuningLevel' "SINGLE_PASS_HQ"

{-# COMPLETE
  XavcHdProfileQualityTuningLevel_MULTI_PASS_HQ,
  XavcHdProfileQualityTuningLevel_SINGLE_PASS,
  XavcHdProfileQualityTuningLevel_SINGLE_PASS_HQ,
  XavcHdProfileQualityTuningLevel'
  #-}
