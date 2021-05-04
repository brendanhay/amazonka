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
-- Module      : Network.AWS.MediaConvert.Types.H264QualityTuningLevel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H264QualityTuningLevel
  ( H264QualityTuningLevel
      ( ..,
        H264QualityTuningLevel_MULTI_PASS_HQ,
        H264QualityTuningLevel_SINGLE_PASS,
        H264QualityTuningLevel_SINGLE_PASS_HQ
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Optional. Use Quality tuning level (qualityTuningLevel) to choose how
-- you want to trade off encoding speed for output video quality. The
-- default behavior is faster, lower quality, single-pass encoding.
newtype H264QualityTuningLevel = H264QualityTuningLevel'
  { fromH264QualityTuningLevel ::
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
