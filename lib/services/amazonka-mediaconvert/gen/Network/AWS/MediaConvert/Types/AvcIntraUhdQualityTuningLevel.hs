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
-- Module      : Network.AWS.MediaConvert.Types.AvcIntraUhdQualityTuningLevel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AvcIntraUhdQualityTuningLevel
  ( AvcIntraUhdQualityTuningLevel
      ( ..,
        AvcIntraUhdQualityTuningLevel_MULTI_PASS,
        AvcIntraUhdQualityTuningLevel_SINGLE_PASS
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Optional. Use Quality tuning level (qualityTuningLevel) to choose how
-- many transcoding passes MediaConvert does with your video. When you
-- choose Multi-pass (MULTI_PASS), your video quality is better and your
-- output bitrate is more accurate. That is, the actual bitrate of your
-- output is closer to the target bitrate defined in the specification.
-- When you choose Single-pass (SINGLE_PASS), your encoding time is faster.
-- The default behavior is Single-pass (SINGLE_PASS).
newtype AvcIntraUhdQualityTuningLevel = AvcIntraUhdQualityTuningLevel'
  { fromAvcIntraUhdQualityTuningLevel ::
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

pattern AvcIntraUhdQualityTuningLevel_MULTI_PASS :: AvcIntraUhdQualityTuningLevel
pattern AvcIntraUhdQualityTuningLevel_MULTI_PASS = AvcIntraUhdQualityTuningLevel' "MULTI_PASS"

pattern AvcIntraUhdQualityTuningLevel_SINGLE_PASS :: AvcIntraUhdQualityTuningLevel
pattern AvcIntraUhdQualityTuningLevel_SINGLE_PASS = AvcIntraUhdQualityTuningLevel' "SINGLE_PASS"

{-# COMPLETE
  AvcIntraUhdQualityTuningLevel_MULTI_PASS,
  AvcIntraUhdQualityTuningLevel_SINGLE_PASS,
  AvcIntraUhdQualityTuningLevel'
  #-}
