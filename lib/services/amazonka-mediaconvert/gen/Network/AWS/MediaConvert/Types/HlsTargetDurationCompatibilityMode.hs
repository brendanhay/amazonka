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
-- Module      : Amazonka.MediaConvert.Types.HlsTargetDurationCompatibilityMode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.HlsTargetDurationCompatibilityMode
  ( HlsTargetDurationCompatibilityMode
      ( ..,
        HlsTargetDurationCompatibilityMode_LEGACY,
        HlsTargetDurationCompatibilityMode_SPEC_COMPLIANT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | When set to LEGACY, the segment target duration is always rounded up to
-- the nearest integer value above its current value in seconds. When set
-- to SPEC\\\\_COMPLIANT, the segment target duration is rounded up to the
-- nearest integer value if fraction seconds are greater than or equal to
-- 0.5 (>= 0.5) and rounded down if less than 0.5 (\< 0.5). You may need to
-- use LEGACY if your client needs to ensure that the target duration is
-- always longer than the actual duration of the segment. Some older
-- players may experience interrupted playback when the actual duration of
-- a track in a segment is longer than the target duration.
newtype HlsTargetDurationCompatibilityMode = HlsTargetDurationCompatibilityMode'
  { fromHlsTargetDurationCompatibilityMode ::
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

pattern HlsTargetDurationCompatibilityMode_LEGACY :: HlsTargetDurationCompatibilityMode
pattern HlsTargetDurationCompatibilityMode_LEGACY = HlsTargetDurationCompatibilityMode' "LEGACY"

pattern HlsTargetDurationCompatibilityMode_SPEC_COMPLIANT :: HlsTargetDurationCompatibilityMode
pattern HlsTargetDurationCompatibilityMode_SPEC_COMPLIANT = HlsTargetDurationCompatibilityMode' "SPEC_COMPLIANT"

{-# COMPLETE
  HlsTargetDurationCompatibilityMode_LEGACY,
  HlsTargetDurationCompatibilityMode_SPEC_COMPLIANT,
  HlsTargetDurationCompatibilityMode'
  #-}
