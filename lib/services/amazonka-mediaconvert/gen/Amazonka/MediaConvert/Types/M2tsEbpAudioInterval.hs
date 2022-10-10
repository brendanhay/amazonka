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
-- Module      : Amazonka.MediaConvert.Types.M2tsEbpAudioInterval
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.M2tsEbpAudioInterval
  ( M2tsEbpAudioInterval
      ( ..,
        M2tsEbpAudioInterval_VIDEO_AND_FIXED_INTERVALS,
        M2tsEbpAudioInterval_VIDEO_INTERVAL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | When set to VIDEO_AND_FIXED_INTERVALS, audio EBP markers will be added
-- to partitions 3 and 4. The interval between these additional markers
-- will be fixed, and will be slightly shorter than the video EBP marker
-- interval. When set to VIDEO_INTERVAL, these additional markers will not
-- be inserted. Only applicable when EBP segmentation markers are is
-- selected (segmentationMarkers is EBP or EBP_LEGACY).
newtype M2tsEbpAudioInterval = M2tsEbpAudioInterval'
  { fromM2tsEbpAudioInterval ::
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

pattern M2tsEbpAudioInterval_VIDEO_AND_FIXED_INTERVALS :: M2tsEbpAudioInterval
pattern M2tsEbpAudioInterval_VIDEO_AND_FIXED_INTERVALS = M2tsEbpAudioInterval' "VIDEO_AND_FIXED_INTERVALS"

pattern M2tsEbpAudioInterval_VIDEO_INTERVAL :: M2tsEbpAudioInterval
pattern M2tsEbpAudioInterval_VIDEO_INTERVAL = M2tsEbpAudioInterval' "VIDEO_INTERVAL"

{-# COMPLETE
  M2tsEbpAudioInterval_VIDEO_AND_FIXED_INTERVALS,
  M2tsEbpAudioInterval_VIDEO_INTERVAL,
  M2tsEbpAudioInterval'
  #-}
