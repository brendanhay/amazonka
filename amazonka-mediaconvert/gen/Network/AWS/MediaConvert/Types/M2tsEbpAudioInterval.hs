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
-- Module      : Network.AWS.MediaConvert.Types.M2tsEbpAudioInterval
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.M2tsEbpAudioInterval
  ( M2tsEbpAudioInterval
      ( ..,
        M2tsEbpAudioInterval_VIDEO_AND_FIXED_INTERVALS,
        M2tsEbpAudioInterval_VIDEO_INTERVAL
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | When set to VIDEO_AND_FIXED_INTERVALS, audio EBP markers will be added
-- to partitions 3 and 4. The interval between these additional markers
-- will be fixed, and will be slightly shorter than the video EBP marker
-- interval. When set to VIDEO_INTERVAL, these additional markers will not
-- be inserted. Only applicable when EBP segmentation markers are is
-- selected (segmentationMarkers is EBP or EBP_LEGACY).
newtype M2tsEbpAudioInterval = M2tsEbpAudioInterval'
  { fromM2tsEbpAudioInterval ::
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

pattern M2tsEbpAudioInterval_VIDEO_AND_FIXED_INTERVALS :: M2tsEbpAudioInterval
pattern M2tsEbpAudioInterval_VIDEO_AND_FIXED_INTERVALS = M2tsEbpAudioInterval' "VIDEO_AND_FIXED_INTERVALS"

pattern M2tsEbpAudioInterval_VIDEO_INTERVAL :: M2tsEbpAudioInterval
pattern M2tsEbpAudioInterval_VIDEO_INTERVAL = M2tsEbpAudioInterval' "VIDEO_INTERVAL"

{-# COMPLETE
  M2tsEbpAudioInterval_VIDEO_AND_FIXED_INTERVALS,
  M2tsEbpAudioInterval_VIDEO_INTERVAL,
  M2tsEbpAudioInterval'
  #-}
