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
-- Module      : Network.AWS.MediaLive.Types.HlsSegmentationMode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsSegmentationMode
  ( HlsSegmentationMode
      ( ..,
        HlsSegmentationMode_USE_INPUT_SEGMENTATION,
        HlsSegmentationMode_USE_SEGMENT_DURATION
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Hls Segmentation Mode
newtype HlsSegmentationMode = HlsSegmentationMode'
  { fromHlsSegmentationMode ::
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

pattern HlsSegmentationMode_USE_INPUT_SEGMENTATION :: HlsSegmentationMode
pattern HlsSegmentationMode_USE_INPUT_SEGMENTATION = HlsSegmentationMode' "USE_INPUT_SEGMENTATION"

pattern HlsSegmentationMode_USE_SEGMENT_DURATION :: HlsSegmentationMode
pattern HlsSegmentationMode_USE_SEGMENT_DURATION = HlsSegmentationMode' "USE_SEGMENT_DURATION"

{-# COMPLETE
  HlsSegmentationMode_USE_INPUT_SEGMENTATION,
  HlsSegmentationMode_USE_SEGMENT_DURATION,
  HlsSegmentationMode'
  #-}
