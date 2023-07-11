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
-- Module      : Amazonka.MediaConvert.Types.HlsCaptionSegmentLengthControl
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.HlsCaptionSegmentLengthControl
  ( HlsCaptionSegmentLengthControl
      ( ..,
        HlsCaptionSegmentLengthControl_LARGE_SEGMENTS,
        HlsCaptionSegmentLengthControl_MATCH_VIDEO
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Set Caption segment length control (CaptionSegmentLengthControl) to
-- Match video (MATCH_VIDEO) to create caption segments that align with the
-- video segments from the first video output in this output group. For
-- example, if the video segments are 2 seconds long, your WebVTT segments
-- will also be 2 seconds long. Keep the default setting, Large segments
-- (LARGE_SEGMENTS) to create caption segments that are 300 seconds long.
newtype HlsCaptionSegmentLengthControl = HlsCaptionSegmentLengthControl'
  { fromHlsCaptionSegmentLengthControl ::
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

pattern HlsCaptionSegmentLengthControl_LARGE_SEGMENTS :: HlsCaptionSegmentLengthControl
pattern HlsCaptionSegmentLengthControl_LARGE_SEGMENTS = HlsCaptionSegmentLengthControl' "LARGE_SEGMENTS"

pattern HlsCaptionSegmentLengthControl_MATCH_VIDEO :: HlsCaptionSegmentLengthControl
pattern HlsCaptionSegmentLengthControl_MATCH_VIDEO = HlsCaptionSegmentLengthControl' "MATCH_VIDEO"

{-# COMPLETE
  HlsCaptionSegmentLengthControl_LARGE_SEGMENTS,
  HlsCaptionSegmentLengthControl_MATCH_VIDEO,
  HlsCaptionSegmentLengthControl'
  #-}
