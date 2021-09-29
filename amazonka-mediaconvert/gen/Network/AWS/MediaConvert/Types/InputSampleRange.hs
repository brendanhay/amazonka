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
-- Module      : Network.AWS.MediaConvert.Types.InputSampleRange
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.InputSampleRange
  ( InputSampleRange
      ( ..,
        InputSampleRange_FOLLOW,
        InputSampleRange_FULL_RANGE,
        InputSampleRange_LIMITED_RANGE
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Use this setting when your input video codec is AVC-Intra. Ignore this
-- setting for all other inputs. If the sample range metadata in your input
-- video is accurate, or if you don\'t know about sample range, keep the
-- default value, Follow (FOLLOW), for this setting. When you do, the
-- service automatically detects your input sample range. If your input
-- video has metadata indicating the wrong sample range, specify the
-- accurate sample range here. When you do, MediaConvert ignores any sample
-- range information in the input metadata. Regardless of whether
-- MediaConvert uses the input sample range or the sample range that you
-- specify, MediaConvert uses the sample range for transcoding and also
-- writes it to the output metadata.
newtype InputSampleRange = InputSampleRange'
  { fromInputSampleRange ::
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

pattern InputSampleRange_FOLLOW :: InputSampleRange
pattern InputSampleRange_FOLLOW = InputSampleRange' "FOLLOW"

pattern InputSampleRange_FULL_RANGE :: InputSampleRange
pattern InputSampleRange_FULL_RANGE = InputSampleRange' "FULL_RANGE"

pattern InputSampleRange_LIMITED_RANGE :: InputSampleRange
pattern InputSampleRange_LIMITED_RANGE = InputSampleRange' "LIMITED_RANGE"

{-# COMPLETE
  InputSampleRange_FOLLOW,
  InputSampleRange_FULL_RANGE,
  InputSampleRange_LIMITED_RANGE,
  InputSampleRange'
  #-}
