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
-- Module      : Amazonka.MediaConvert.Types.InputSampleRange
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.InputSampleRange
  ( InputSampleRange
      ( ..,
        InputSampleRange_FOLLOW,
        InputSampleRange_FULL_RANGE,
        InputSampleRange_LIMITED_RANGE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | If the sample range metadata in your input video is accurate, or if you
-- don\'t know about sample range, keep the default value, Follow (FOLLOW),
-- for this setting. When you do, the service automatically detects your
-- input sample range. If your input video has metadata indicating the
-- wrong sample range, specify the accurate sample range here. When you do,
-- MediaConvert ignores any sample range information in the input metadata.
-- Regardless of whether MediaConvert uses the input sample range or the
-- sample range that you specify, MediaConvert uses the sample range for
-- transcoding and also writes it to the output metadata.
newtype InputSampleRange = InputSampleRange'
  { fromInputSampleRange ::
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
