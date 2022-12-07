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
-- Module      : Amazonka.MediaConvert.Types.InputTimecodeSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.InputTimecodeSource
  ( InputTimecodeSource
      ( ..,
        InputTimecodeSource_EMBEDDED,
        InputTimecodeSource_SPECIFIEDSTART,
        InputTimecodeSource_ZEROBASED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Use this Timecode source setting, located under the input settings
-- (InputTimecodeSource), to specify how the service counts input video
-- frames. This input frame count affects only the behavior of features
-- that apply to a single input at a time, such as input clipping and
-- synchronizing some captions formats. Choose Embedded (EMBEDDED) to use
-- the timecodes in your input video. Choose Start at zero (ZEROBASED) to
-- start the first frame at zero. Choose Specified start (SPECIFIEDSTART)
-- to start the first frame at the timecode that you specify in the setting
-- Start timecode (timecodeStart). If you don\'t specify a value for
-- Timecode source, the service will use Embedded by default. For more
-- information about timecodes, see
-- https:\/\/docs.aws.amazon.com\/console\/mediaconvert\/timecode.
newtype InputTimecodeSource = InputTimecodeSource'
  { fromInputTimecodeSource ::
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

pattern InputTimecodeSource_EMBEDDED :: InputTimecodeSource
pattern InputTimecodeSource_EMBEDDED = InputTimecodeSource' "EMBEDDED"

pattern InputTimecodeSource_SPECIFIEDSTART :: InputTimecodeSource
pattern InputTimecodeSource_SPECIFIEDSTART = InputTimecodeSource' "SPECIFIEDSTART"

pattern InputTimecodeSource_ZEROBASED :: InputTimecodeSource
pattern InputTimecodeSource_ZEROBASED = InputTimecodeSource' "ZEROBASED"

{-# COMPLETE
  InputTimecodeSource_EMBEDDED,
  InputTimecodeSource_SPECIFIEDSTART,
  InputTimecodeSource_ZEROBASED,
  InputTimecodeSource'
  #-}
