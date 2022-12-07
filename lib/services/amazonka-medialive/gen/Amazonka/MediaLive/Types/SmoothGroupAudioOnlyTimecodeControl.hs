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
-- Module      : Amazonka.MediaLive.Types.SmoothGroupAudioOnlyTimecodeControl
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.SmoothGroupAudioOnlyTimecodeControl
  ( SmoothGroupAudioOnlyTimecodeControl
      ( ..,
        SmoothGroupAudioOnlyTimecodeControl_PASSTHROUGH,
        SmoothGroupAudioOnlyTimecodeControl_USE_CONFIGURED_CLOCK
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Smooth Group Audio Only Timecode Control
newtype SmoothGroupAudioOnlyTimecodeControl = SmoothGroupAudioOnlyTimecodeControl'
  { fromSmoothGroupAudioOnlyTimecodeControl ::
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

pattern SmoothGroupAudioOnlyTimecodeControl_PASSTHROUGH :: SmoothGroupAudioOnlyTimecodeControl
pattern SmoothGroupAudioOnlyTimecodeControl_PASSTHROUGH = SmoothGroupAudioOnlyTimecodeControl' "PASSTHROUGH"

pattern SmoothGroupAudioOnlyTimecodeControl_USE_CONFIGURED_CLOCK :: SmoothGroupAudioOnlyTimecodeControl
pattern SmoothGroupAudioOnlyTimecodeControl_USE_CONFIGURED_CLOCK = SmoothGroupAudioOnlyTimecodeControl' "USE_CONFIGURED_CLOCK"

{-# COMPLETE
  SmoothGroupAudioOnlyTimecodeControl_PASSTHROUGH,
  SmoothGroupAudioOnlyTimecodeControl_USE_CONFIGURED_CLOCK,
  SmoothGroupAudioOnlyTimecodeControl'
  #-}
