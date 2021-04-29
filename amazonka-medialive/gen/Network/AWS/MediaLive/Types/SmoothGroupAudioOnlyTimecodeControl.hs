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
-- Module      : Network.AWS.MediaLive.Types.SmoothGroupAudioOnlyTimecodeControl
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.SmoothGroupAudioOnlyTimecodeControl
  ( SmoothGroupAudioOnlyTimecodeControl
      ( ..,
        SmoothGroupAudioOnlyTimecodeControl_PASSTHROUGH,
        SmoothGroupAudioOnlyTimecodeControl_USE_CONFIGURED_CLOCK
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Smooth Group Audio Only Timecode Control
newtype SmoothGroupAudioOnlyTimecodeControl = SmoothGroupAudioOnlyTimecodeControl'
  { fromSmoothGroupAudioOnlyTimecodeControl ::
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

pattern SmoothGroupAudioOnlyTimecodeControl_PASSTHROUGH :: SmoothGroupAudioOnlyTimecodeControl
pattern SmoothGroupAudioOnlyTimecodeControl_PASSTHROUGH = SmoothGroupAudioOnlyTimecodeControl' "PASSTHROUGH"

pattern SmoothGroupAudioOnlyTimecodeControl_USE_CONFIGURED_CLOCK :: SmoothGroupAudioOnlyTimecodeControl
pattern SmoothGroupAudioOnlyTimecodeControl_USE_CONFIGURED_CLOCK = SmoothGroupAudioOnlyTimecodeControl' "USE_CONFIGURED_CLOCK"

{-# COMPLETE
  SmoothGroupAudioOnlyTimecodeControl_PASSTHROUGH,
  SmoothGroupAudioOnlyTimecodeControl_USE_CONFIGURED_CLOCK,
  SmoothGroupAudioOnlyTimecodeControl'
  #-}
