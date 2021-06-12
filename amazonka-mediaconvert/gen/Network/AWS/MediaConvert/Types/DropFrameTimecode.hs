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
-- Module      : Network.AWS.MediaConvert.Types.DropFrameTimecode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DropFrameTimecode
  ( DropFrameTimecode
      ( ..,
        DropFrameTimecode_DISABLED,
        DropFrameTimecode_ENABLED
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Applies only to 29.97 fps outputs. When this feature is enabled, the
-- service will use drop-frame timecode on outputs. If it is not possible
-- to use drop-frame timecode, the system will fall back to non-drop-frame.
-- This setting is enabled by default when Timecode insertion
-- (TimecodeInsertion) is enabled.
newtype DropFrameTimecode = DropFrameTimecode'
  { fromDropFrameTimecode ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern DropFrameTimecode_DISABLED :: DropFrameTimecode
pattern DropFrameTimecode_DISABLED = DropFrameTimecode' "DISABLED"

pattern DropFrameTimecode_ENABLED :: DropFrameTimecode
pattern DropFrameTimecode_ENABLED = DropFrameTimecode' "ENABLED"

{-# COMPLETE
  DropFrameTimecode_DISABLED,
  DropFrameTimecode_ENABLED,
  DropFrameTimecode'
  #-}
