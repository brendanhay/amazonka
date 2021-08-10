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
-- Module      : Network.AWS.MediaLive.Types.H264TimecodeInsertionBehavior
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H264TimecodeInsertionBehavior
  ( H264TimecodeInsertionBehavior
      ( ..,
        H264TimecodeInsertionBehavior_DISABLED,
        H264TimecodeInsertionBehavior_PIC_TIMING_SEI
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | H264 Timecode Insertion Behavior
newtype H264TimecodeInsertionBehavior = H264TimecodeInsertionBehavior'
  { fromH264TimecodeInsertionBehavior ::
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

pattern H264TimecodeInsertionBehavior_DISABLED :: H264TimecodeInsertionBehavior
pattern H264TimecodeInsertionBehavior_DISABLED = H264TimecodeInsertionBehavior' "DISABLED"

pattern H264TimecodeInsertionBehavior_PIC_TIMING_SEI :: H264TimecodeInsertionBehavior
pattern H264TimecodeInsertionBehavior_PIC_TIMING_SEI = H264TimecodeInsertionBehavior' "PIC_TIMING_SEI"

{-# COMPLETE
  H264TimecodeInsertionBehavior_DISABLED,
  H264TimecodeInsertionBehavior_PIC_TIMING_SEI,
  H264TimecodeInsertionBehavior'
  #-}
