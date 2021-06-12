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
-- Module      : Network.AWS.MediaConvert.Types.MotionImagePlayback
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MotionImagePlayback
  ( MotionImagePlayback
      ( ..,
        MotionImagePlayback_ONCE,
        MotionImagePlayback_REPEAT
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Specify whether your motion graphic overlay repeats on a loop or plays
-- only once.
newtype MotionImagePlayback = MotionImagePlayback'
  { fromMotionImagePlayback ::
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

pattern MotionImagePlayback_ONCE :: MotionImagePlayback
pattern MotionImagePlayback_ONCE = MotionImagePlayback' "ONCE"

pattern MotionImagePlayback_REPEAT :: MotionImagePlayback
pattern MotionImagePlayback_REPEAT = MotionImagePlayback' "REPEAT"

{-# COMPLETE
  MotionImagePlayback_ONCE,
  MotionImagePlayback_REPEAT,
  MotionImagePlayback'
  #-}
