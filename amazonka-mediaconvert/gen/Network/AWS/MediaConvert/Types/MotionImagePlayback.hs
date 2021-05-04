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

import qualified Network.AWS.Prelude as Prelude

-- | Specify whether your motion graphic overlay repeats on a loop or plays
-- only once.
newtype MotionImagePlayback = MotionImagePlayback'
  { fromMotionImagePlayback ::
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

pattern MotionImagePlayback_ONCE :: MotionImagePlayback
pattern MotionImagePlayback_ONCE = MotionImagePlayback' "ONCE"

pattern MotionImagePlayback_REPEAT :: MotionImagePlayback
pattern MotionImagePlayback_REPEAT = MotionImagePlayback' "REPEAT"

{-# COMPLETE
  MotionImagePlayback_ONCE,
  MotionImagePlayback_REPEAT,
  MotionImagePlayback'
  #-}
