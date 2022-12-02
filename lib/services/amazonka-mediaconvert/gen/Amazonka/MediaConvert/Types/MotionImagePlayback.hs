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
-- Module      : Amazonka.MediaConvert.Types.MotionImagePlayback
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.MotionImagePlayback
  ( MotionImagePlayback
      ( ..,
        MotionImagePlayback_ONCE,
        MotionImagePlayback_REPEAT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specify whether your motion graphic overlay repeats on a loop or plays
-- only once.
newtype MotionImagePlayback = MotionImagePlayback'
  { fromMotionImagePlayback ::
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

pattern MotionImagePlayback_ONCE :: MotionImagePlayback
pattern MotionImagePlayback_ONCE = MotionImagePlayback' "ONCE"

pattern MotionImagePlayback_REPEAT :: MotionImagePlayback
pattern MotionImagePlayback_REPEAT = MotionImagePlayback' "REPEAT"

{-# COMPLETE
  MotionImagePlayback_ONCE,
  MotionImagePlayback_REPEAT,
  MotionImagePlayback'
  #-}
