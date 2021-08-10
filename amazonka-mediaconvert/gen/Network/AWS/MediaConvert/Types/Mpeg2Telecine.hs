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
-- Module      : Network.AWS.MediaConvert.Types.Mpeg2Telecine
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Mpeg2Telecine
  ( Mpeg2Telecine
      ( ..,
        Mpeg2Telecine_HARD,
        Mpeg2Telecine_NONE,
        Mpeg2Telecine_SOFT
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | When you do frame rate conversion from 23.976 frames per second (fps) to
-- 29.97 fps, and your output scan type is interlaced, you can optionally
-- enable hard or soft telecine to create a smoother picture. Hard telecine
-- (HARD) produces a 29.97i output. Soft telecine (SOFT) produces an output
-- with a 23.976 output that signals to the video player device to do the
-- conversion during play back. When you keep the default value, None
-- (NONE), MediaConvert does a standard frame rate conversion to 29.97
-- without doing anything with the field polarity to create a smoother
-- picture.
newtype Mpeg2Telecine = Mpeg2Telecine'
  { fromMpeg2Telecine ::
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

pattern Mpeg2Telecine_HARD :: Mpeg2Telecine
pattern Mpeg2Telecine_HARD = Mpeg2Telecine' "HARD"

pattern Mpeg2Telecine_NONE :: Mpeg2Telecine
pattern Mpeg2Telecine_NONE = Mpeg2Telecine' "NONE"

pattern Mpeg2Telecine_SOFT :: Mpeg2Telecine
pattern Mpeg2Telecine_SOFT = Mpeg2Telecine' "SOFT"

{-# COMPLETE
  Mpeg2Telecine_HARD,
  Mpeg2Telecine_NONE,
  Mpeg2Telecine_SOFT,
  Mpeg2Telecine'
  #-}
