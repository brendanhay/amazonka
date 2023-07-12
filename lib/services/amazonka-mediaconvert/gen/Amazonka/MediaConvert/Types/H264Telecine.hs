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
-- Module      : Amazonka.MediaConvert.Types.H264Telecine
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.H264Telecine
  ( H264Telecine
      ( ..,
        H264Telecine_HARD,
        H264Telecine_NONE,
        H264Telecine_SOFT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | When you do frame rate conversion from 23.976 frames per second (fps) to
-- 29.97 fps, and your output scan type is interlaced, you can optionally
-- enable hard or soft telecine to create a smoother picture. Hard telecine
-- (HARD) produces a 29.97i output. Soft telecine (SOFT) produces an output
-- with a 23.976 output that signals to the video player device to do the
-- conversion during play back. When you keep the default value, None
-- (NONE), MediaConvert does a standard frame rate conversion to 29.97
-- without doing anything with the field polarity to create a smoother
-- picture.
newtype H264Telecine = H264Telecine'
  { fromH264Telecine ::
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

pattern H264Telecine_HARD :: H264Telecine
pattern H264Telecine_HARD = H264Telecine' "HARD"

pattern H264Telecine_NONE :: H264Telecine
pattern H264Telecine_NONE = H264Telecine' "NONE"

pattern H264Telecine_SOFT :: H264Telecine
pattern H264Telecine_SOFT = H264Telecine' "SOFT"

{-# COMPLETE
  H264Telecine_HARD,
  H264Telecine_NONE,
  H264Telecine_SOFT,
  H264Telecine'
  #-}
