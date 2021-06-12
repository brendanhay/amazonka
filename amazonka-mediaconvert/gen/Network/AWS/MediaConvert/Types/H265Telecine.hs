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
-- Module      : Network.AWS.MediaConvert.Types.H265Telecine
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H265Telecine
  ( H265Telecine
      ( ..,
        H265Telecine_HARD,
        H265Telecine_NONE,
        H265Telecine_SOFT
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | This field applies only if the Streams > Advanced > Framerate
-- (framerate) field is set to 29.970. This field works with the Streams >
-- Advanced > Preprocessors > Deinterlacer field (deinterlace_mode) and the
-- Streams > Advanced > Interlaced Mode field (interlace_mode) to identify
-- the scan type for the output: Progressive, Interlaced, Hard Telecine or
-- Soft Telecine. - Hard: produces 29.97i output from 23.976 input. - Soft:
-- produces 23.976; the player converts this output to 29.97i.
newtype H265Telecine = H265Telecine'
  { fromH265Telecine ::
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

pattern H265Telecine_HARD :: H265Telecine
pattern H265Telecine_HARD = H265Telecine' "HARD"

pattern H265Telecine_NONE :: H265Telecine
pattern H265Telecine_NONE = H265Telecine' "NONE"

pattern H265Telecine_SOFT :: H265Telecine
pattern H265Telecine_SOFT = H265Telecine' "SOFT"

{-# COMPLETE
  H265Telecine_HARD,
  H265Telecine_NONE,
  H265Telecine_SOFT,
  H265Telecine'
  #-}
