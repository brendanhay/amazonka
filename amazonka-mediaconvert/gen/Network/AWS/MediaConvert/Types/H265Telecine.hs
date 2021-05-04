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

import qualified Network.AWS.Prelude as Prelude

-- | This field applies only if the Streams > Advanced > Framerate
-- (framerate) field is set to 29.970. This field works with the Streams >
-- Advanced > Preprocessors > Deinterlacer field (deinterlace_mode) and the
-- Streams > Advanced > Interlaced Mode field (interlace_mode) to identify
-- the scan type for the output: Progressive, Interlaced, Hard Telecine or
-- Soft Telecine. - Hard: produces 29.97i output from 23.976 input. - Soft:
-- produces 23.976; the player converts this output to 29.97i.
newtype H265Telecine = H265Telecine'
  { fromH265Telecine ::
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
