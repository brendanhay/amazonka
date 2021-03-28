{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265Telecine
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.H265Telecine
  ( H265Telecine
    ( H265Telecine'
    , H265TelecineNone
    , H265TelecineSoft
    , H265TelecineHard
    , fromH265Telecine
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | This field applies only if the Streams > Advanced > Framerate (framerate) field  is set to 29.970. This field works with the Streams > Advanced > Preprocessors > Deinterlacer  field (deinterlace_mode) and the Streams > Advanced > Interlaced Mode field (interlace_mode)  to identify the scan type for the output: Progressive, Interlaced, Hard Telecine or Soft Telecine. - Hard: produces 29.97i output from 23.976 input. - Soft: produces 23.976; the player converts this output to 29.97i.
newtype H265Telecine = H265Telecine'{fromH265Telecine :: Core.Text}
                         deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                         Core.Generic)
                         deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                           Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                           Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                           Core.FromText, Core.ToByteString, Core.ToQuery,
                                           Core.ToHeader)

pattern H265TelecineNone :: H265Telecine
pattern H265TelecineNone = H265Telecine' "NONE"

pattern H265TelecineSoft :: H265Telecine
pattern H265TelecineSoft = H265Telecine' "SOFT"

pattern H265TelecineHard :: H265Telecine
pattern H265TelecineHard = H265Telecine' "HARD"

{-# COMPLETE 
  H265TelecineNone,

  H265TelecineSoft,

  H265TelecineHard,
  H265Telecine'
  #-}
