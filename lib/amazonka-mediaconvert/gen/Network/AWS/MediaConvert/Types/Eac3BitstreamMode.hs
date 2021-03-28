{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Eac3BitstreamMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Eac3BitstreamMode
  ( Eac3BitstreamMode
    ( Eac3BitstreamMode'
    , Eac3BitstreamModeCompleteMain
    , Eac3BitstreamModeCommentary
    , Eac3BitstreamModeEmergency
    , Eac3BitstreamModeHearingImpaired
    , Eac3BitstreamModeVisuallyImpaired
    , fromEac3BitstreamMode
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Specify the bitstream mode for the E-AC-3 stream that the encoder emits. For more information about the EAC3 bitstream mode, see ATSC A/52-2012 (Annex E).
newtype Eac3BitstreamMode = Eac3BitstreamMode'{fromEac3BitstreamMode
                                               :: Core.Text}
                              deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                              Core.Generic)
                              deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                Core.FromJSON, Core.ToXML, Core.FromXML,
                                                Core.ToText, Core.FromText, Core.ToByteString,
                                                Core.ToQuery, Core.ToHeader)

pattern Eac3BitstreamModeCompleteMain :: Eac3BitstreamMode
pattern Eac3BitstreamModeCompleteMain = Eac3BitstreamMode' "COMPLETE_MAIN"

pattern Eac3BitstreamModeCommentary :: Eac3BitstreamMode
pattern Eac3BitstreamModeCommentary = Eac3BitstreamMode' "COMMENTARY"

pattern Eac3BitstreamModeEmergency :: Eac3BitstreamMode
pattern Eac3BitstreamModeEmergency = Eac3BitstreamMode' "EMERGENCY"

pattern Eac3BitstreamModeHearingImpaired :: Eac3BitstreamMode
pattern Eac3BitstreamModeHearingImpaired = Eac3BitstreamMode' "HEARING_IMPAIRED"

pattern Eac3BitstreamModeVisuallyImpaired :: Eac3BitstreamMode
pattern Eac3BitstreamModeVisuallyImpaired = Eac3BitstreamMode' "VISUALLY_IMPAIRED"

{-# COMPLETE 
  Eac3BitstreamModeCompleteMain,

  Eac3BitstreamModeCommentary,

  Eac3BitstreamModeEmergency,

  Eac3BitstreamModeHearingImpaired,

  Eac3BitstreamModeVisuallyImpaired,
  Eac3BitstreamMode'
  #-}
