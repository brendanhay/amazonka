{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Ac3BitstreamMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.Ac3BitstreamMode
  ( Ac3BitstreamMode
    ( Ac3BitstreamMode'
    , Ac3BitstreamModeCommentary
    , Ac3BitstreamModeCompleteMain
    , Ac3BitstreamModeDialogue
    , Ac3BitstreamModeEmergency
    , Ac3BitstreamModeHearingImpaired
    , Ac3BitstreamModeMusicAndEffects
    , Ac3BitstreamModeVisuallyImpaired
    , Ac3BitstreamModeVoiceOver
    , fromAc3BitstreamMode
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Ac3 Bitstream Mode
newtype Ac3BitstreamMode = Ac3BitstreamMode'{fromAc3BitstreamMode
                                             :: Core.Text}
                             deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                             Core.Generic)
                             deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                               Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                               Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                               Core.FromText, Core.ToByteString, Core.ToQuery,
                                               Core.ToHeader)

pattern Ac3BitstreamModeCommentary :: Ac3BitstreamMode
pattern Ac3BitstreamModeCommentary = Ac3BitstreamMode' "COMMENTARY"

pattern Ac3BitstreamModeCompleteMain :: Ac3BitstreamMode
pattern Ac3BitstreamModeCompleteMain = Ac3BitstreamMode' "COMPLETE_MAIN"

pattern Ac3BitstreamModeDialogue :: Ac3BitstreamMode
pattern Ac3BitstreamModeDialogue = Ac3BitstreamMode' "DIALOGUE"

pattern Ac3BitstreamModeEmergency :: Ac3BitstreamMode
pattern Ac3BitstreamModeEmergency = Ac3BitstreamMode' "EMERGENCY"

pattern Ac3BitstreamModeHearingImpaired :: Ac3BitstreamMode
pattern Ac3BitstreamModeHearingImpaired = Ac3BitstreamMode' "HEARING_IMPAIRED"

pattern Ac3BitstreamModeMusicAndEffects :: Ac3BitstreamMode
pattern Ac3BitstreamModeMusicAndEffects = Ac3BitstreamMode' "MUSIC_AND_EFFECTS"

pattern Ac3BitstreamModeVisuallyImpaired :: Ac3BitstreamMode
pattern Ac3BitstreamModeVisuallyImpaired = Ac3BitstreamMode' "VISUALLY_IMPAIRED"

pattern Ac3BitstreamModeVoiceOver :: Ac3BitstreamMode
pattern Ac3BitstreamModeVoiceOver = Ac3BitstreamMode' "VOICE_OVER"

{-# COMPLETE 
  Ac3BitstreamModeCommentary,

  Ac3BitstreamModeCompleteMain,

  Ac3BitstreamModeDialogue,

  Ac3BitstreamModeEmergency,

  Ac3BitstreamModeHearingImpaired,

  Ac3BitstreamModeMusicAndEffects,

  Ac3BitstreamModeVisuallyImpaired,

  Ac3BitstreamModeVoiceOver,
  Ac3BitstreamMode'
  #-}
