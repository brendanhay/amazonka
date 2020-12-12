{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Ac3BitstreamMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Ac3BitstreamMode
  ( Ac3BitstreamMode
      ( Ac3BitstreamMode',
        ABMCommentary,
        ABMCompleteMain,
        ABMDialogue,
        ABMEmergency,
        ABMHearingImpaired,
        ABMMusicAndEffects,
        ABMVisuallyImpaired,
        ABMVoiceOver
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Specify the bitstream mode for the AC-3 stream that the encoder emits. For more information about the AC3 bitstream mode, see ATSC A/52-2012 (Annex E).
newtype Ac3BitstreamMode = Ac3BitstreamMode' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern ABMCommentary :: Ac3BitstreamMode
pattern ABMCommentary = Ac3BitstreamMode' "COMMENTARY"

pattern ABMCompleteMain :: Ac3BitstreamMode
pattern ABMCompleteMain = Ac3BitstreamMode' "COMPLETE_MAIN"

pattern ABMDialogue :: Ac3BitstreamMode
pattern ABMDialogue = Ac3BitstreamMode' "DIALOGUE"

pattern ABMEmergency :: Ac3BitstreamMode
pattern ABMEmergency = Ac3BitstreamMode' "EMERGENCY"

pattern ABMHearingImpaired :: Ac3BitstreamMode
pattern ABMHearingImpaired = Ac3BitstreamMode' "HEARING_IMPAIRED"

pattern ABMMusicAndEffects :: Ac3BitstreamMode
pattern ABMMusicAndEffects = Ac3BitstreamMode' "MUSIC_AND_EFFECTS"

pattern ABMVisuallyImpaired :: Ac3BitstreamMode
pattern ABMVisuallyImpaired = Ac3BitstreamMode' "VISUALLY_IMPAIRED"

pattern ABMVoiceOver :: Ac3BitstreamMode
pattern ABMVoiceOver = Ac3BitstreamMode' "VOICE_OVER"

{-# COMPLETE
  ABMCommentary,
  ABMCompleteMain,
  ABMDialogue,
  ABMEmergency,
  ABMHearingImpaired,
  ABMMusicAndEffects,
  ABMVisuallyImpaired,
  ABMVoiceOver,
  Ac3BitstreamMode'
  #-}
