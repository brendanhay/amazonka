-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Eac3BitstreamMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Eac3BitstreamMode
  ( Eac3BitstreamMode
      ( Eac3BitstreamMode',
        EBMCommentary,
        EBMCompleteMain,
        EBMEmergency,
        EBMHearingImpaired,
        EBMVisuallyImpaired
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Specify the bitstream mode for the E-AC-3 stream that the encoder emits. For more information about the EAC3 bitstream mode, see ATSC A/52-2012 (Annex E).
newtype Eac3BitstreamMode = Eac3BitstreamMode' Lude.Text
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

pattern EBMCommentary :: Eac3BitstreamMode
pattern EBMCommentary = Eac3BitstreamMode' "COMMENTARY"

pattern EBMCompleteMain :: Eac3BitstreamMode
pattern EBMCompleteMain = Eac3BitstreamMode' "COMPLETE_MAIN"

pattern EBMEmergency :: Eac3BitstreamMode
pattern EBMEmergency = Eac3BitstreamMode' "EMERGENCY"

pattern EBMHearingImpaired :: Eac3BitstreamMode
pattern EBMHearingImpaired = Eac3BitstreamMode' "HEARING_IMPAIRED"

pattern EBMVisuallyImpaired :: Eac3BitstreamMode
pattern EBMVisuallyImpaired = Eac3BitstreamMode' "VISUALLY_IMPAIRED"

{-# COMPLETE
  EBMCommentary,
  EBMCompleteMain,
  EBMEmergency,
  EBMHearingImpaired,
  EBMVisuallyImpaired,
  Eac3BitstreamMode'
  #-}
