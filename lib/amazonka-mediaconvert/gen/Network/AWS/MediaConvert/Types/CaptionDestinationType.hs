{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CaptionDestinationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CaptionDestinationType
  ( CaptionDestinationType
      ( CaptionDestinationType',
        CDTBurnIn,
        CDTDvbSub,
        CDTEmbedded,
        CDTEmbeddedPlusSCTE20,
        CDTImsc,
        CDTSCTE20PlusEmbedded,
        CDTScc,
        CDTSrt,
        CDTSmi,
        CDTTeletext,
        CDTTtml,
        CDTWebvtt
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Specify the format for this set of captions on this output. The default format is embedded without SCTE-20. Other options are embedded with SCTE-20, burn-in, DVB-sub, IMSC, SCC, SRT, teletext, TTML, and web-VTT. If you are using SCTE-20, choose SCTE-20 plus embedded (SCTE20_PLUS_EMBEDDED) to create an output that complies with the SCTE-43 spec. To create a non-compliant output where the embedded captions come first, choose Embedded plus SCTE-20 (EMBEDDED_PLUS_SCTE20).
newtype CaptionDestinationType = CaptionDestinationType' Lude.Text
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

pattern CDTBurnIn :: CaptionDestinationType
pattern CDTBurnIn = CaptionDestinationType' "BURN_IN"

pattern CDTDvbSub :: CaptionDestinationType
pattern CDTDvbSub = CaptionDestinationType' "DVB_SUB"

pattern CDTEmbedded :: CaptionDestinationType
pattern CDTEmbedded = CaptionDestinationType' "EMBEDDED"

pattern CDTEmbeddedPlusSCTE20 :: CaptionDestinationType
pattern CDTEmbeddedPlusSCTE20 = CaptionDestinationType' "EMBEDDED_PLUS_SCTE20"

pattern CDTImsc :: CaptionDestinationType
pattern CDTImsc = CaptionDestinationType' "IMSC"

pattern CDTSCTE20PlusEmbedded :: CaptionDestinationType
pattern CDTSCTE20PlusEmbedded = CaptionDestinationType' "SCTE20_PLUS_EMBEDDED"

pattern CDTScc :: CaptionDestinationType
pattern CDTScc = CaptionDestinationType' "SCC"

pattern CDTSrt :: CaptionDestinationType
pattern CDTSrt = CaptionDestinationType' "SRT"

pattern CDTSmi :: CaptionDestinationType
pattern CDTSmi = CaptionDestinationType' "SMI"

pattern CDTTeletext :: CaptionDestinationType
pattern CDTTeletext = CaptionDestinationType' "TELETEXT"

pattern CDTTtml :: CaptionDestinationType
pattern CDTTtml = CaptionDestinationType' "TTML"

pattern CDTWebvtt :: CaptionDestinationType
pattern CDTWebvtt = CaptionDestinationType' "WEBVTT"

{-# COMPLETE
  CDTBurnIn,
  CDTDvbSub,
  CDTEmbedded,
  CDTEmbeddedPlusSCTE20,
  CDTImsc,
  CDTSCTE20PlusEmbedded,
  CDTScc,
  CDTSrt,
  CDTSmi,
  CDTTeletext,
  CDTTtml,
  CDTWebvtt,
  CaptionDestinationType'
  #-}
