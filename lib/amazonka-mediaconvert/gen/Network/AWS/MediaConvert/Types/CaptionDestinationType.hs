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
        CaptionDestinationTypeBurnIn,
        CaptionDestinationTypeDvbSub,
        CaptionDestinationTypeEmbedded,
        CaptionDestinationTypeEmbeddedPlusSCTE20,
        CaptionDestinationTypeImsc,
        CaptionDestinationTypeSCTE20PlusEmbedded,
        CaptionDestinationTypeScc,
        CaptionDestinationTypeSrt,
        CaptionDestinationTypeSmi,
        CaptionDestinationTypeTeletext,
        CaptionDestinationTypeTtml,
        CaptionDestinationTypeWebvtt,
        fromCaptionDestinationType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Specify the format for this set of captions on this output. The default format is embedded without SCTE-20. Other options are embedded with SCTE-20, burn-in, DVB-sub, IMSC, SCC, SRT, teletext, TTML, and web-VTT. If you are using SCTE-20, choose SCTE-20 plus embedded (SCTE20_PLUS_EMBEDDED) to create an output that complies with the SCTE-43 spec. To create a non-compliant output where the embedded captions come first, choose Embedded plus SCTE-20 (EMBEDDED_PLUS_SCTE20).
newtype CaptionDestinationType = CaptionDestinationType'
  { fromCaptionDestinationType ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern CaptionDestinationTypeBurnIn :: CaptionDestinationType
pattern CaptionDestinationTypeBurnIn = CaptionDestinationType' "BURN_IN"

pattern CaptionDestinationTypeDvbSub :: CaptionDestinationType
pattern CaptionDestinationTypeDvbSub = CaptionDestinationType' "DVB_SUB"

pattern CaptionDestinationTypeEmbedded :: CaptionDestinationType
pattern CaptionDestinationTypeEmbedded = CaptionDestinationType' "EMBEDDED"

pattern CaptionDestinationTypeEmbeddedPlusSCTE20 :: CaptionDestinationType
pattern CaptionDestinationTypeEmbeddedPlusSCTE20 = CaptionDestinationType' "EMBEDDED_PLUS_SCTE20"

pattern CaptionDestinationTypeImsc :: CaptionDestinationType
pattern CaptionDestinationTypeImsc = CaptionDestinationType' "IMSC"

pattern CaptionDestinationTypeSCTE20PlusEmbedded :: CaptionDestinationType
pattern CaptionDestinationTypeSCTE20PlusEmbedded = CaptionDestinationType' "SCTE20_PLUS_EMBEDDED"

pattern CaptionDestinationTypeScc :: CaptionDestinationType
pattern CaptionDestinationTypeScc = CaptionDestinationType' "SCC"

pattern CaptionDestinationTypeSrt :: CaptionDestinationType
pattern CaptionDestinationTypeSrt = CaptionDestinationType' "SRT"

pattern CaptionDestinationTypeSmi :: CaptionDestinationType
pattern CaptionDestinationTypeSmi = CaptionDestinationType' "SMI"

pattern CaptionDestinationTypeTeletext :: CaptionDestinationType
pattern CaptionDestinationTypeTeletext = CaptionDestinationType' "TELETEXT"

pattern CaptionDestinationTypeTtml :: CaptionDestinationType
pattern CaptionDestinationTypeTtml = CaptionDestinationType' "TTML"

pattern CaptionDestinationTypeWebvtt :: CaptionDestinationType
pattern CaptionDestinationTypeWebvtt = CaptionDestinationType' "WEBVTT"

{-# COMPLETE
  CaptionDestinationTypeBurnIn,
  CaptionDestinationTypeDvbSub,
  CaptionDestinationTypeEmbedded,
  CaptionDestinationTypeEmbeddedPlusSCTE20,
  CaptionDestinationTypeImsc,
  CaptionDestinationTypeSCTE20PlusEmbedded,
  CaptionDestinationTypeScc,
  CaptionDestinationTypeSrt,
  CaptionDestinationTypeSmi,
  CaptionDestinationTypeTeletext,
  CaptionDestinationTypeTtml,
  CaptionDestinationTypeWebvtt,
  CaptionDestinationType'
  #-}
