{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CaptionSourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CaptionSourceType
  ( CaptionSourceType
      ( CaptionSourceType',
        CSTAncillary,
        CSTDvbSub,
        CSTEmbedded,
        CSTSCTE20,
        CSTScc,
        CSTTtml,
        CSTStl,
        CSTSrt,
        CSTSmi,
        CSTTeletext,
        CSTNullSource,
        CSTImsc
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Use Source (SourceType) to identify the format of your input captions.  The service cannot auto-detect caption format.
newtype CaptionSourceType = CaptionSourceType' Lude.Text
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

pattern CSTAncillary :: CaptionSourceType
pattern CSTAncillary = CaptionSourceType' "ANCILLARY"

pattern CSTDvbSub :: CaptionSourceType
pattern CSTDvbSub = CaptionSourceType' "DVB_SUB"

pattern CSTEmbedded :: CaptionSourceType
pattern CSTEmbedded = CaptionSourceType' "EMBEDDED"

pattern CSTSCTE20 :: CaptionSourceType
pattern CSTSCTE20 = CaptionSourceType' "SCTE20"

pattern CSTScc :: CaptionSourceType
pattern CSTScc = CaptionSourceType' "SCC"

pattern CSTTtml :: CaptionSourceType
pattern CSTTtml = CaptionSourceType' "TTML"

pattern CSTStl :: CaptionSourceType
pattern CSTStl = CaptionSourceType' "STL"

pattern CSTSrt :: CaptionSourceType
pattern CSTSrt = CaptionSourceType' "SRT"

pattern CSTSmi :: CaptionSourceType
pattern CSTSmi = CaptionSourceType' "SMI"

pattern CSTTeletext :: CaptionSourceType
pattern CSTTeletext = CaptionSourceType' "TELETEXT"

pattern CSTNullSource :: CaptionSourceType
pattern CSTNullSource = CaptionSourceType' "NULL_SOURCE"

pattern CSTImsc :: CaptionSourceType
pattern CSTImsc = CaptionSourceType' "IMSC"

{-# COMPLETE
  CSTAncillary,
  CSTDvbSub,
  CSTEmbedded,
  CSTSCTE20,
  CSTScc,
  CSTTtml,
  CSTStl,
  CSTSrt,
  CSTSmi,
  CSTTeletext,
  CSTNullSource,
  CSTImsc,
  CaptionSourceType'
  #-}
