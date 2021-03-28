{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CaptionSourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.CaptionSourceType
  ( CaptionSourceType
    ( CaptionSourceType'
    , CaptionSourceTypeAncillary
    , CaptionSourceTypeDvbSub
    , CaptionSourceTypeEmbedded
    , CaptionSourceTypeSCTE20
    , CaptionSourceTypeScc
    , CaptionSourceTypeTtml
    , CaptionSourceTypeStl
    , CaptionSourceTypeSrt
    , CaptionSourceTypeSmi
    , CaptionSourceTypeTeletext
    , CaptionSourceTypeNullSource
    , CaptionSourceTypeImsc
    , fromCaptionSourceType
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Use Source (SourceType) to identify the format of your input captions.  The service cannot auto-detect caption format.
newtype CaptionSourceType = CaptionSourceType'{fromCaptionSourceType
                                               :: Core.Text}
                              deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                              Core.Generic)
                              deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                Core.FromJSON, Core.ToXML, Core.FromXML,
                                                Core.ToText, Core.FromText, Core.ToByteString,
                                                Core.ToQuery, Core.ToHeader)

pattern CaptionSourceTypeAncillary :: CaptionSourceType
pattern CaptionSourceTypeAncillary = CaptionSourceType' "ANCILLARY"

pattern CaptionSourceTypeDvbSub :: CaptionSourceType
pattern CaptionSourceTypeDvbSub = CaptionSourceType' "DVB_SUB"

pattern CaptionSourceTypeEmbedded :: CaptionSourceType
pattern CaptionSourceTypeEmbedded = CaptionSourceType' "EMBEDDED"

pattern CaptionSourceTypeSCTE20 :: CaptionSourceType
pattern CaptionSourceTypeSCTE20 = CaptionSourceType' "SCTE20"

pattern CaptionSourceTypeScc :: CaptionSourceType
pattern CaptionSourceTypeScc = CaptionSourceType' "SCC"

pattern CaptionSourceTypeTtml :: CaptionSourceType
pattern CaptionSourceTypeTtml = CaptionSourceType' "TTML"

pattern CaptionSourceTypeStl :: CaptionSourceType
pattern CaptionSourceTypeStl = CaptionSourceType' "STL"

pattern CaptionSourceTypeSrt :: CaptionSourceType
pattern CaptionSourceTypeSrt = CaptionSourceType' "SRT"

pattern CaptionSourceTypeSmi :: CaptionSourceType
pattern CaptionSourceTypeSmi = CaptionSourceType' "SMI"

pattern CaptionSourceTypeTeletext :: CaptionSourceType
pattern CaptionSourceTypeTeletext = CaptionSourceType' "TELETEXT"

pattern CaptionSourceTypeNullSource :: CaptionSourceType
pattern CaptionSourceTypeNullSource = CaptionSourceType' "NULL_SOURCE"

pattern CaptionSourceTypeImsc :: CaptionSourceType
pattern CaptionSourceTypeImsc = CaptionSourceType' "IMSC"

{-# COMPLETE 
  CaptionSourceTypeAncillary,

  CaptionSourceTypeDvbSub,

  CaptionSourceTypeEmbedded,

  CaptionSourceTypeSCTE20,

  CaptionSourceTypeScc,

  CaptionSourceTypeTtml,

  CaptionSourceTypeStl,

  CaptionSourceTypeSrt,

  CaptionSourceTypeSmi,

  CaptionSourceTypeTeletext,

  CaptionSourceTypeNullSource,

  CaptionSourceTypeImsc,
  CaptionSourceType'
  #-}
