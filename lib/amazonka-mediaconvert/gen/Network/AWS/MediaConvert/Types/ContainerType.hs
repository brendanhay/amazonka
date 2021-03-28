{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ContainerType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.ContainerType
  ( ContainerType
    ( ContainerType'
    , ContainerTypeF4V
    , ContainerTypeIsmv
    , ContainerTypeM2TS
    , ContainerTypeM3U8
    , ContainerTypeCmfc
    , ContainerTypeMov
    , ContainerTypeMP4
    , ContainerTypeMpd
    , ContainerTypeMxf
    , ContainerTypeWebm
    , ContainerTypeRaw
    , fromContainerType
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Container for this output. Some containers require a container settings object. If not specified, the default object will be created.
newtype ContainerType = ContainerType'{fromContainerType ::
                                       Core.Text}
                          deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                          Core.Generic)
                          deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                            Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                            Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                            Core.FromText, Core.ToByteString, Core.ToQuery,
                                            Core.ToHeader)

pattern ContainerTypeF4V :: ContainerType
pattern ContainerTypeF4V = ContainerType' "F4V"

pattern ContainerTypeIsmv :: ContainerType
pattern ContainerTypeIsmv = ContainerType' "ISMV"

pattern ContainerTypeM2TS :: ContainerType
pattern ContainerTypeM2TS = ContainerType' "M2TS"

pattern ContainerTypeM3U8 :: ContainerType
pattern ContainerTypeM3U8 = ContainerType' "M3U8"

pattern ContainerTypeCmfc :: ContainerType
pattern ContainerTypeCmfc = ContainerType' "CMFC"

pattern ContainerTypeMov :: ContainerType
pattern ContainerTypeMov = ContainerType' "MOV"

pattern ContainerTypeMP4 :: ContainerType
pattern ContainerTypeMP4 = ContainerType' "MP4"

pattern ContainerTypeMpd :: ContainerType
pattern ContainerTypeMpd = ContainerType' "MPD"

pattern ContainerTypeMxf :: ContainerType
pattern ContainerTypeMxf = ContainerType' "MXF"

pattern ContainerTypeWebm :: ContainerType
pattern ContainerTypeWebm = ContainerType' "WEBM"

pattern ContainerTypeRaw :: ContainerType
pattern ContainerTypeRaw = ContainerType' "RAW"

{-# COMPLETE 
  ContainerTypeF4V,

  ContainerTypeIsmv,

  ContainerTypeM2TS,

  ContainerTypeM3U8,

  ContainerTypeCmfc,

  ContainerTypeMov,

  ContainerTypeMP4,

  ContainerTypeMpd,

  ContainerTypeMxf,

  ContainerTypeWebm,

  ContainerTypeRaw,
  ContainerType'
  #-}
