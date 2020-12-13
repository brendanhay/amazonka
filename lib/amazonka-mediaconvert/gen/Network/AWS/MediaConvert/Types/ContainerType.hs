{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ContainerType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.ContainerType
  ( ContainerType
      ( ContainerType',
        CTF4V,
        CTIsmv,
        CTM2TS,
        CTM3U8,
        CTCmfc,
        CTMov,
        CTMP4,
        CTMpd,
        CTMxf,
        CTWebm,
        CTRaw
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Container for this output. Some containers require a container settings object. If not specified, the default object will be created.
newtype ContainerType = ContainerType' Lude.Text
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

pattern CTF4V :: ContainerType
pattern CTF4V = ContainerType' "F4V"

pattern CTIsmv :: ContainerType
pattern CTIsmv = ContainerType' "ISMV"

pattern CTM2TS :: ContainerType
pattern CTM2TS = ContainerType' "M2TS"

pattern CTM3U8 :: ContainerType
pattern CTM3U8 = ContainerType' "M3U8"

pattern CTCmfc :: ContainerType
pattern CTCmfc = ContainerType' "CMFC"

pattern CTMov :: ContainerType
pattern CTMov = ContainerType' "MOV"

pattern CTMP4 :: ContainerType
pattern CTMP4 = ContainerType' "MP4"

pattern CTMpd :: ContainerType
pattern CTMpd = ContainerType' "MPD"

pattern CTMxf :: ContainerType
pattern CTMxf = ContainerType' "MXF"

pattern CTWebm :: ContainerType
pattern CTWebm = ContainerType' "WEBM"

pattern CTRaw :: ContainerType
pattern CTRaw = ContainerType' "RAW"

{-# COMPLETE
  CTF4V,
  CTIsmv,
  CTM2TS,
  CTM3U8,
  CTCmfc,
  CTMov,
  CTMP4,
  CTMpd,
  CTMxf,
  CTWebm,
  CTRaw,
  ContainerType'
  #-}
