{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.Unit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.Unit
  ( Unit
      ( Unit',
        None,
        Seconds,
        MicroSeconds,
        MilliSeconds,
        Bytes,
        KiloBytes,
        MegaBytes,
        GigaBytes,
        TeraBytes,
        Bits,
        KiloBits,
        MegaBits,
        GigaBits,
        TeraBits,
        Percent,
        Count,
        BytesPerSecond,
        KiloBytesPerSecond,
        MegaBytesPerSecond,
        GigaBytesPerSecond,
        TeraBytesPerSecond,
        BitsPerSecond,
        KiloBitsPerSecond,
        MegaBitsPerSecond,
        GigaBitsPerSecond,
        TeraBitsPerSecond,
        CountPerSecond
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype Unit = Unit' Lude.Text
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

pattern None :: Unit
pattern None = Unit' "NONE"

pattern Seconds :: Unit
pattern Seconds = Unit' "SECONDS"

pattern MicroSeconds :: Unit
pattern MicroSeconds = Unit' "MICRO_SECONDS"

pattern MilliSeconds :: Unit
pattern MilliSeconds = Unit' "MILLI_SECONDS"

pattern Bytes :: Unit
pattern Bytes = Unit' "BYTES"

pattern KiloBytes :: Unit
pattern KiloBytes = Unit' "KILO_BYTES"

pattern MegaBytes :: Unit
pattern MegaBytes = Unit' "MEGA_BYTES"

pattern GigaBytes :: Unit
pattern GigaBytes = Unit' "GIGA_BYTES"

pattern TeraBytes :: Unit
pattern TeraBytes = Unit' "TERA_BYTES"

pattern Bits :: Unit
pattern Bits = Unit' "BITS"

pattern KiloBits :: Unit
pattern KiloBits = Unit' "KILO_BITS"

pattern MegaBits :: Unit
pattern MegaBits = Unit' "MEGA_BITS"

pattern GigaBits :: Unit
pattern GigaBits = Unit' "GIGA_BITS"

pattern TeraBits :: Unit
pattern TeraBits = Unit' "TERA_BITS"

pattern Percent :: Unit
pattern Percent = Unit' "PERCENT"

pattern Count :: Unit
pattern Count = Unit' "COUNT"

pattern BytesPerSecond :: Unit
pattern BytesPerSecond = Unit' "BYTES_PER_SECOND"

pattern KiloBytesPerSecond :: Unit
pattern KiloBytesPerSecond = Unit' "KILO_BYTES_PER_SECOND"

pattern MegaBytesPerSecond :: Unit
pattern MegaBytesPerSecond = Unit' "MEGA_BYTES_PER_SECOND"

pattern GigaBytesPerSecond :: Unit
pattern GigaBytesPerSecond = Unit' "GIGA_BYTES_PER_SECOND"

pattern TeraBytesPerSecond :: Unit
pattern TeraBytesPerSecond = Unit' "TERA_BYTES_PER_SECOND"

pattern BitsPerSecond :: Unit
pattern BitsPerSecond = Unit' "BITS_PER_SECOND"

pattern KiloBitsPerSecond :: Unit
pattern KiloBitsPerSecond = Unit' "KILO_BITS_PER_SECOND"

pattern MegaBitsPerSecond :: Unit
pattern MegaBitsPerSecond = Unit' "MEGA_BITS_PER_SECOND"

pattern GigaBitsPerSecond :: Unit
pattern GigaBitsPerSecond = Unit' "GIGA_BITS_PER_SECOND"

pattern TeraBitsPerSecond :: Unit
pattern TeraBitsPerSecond = Unit' "TERA_BITS_PER_SECOND"

pattern CountPerSecond :: Unit
pattern CountPerSecond = Unit' "COUNT_PER_SECOND"

{-# COMPLETE
  None,
  Seconds,
  MicroSeconds,
  MilliSeconds,
  Bytes,
  KiloBytes,
  MegaBytes,
  GigaBytes,
  TeraBytes,
  Bits,
  KiloBits,
  MegaBits,
  GigaBits,
  TeraBits,
  Percent,
  Count,
  BytesPerSecond,
  KiloBytesPerSecond,
  MegaBytesPerSecond,
  GigaBytesPerSecond,
  TeraBytesPerSecond,
  BitsPerSecond,
  KiloBitsPerSecond,
  MegaBitsPerSecond,
  GigaBitsPerSecond,
  TeraBitsPerSecond,
  CountPerSecond,
  Unit'
  #-}
