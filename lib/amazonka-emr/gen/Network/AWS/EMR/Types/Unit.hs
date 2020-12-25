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
        UnitNone,
        UnitSeconds,
        UnitMicroSeconds,
        UnitMilliSeconds,
        UnitBytes,
        UnitKiloBytes,
        UnitMegaBytes,
        UnitGigaBytes,
        UnitTeraBytes,
        UnitBits,
        UnitKiloBits,
        UnitMegaBits,
        UnitGigaBits,
        UnitTeraBits,
        UnitPercent,
        UnitCount,
        UnitBytesPerSecond,
        UnitKiloBytesPerSecond,
        UnitMegaBytesPerSecond,
        UnitGigaBytesPerSecond,
        UnitTeraBytesPerSecond,
        UnitBitsPerSecond,
        UnitKiloBitsPerSecond,
        UnitMegaBitsPerSecond,
        UnitGigaBitsPerSecond,
        UnitTeraBitsPerSecond,
        UnitCountPerSecond,
        fromUnit
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype Unit = Unit' {fromUnit :: Core.Text}
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

pattern UnitNone :: Unit
pattern UnitNone = Unit' "NONE"

pattern UnitSeconds :: Unit
pattern UnitSeconds = Unit' "SECONDS"

pattern UnitMicroSeconds :: Unit
pattern UnitMicroSeconds = Unit' "MICRO_SECONDS"

pattern UnitMilliSeconds :: Unit
pattern UnitMilliSeconds = Unit' "MILLI_SECONDS"

pattern UnitBytes :: Unit
pattern UnitBytes = Unit' "BYTES"

pattern UnitKiloBytes :: Unit
pattern UnitKiloBytes = Unit' "KILO_BYTES"

pattern UnitMegaBytes :: Unit
pattern UnitMegaBytes = Unit' "MEGA_BYTES"

pattern UnitGigaBytes :: Unit
pattern UnitGigaBytes = Unit' "GIGA_BYTES"

pattern UnitTeraBytes :: Unit
pattern UnitTeraBytes = Unit' "TERA_BYTES"

pattern UnitBits :: Unit
pattern UnitBits = Unit' "BITS"

pattern UnitKiloBits :: Unit
pattern UnitKiloBits = Unit' "KILO_BITS"

pattern UnitMegaBits :: Unit
pattern UnitMegaBits = Unit' "MEGA_BITS"

pattern UnitGigaBits :: Unit
pattern UnitGigaBits = Unit' "GIGA_BITS"

pattern UnitTeraBits :: Unit
pattern UnitTeraBits = Unit' "TERA_BITS"

pattern UnitPercent :: Unit
pattern UnitPercent = Unit' "PERCENT"

pattern UnitCount :: Unit
pattern UnitCount = Unit' "COUNT"

pattern UnitBytesPerSecond :: Unit
pattern UnitBytesPerSecond = Unit' "BYTES_PER_SECOND"

pattern UnitKiloBytesPerSecond :: Unit
pattern UnitKiloBytesPerSecond = Unit' "KILO_BYTES_PER_SECOND"

pattern UnitMegaBytesPerSecond :: Unit
pattern UnitMegaBytesPerSecond = Unit' "MEGA_BYTES_PER_SECOND"

pattern UnitGigaBytesPerSecond :: Unit
pattern UnitGigaBytesPerSecond = Unit' "GIGA_BYTES_PER_SECOND"

pattern UnitTeraBytesPerSecond :: Unit
pattern UnitTeraBytesPerSecond = Unit' "TERA_BYTES_PER_SECOND"

pattern UnitBitsPerSecond :: Unit
pattern UnitBitsPerSecond = Unit' "BITS_PER_SECOND"

pattern UnitKiloBitsPerSecond :: Unit
pattern UnitKiloBitsPerSecond = Unit' "KILO_BITS_PER_SECOND"

pattern UnitMegaBitsPerSecond :: Unit
pattern UnitMegaBitsPerSecond = Unit' "MEGA_BITS_PER_SECOND"

pattern UnitGigaBitsPerSecond :: Unit
pattern UnitGigaBitsPerSecond = Unit' "GIGA_BITS_PER_SECOND"

pattern UnitTeraBitsPerSecond :: Unit
pattern UnitTeraBitsPerSecond = Unit' "TERA_BITS_PER_SECOND"

pattern UnitCountPerSecond :: Unit
pattern UnitCountPerSecond = Unit' "COUNT_PER_SECOND"

{-# COMPLETE
  UnitNone,
  UnitSeconds,
  UnitMicroSeconds,
  UnitMilliSeconds,
  UnitBytes,
  UnitKiloBytes,
  UnitMegaBytes,
  UnitGigaBytes,
  UnitTeraBytes,
  UnitBits,
  UnitKiloBits,
  UnitMegaBits,
  UnitGigaBits,
  UnitTeraBits,
  UnitPercent,
  UnitCount,
  UnitBytesPerSecond,
  UnitKiloBytesPerSecond,
  UnitMegaBytesPerSecond,
  UnitGigaBytesPerSecond,
  UnitTeraBytesPerSecond,
  UnitBitsPerSecond,
  UnitKiloBitsPerSecond,
  UnitMegaBitsPerSecond,
  UnitGigaBitsPerSecond,
  UnitTeraBitsPerSecond,
  UnitCountPerSecond,
  Unit'
  #-}
