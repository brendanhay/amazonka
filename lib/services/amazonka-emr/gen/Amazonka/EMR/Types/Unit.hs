{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EMR.Types.Unit
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.Unit
  ( Unit
      ( ..,
        Unit_BITS,
        Unit_BITS_PER_SECOND,
        Unit_BYTES,
        Unit_BYTES_PER_SECOND,
        Unit_COUNT,
        Unit_COUNT_PER_SECOND,
        Unit_GIGA_BITS,
        Unit_GIGA_BITS_PER_SECOND,
        Unit_GIGA_BYTES,
        Unit_GIGA_BYTES_PER_SECOND,
        Unit_KILO_BITS,
        Unit_KILO_BITS_PER_SECOND,
        Unit_KILO_BYTES,
        Unit_KILO_BYTES_PER_SECOND,
        Unit_MEGA_BITS,
        Unit_MEGA_BITS_PER_SECOND,
        Unit_MEGA_BYTES,
        Unit_MEGA_BYTES_PER_SECOND,
        Unit_MICRO_SECONDS,
        Unit_MILLI_SECONDS,
        Unit_NONE,
        Unit_PERCENT,
        Unit_SECONDS,
        Unit_TERA_BITS,
        Unit_TERA_BITS_PER_SECOND,
        Unit_TERA_BYTES,
        Unit_TERA_BYTES_PER_SECOND
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype Unit = Unit' {fromUnit :: Data.Text}
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern Unit_BITS :: Unit
pattern Unit_BITS = Unit' "BITS"

pattern Unit_BITS_PER_SECOND :: Unit
pattern Unit_BITS_PER_SECOND = Unit' "BITS_PER_SECOND"

pattern Unit_BYTES :: Unit
pattern Unit_BYTES = Unit' "BYTES"

pattern Unit_BYTES_PER_SECOND :: Unit
pattern Unit_BYTES_PER_SECOND = Unit' "BYTES_PER_SECOND"

pattern Unit_COUNT :: Unit
pattern Unit_COUNT = Unit' "COUNT"

pattern Unit_COUNT_PER_SECOND :: Unit
pattern Unit_COUNT_PER_SECOND = Unit' "COUNT_PER_SECOND"

pattern Unit_GIGA_BITS :: Unit
pattern Unit_GIGA_BITS = Unit' "GIGA_BITS"

pattern Unit_GIGA_BITS_PER_SECOND :: Unit
pattern Unit_GIGA_BITS_PER_SECOND = Unit' "GIGA_BITS_PER_SECOND"

pattern Unit_GIGA_BYTES :: Unit
pattern Unit_GIGA_BYTES = Unit' "GIGA_BYTES"

pattern Unit_GIGA_BYTES_PER_SECOND :: Unit
pattern Unit_GIGA_BYTES_PER_SECOND = Unit' "GIGA_BYTES_PER_SECOND"

pattern Unit_KILO_BITS :: Unit
pattern Unit_KILO_BITS = Unit' "KILO_BITS"

pattern Unit_KILO_BITS_PER_SECOND :: Unit
pattern Unit_KILO_BITS_PER_SECOND = Unit' "KILO_BITS_PER_SECOND"

pattern Unit_KILO_BYTES :: Unit
pattern Unit_KILO_BYTES = Unit' "KILO_BYTES"

pattern Unit_KILO_BYTES_PER_SECOND :: Unit
pattern Unit_KILO_BYTES_PER_SECOND = Unit' "KILO_BYTES_PER_SECOND"

pattern Unit_MEGA_BITS :: Unit
pattern Unit_MEGA_BITS = Unit' "MEGA_BITS"

pattern Unit_MEGA_BITS_PER_SECOND :: Unit
pattern Unit_MEGA_BITS_PER_SECOND = Unit' "MEGA_BITS_PER_SECOND"

pattern Unit_MEGA_BYTES :: Unit
pattern Unit_MEGA_BYTES = Unit' "MEGA_BYTES"

pattern Unit_MEGA_BYTES_PER_SECOND :: Unit
pattern Unit_MEGA_BYTES_PER_SECOND = Unit' "MEGA_BYTES_PER_SECOND"

pattern Unit_MICRO_SECONDS :: Unit
pattern Unit_MICRO_SECONDS = Unit' "MICRO_SECONDS"

pattern Unit_MILLI_SECONDS :: Unit
pattern Unit_MILLI_SECONDS = Unit' "MILLI_SECONDS"

pattern Unit_NONE :: Unit
pattern Unit_NONE = Unit' "NONE"

pattern Unit_PERCENT :: Unit
pattern Unit_PERCENT = Unit' "PERCENT"

pattern Unit_SECONDS :: Unit
pattern Unit_SECONDS = Unit' "SECONDS"

pattern Unit_TERA_BITS :: Unit
pattern Unit_TERA_BITS = Unit' "TERA_BITS"

pattern Unit_TERA_BITS_PER_SECOND :: Unit
pattern Unit_TERA_BITS_PER_SECOND = Unit' "TERA_BITS_PER_SECOND"

pattern Unit_TERA_BYTES :: Unit
pattern Unit_TERA_BYTES = Unit' "TERA_BYTES"

pattern Unit_TERA_BYTES_PER_SECOND :: Unit
pattern Unit_TERA_BYTES_PER_SECOND = Unit' "TERA_BYTES_PER_SECOND"

{-# COMPLETE
  Unit_BITS,
  Unit_BITS_PER_SECOND,
  Unit_BYTES,
  Unit_BYTES_PER_SECOND,
  Unit_COUNT,
  Unit_COUNT_PER_SECOND,
  Unit_GIGA_BITS,
  Unit_GIGA_BITS_PER_SECOND,
  Unit_GIGA_BYTES,
  Unit_GIGA_BYTES_PER_SECOND,
  Unit_KILO_BITS,
  Unit_KILO_BITS_PER_SECOND,
  Unit_KILO_BYTES,
  Unit_KILO_BYTES_PER_SECOND,
  Unit_MEGA_BITS,
  Unit_MEGA_BITS_PER_SECOND,
  Unit_MEGA_BYTES,
  Unit_MEGA_BYTES_PER_SECOND,
  Unit_MICRO_SECONDS,
  Unit_MILLI_SECONDS,
  Unit_NONE,
  Unit_PERCENT,
  Unit_SECONDS,
  Unit_TERA_BITS,
  Unit_TERA_BITS_PER_SECOND,
  Unit_TERA_BYTES,
  Unit_TERA_BYTES_PER_SECOND,
  Unit'
  #-}
