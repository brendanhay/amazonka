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
-- Module      : Amazonka.CleanRooms.Types.ScalarFunctions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CleanRooms.Types.ScalarFunctions
  ( ScalarFunctions
      ( ..,
        ScalarFunctions_ABS,
        ScalarFunctions_CAST,
        ScalarFunctions_CEILING,
        ScalarFunctions_COALESCE,
        ScalarFunctions_FLOOR,
        ScalarFunctions_LN,
        ScalarFunctions_LOG,
        ScalarFunctions_LOWER,
        ScalarFunctions_ROUND,
        ScalarFunctions_RTRIM,
        ScalarFunctions_SQRT,
        ScalarFunctions_TRUNC,
        ScalarFunctions_UPPER
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ScalarFunctions = ScalarFunctions'
  { fromScalarFunctions ::
      Data.Text
  }
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

pattern ScalarFunctions_ABS :: ScalarFunctions
pattern ScalarFunctions_ABS = ScalarFunctions' "ABS"

pattern ScalarFunctions_CAST :: ScalarFunctions
pattern ScalarFunctions_CAST = ScalarFunctions' "CAST"

pattern ScalarFunctions_CEILING :: ScalarFunctions
pattern ScalarFunctions_CEILING = ScalarFunctions' "CEILING"

pattern ScalarFunctions_COALESCE :: ScalarFunctions
pattern ScalarFunctions_COALESCE = ScalarFunctions' "COALESCE"

pattern ScalarFunctions_FLOOR :: ScalarFunctions
pattern ScalarFunctions_FLOOR = ScalarFunctions' "FLOOR"

pattern ScalarFunctions_LN :: ScalarFunctions
pattern ScalarFunctions_LN = ScalarFunctions' "LN"

pattern ScalarFunctions_LOG :: ScalarFunctions
pattern ScalarFunctions_LOG = ScalarFunctions' "LOG"

pattern ScalarFunctions_LOWER :: ScalarFunctions
pattern ScalarFunctions_LOWER = ScalarFunctions' "LOWER"

pattern ScalarFunctions_ROUND :: ScalarFunctions
pattern ScalarFunctions_ROUND = ScalarFunctions' "ROUND"

pattern ScalarFunctions_RTRIM :: ScalarFunctions
pattern ScalarFunctions_RTRIM = ScalarFunctions' "RTRIM"

pattern ScalarFunctions_SQRT :: ScalarFunctions
pattern ScalarFunctions_SQRT = ScalarFunctions' "SQRT"

pattern ScalarFunctions_TRUNC :: ScalarFunctions
pattern ScalarFunctions_TRUNC = ScalarFunctions' "TRUNC"

pattern ScalarFunctions_UPPER :: ScalarFunctions
pattern ScalarFunctions_UPPER = ScalarFunctions' "UPPER"

{-# COMPLETE
  ScalarFunctions_ABS,
  ScalarFunctions_CAST,
  ScalarFunctions_CEILING,
  ScalarFunctions_COALESCE,
  ScalarFunctions_FLOOR,
  ScalarFunctions_LN,
  ScalarFunctions_LOG,
  ScalarFunctions_LOWER,
  ScalarFunctions_ROUND,
  ScalarFunctions_RTRIM,
  ScalarFunctions_SQRT,
  ScalarFunctions_TRUNC,
  ScalarFunctions_UPPER,
  ScalarFunctions'
  #-}
