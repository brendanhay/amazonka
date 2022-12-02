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
-- Module      : Amazonka.AppFlow.Types.DynatraceConnectorOperator
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.DynatraceConnectorOperator
  ( DynatraceConnectorOperator
      ( ..,
        DynatraceConnectorOperator_ADDITION,
        DynatraceConnectorOperator_BETWEEN,
        DynatraceConnectorOperator_DIVISION,
        DynatraceConnectorOperator_EQUAL_TO,
        DynatraceConnectorOperator_MASK_ALL,
        DynatraceConnectorOperator_MASK_FIRST_N,
        DynatraceConnectorOperator_MASK_LAST_N,
        DynatraceConnectorOperator_MULTIPLICATION,
        DynatraceConnectorOperator_NO_OP,
        DynatraceConnectorOperator_PROJECTION,
        DynatraceConnectorOperator_SUBTRACTION,
        DynatraceConnectorOperator_VALIDATE_NON_NEGATIVE,
        DynatraceConnectorOperator_VALIDATE_NON_NULL,
        DynatraceConnectorOperator_VALIDATE_NON_ZERO,
        DynatraceConnectorOperator_VALIDATE_NUMERIC
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DynatraceConnectorOperator = DynatraceConnectorOperator'
  { fromDynatraceConnectorOperator ::
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

pattern DynatraceConnectorOperator_ADDITION :: DynatraceConnectorOperator
pattern DynatraceConnectorOperator_ADDITION = DynatraceConnectorOperator' "ADDITION"

pattern DynatraceConnectorOperator_BETWEEN :: DynatraceConnectorOperator
pattern DynatraceConnectorOperator_BETWEEN = DynatraceConnectorOperator' "BETWEEN"

pattern DynatraceConnectorOperator_DIVISION :: DynatraceConnectorOperator
pattern DynatraceConnectorOperator_DIVISION = DynatraceConnectorOperator' "DIVISION"

pattern DynatraceConnectorOperator_EQUAL_TO :: DynatraceConnectorOperator
pattern DynatraceConnectorOperator_EQUAL_TO = DynatraceConnectorOperator' "EQUAL_TO"

pattern DynatraceConnectorOperator_MASK_ALL :: DynatraceConnectorOperator
pattern DynatraceConnectorOperator_MASK_ALL = DynatraceConnectorOperator' "MASK_ALL"

pattern DynatraceConnectorOperator_MASK_FIRST_N :: DynatraceConnectorOperator
pattern DynatraceConnectorOperator_MASK_FIRST_N = DynatraceConnectorOperator' "MASK_FIRST_N"

pattern DynatraceConnectorOperator_MASK_LAST_N :: DynatraceConnectorOperator
pattern DynatraceConnectorOperator_MASK_LAST_N = DynatraceConnectorOperator' "MASK_LAST_N"

pattern DynatraceConnectorOperator_MULTIPLICATION :: DynatraceConnectorOperator
pattern DynatraceConnectorOperator_MULTIPLICATION = DynatraceConnectorOperator' "MULTIPLICATION"

pattern DynatraceConnectorOperator_NO_OP :: DynatraceConnectorOperator
pattern DynatraceConnectorOperator_NO_OP = DynatraceConnectorOperator' "NO_OP"

pattern DynatraceConnectorOperator_PROJECTION :: DynatraceConnectorOperator
pattern DynatraceConnectorOperator_PROJECTION = DynatraceConnectorOperator' "PROJECTION"

pattern DynatraceConnectorOperator_SUBTRACTION :: DynatraceConnectorOperator
pattern DynatraceConnectorOperator_SUBTRACTION = DynatraceConnectorOperator' "SUBTRACTION"

pattern DynatraceConnectorOperator_VALIDATE_NON_NEGATIVE :: DynatraceConnectorOperator
pattern DynatraceConnectorOperator_VALIDATE_NON_NEGATIVE = DynatraceConnectorOperator' "VALIDATE_NON_NEGATIVE"

pattern DynatraceConnectorOperator_VALIDATE_NON_NULL :: DynatraceConnectorOperator
pattern DynatraceConnectorOperator_VALIDATE_NON_NULL = DynatraceConnectorOperator' "VALIDATE_NON_NULL"

pattern DynatraceConnectorOperator_VALIDATE_NON_ZERO :: DynatraceConnectorOperator
pattern DynatraceConnectorOperator_VALIDATE_NON_ZERO = DynatraceConnectorOperator' "VALIDATE_NON_ZERO"

pattern DynatraceConnectorOperator_VALIDATE_NUMERIC :: DynatraceConnectorOperator
pattern DynatraceConnectorOperator_VALIDATE_NUMERIC = DynatraceConnectorOperator' "VALIDATE_NUMERIC"

{-# COMPLETE
  DynatraceConnectorOperator_ADDITION,
  DynatraceConnectorOperator_BETWEEN,
  DynatraceConnectorOperator_DIVISION,
  DynatraceConnectorOperator_EQUAL_TO,
  DynatraceConnectorOperator_MASK_ALL,
  DynatraceConnectorOperator_MASK_FIRST_N,
  DynatraceConnectorOperator_MASK_LAST_N,
  DynatraceConnectorOperator_MULTIPLICATION,
  DynatraceConnectorOperator_NO_OP,
  DynatraceConnectorOperator_PROJECTION,
  DynatraceConnectorOperator_SUBTRACTION,
  DynatraceConnectorOperator_VALIDATE_NON_NEGATIVE,
  DynatraceConnectorOperator_VALIDATE_NON_NULL,
  DynatraceConnectorOperator_VALIDATE_NON_ZERO,
  DynatraceConnectorOperator_VALIDATE_NUMERIC,
  DynatraceConnectorOperator'
  #-}
