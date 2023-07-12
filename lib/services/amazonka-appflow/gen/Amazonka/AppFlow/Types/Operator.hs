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
-- Module      : Amazonka.AppFlow.Types.Operator
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.Operator
  ( Operator
      ( ..,
        Operator_ADDITION,
        Operator_BETWEEN,
        Operator_CONTAINS,
        Operator_DIVISION,
        Operator_EQUAL_TO,
        Operator_GREATER_THAN,
        Operator_GREATER_THAN_OR_EQUAL_TO,
        Operator_LESS_THAN,
        Operator_LESS_THAN_OR_EQUAL_TO,
        Operator_MASK_ALL,
        Operator_MASK_FIRST_N,
        Operator_MASK_LAST_N,
        Operator_MULTIPLICATION,
        Operator_NOT_EQUAL_TO,
        Operator_NO_OP,
        Operator_PROJECTION,
        Operator_SUBTRACTION,
        Operator_VALIDATE_NON_NEGATIVE,
        Operator_VALIDATE_NON_NULL,
        Operator_VALIDATE_NON_ZERO,
        Operator_VALIDATE_NUMERIC
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype Operator = Operator'
  { fromOperator ::
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

pattern Operator_ADDITION :: Operator
pattern Operator_ADDITION = Operator' "ADDITION"

pattern Operator_BETWEEN :: Operator
pattern Operator_BETWEEN = Operator' "BETWEEN"

pattern Operator_CONTAINS :: Operator
pattern Operator_CONTAINS = Operator' "CONTAINS"

pattern Operator_DIVISION :: Operator
pattern Operator_DIVISION = Operator' "DIVISION"

pattern Operator_EQUAL_TO :: Operator
pattern Operator_EQUAL_TO = Operator' "EQUAL_TO"

pattern Operator_GREATER_THAN :: Operator
pattern Operator_GREATER_THAN = Operator' "GREATER_THAN"

pattern Operator_GREATER_THAN_OR_EQUAL_TO :: Operator
pattern Operator_GREATER_THAN_OR_EQUAL_TO = Operator' "GREATER_THAN_OR_EQUAL_TO"

pattern Operator_LESS_THAN :: Operator
pattern Operator_LESS_THAN = Operator' "LESS_THAN"

pattern Operator_LESS_THAN_OR_EQUAL_TO :: Operator
pattern Operator_LESS_THAN_OR_EQUAL_TO = Operator' "LESS_THAN_OR_EQUAL_TO"

pattern Operator_MASK_ALL :: Operator
pattern Operator_MASK_ALL = Operator' "MASK_ALL"

pattern Operator_MASK_FIRST_N :: Operator
pattern Operator_MASK_FIRST_N = Operator' "MASK_FIRST_N"

pattern Operator_MASK_LAST_N :: Operator
pattern Operator_MASK_LAST_N = Operator' "MASK_LAST_N"

pattern Operator_MULTIPLICATION :: Operator
pattern Operator_MULTIPLICATION = Operator' "MULTIPLICATION"

pattern Operator_NOT_EQUAL_TO :: Operator
pattern Operator_NOT_EQUAL_TO = Operator' "NOT_EQUAL_TO"

pattern Operator_NO_OP :: Operator
pattern Operator_NO_OP = Operator' "NO_OP"

pattern Operator_PROJECTION :: Operator
pattern Operator_PROJECTION = Operator' "PROJECTION"

pattern Operator_SUBTRACTION :: Operator
pattern Operator_SUBTRACTION = Operator' "SUBTRACTION"

pattern Operator_VALIDATE_NON_NEGATIVE :: Operator
pattern Operator_VALIDATE_NON_NEGATIVE = Operator' "VALIDATE_NON_NEGATIVE"

pattern Operator_VALIDATE_NON_NULL :: Operator
pattern Operator_VALIDATE_NON_NULL = Operator' "VALIDATE_NON_NULL"

pattern Operator_VALIDATE_NON_ZERO :: Operator
pattern Operator_VALIDATE_NON_ZERO = Operator' "VALIDATE_NON_ZERO"

pattern Operator_VALIDATE_NUMERIC :: Operator
pattern Operator_VALIDATE_NUMERIC = Operator' "VALIDATE_NUMERIC"

{-# COMPLETE
  Operator_ADDITION,
  Operator_BETWEEN,
  Operator_CONTAINS,
  Operator_DIVISION,
  Operator_EQUAL_TO,
  Operator_GREATER_THAN,
  Operator_GREATER_THAN_OR_EQUAL_TO,
  Operator_LESS_THAN,
  Operator_LESS_THAN_OR_EQUAL_TO,
  Operator_MASK_ALL,
  Operator_MASK_FIRST_N,
  Operator_MASK_LAST_N,
  Operator_MULTIPLICATION,
  Operator_NOT_EQUAL_TO,
  Operator_NO_OP,
  Operator_PROJECTION,
  Operator_SUBTRACTION,
  Operator_VALIDATE_NON_NEGATIVE,
  Operator_VALIDATE_NON_NULL,
  Operator_VALIDATE_NON_ZERO,
  Operator_VALIDATE_NUMERIC,
  Operator'
  #-}
