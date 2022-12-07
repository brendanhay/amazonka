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
-- Module      : Amazonka.AppFlow.Types.Operators
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.Operators
  ( Operators
      ( ..,
        Operators_ADDITION,
        Operators_BETWEEN,
        Operators_CONTAINS,
        Operators_DIVISION,
        Operators_EQUAL_TO,
        Operators_GREATER_THAN,
        Operators_GREATER_THAN_OR_EQUAL_TO,
        Operators_LESS_THAN,
        Operators_LESS_THAN_OR_EQUAL_TO,
        Operators_MASK_ALL,
        Operators_MASK_FIRST_N,
        Operators_MASK_LAST_N,
        Operators_MULTIPLICATION,
        Operators_NOT_EQUAL_TO,
        Operators_NO_OP,
        Operators_PROJECTION,
        Operators_SUBTRACTION,
        Operators_VALIDATE_NON_NEGATIVE,
        Operators_VALIDATE_NON_NULL,
        Operators_VALIDATE_NON_ZERO,
        Operators_VALIDATE_NUMERIC
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype Operators = Operators'
  { fromOperators ::
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

pattern Operators_ADDITION :: Operators
pattern Operators_ADDITION = Operators' "ADDITION"

pattern Operators_BETWEEN :: Operators
pattern Operators_BETWEEN = Operators' "BETWEEN"

pattern Operators_CONTAINS :: Operators
pattern Operators_CONTAINS = Operators' "CONTAINS"

pattern Operators_DIVISION :: Operators
pattern Operators_DIVISION = Operators' "DIVISION"

pattern Operators_EQUAL_TO :: Operators
pattern Operators_EQUAL_TO = Operators' "EQUAL_TO"

pattern Operators_GREATER_THAN :: Operators
pattern Operators_GREATER_THAN = Operators' "GREATER_THAN"

pattern Operators_GREATER_THAN_OR_EQUAL_TO :: Operators
pattern Operators_GREATER_THAN_OR_EQUAL_TO = Operators' "GREATER_THAN_OR_EQUAL_TO"

pattern Operators_LESS_THAN :: Operators
pattern Operators_LESS_THAN = Operators' "LESS_THAN"

pattern Operators_LESS_THAN_OR_EQUAL_TO :: Operators
pattern Operators_LESS_THAN_OR_EQUAL_TO = Operators' "LESS_THAN_OR_EQUAL_TO"

pattern Operators_MASK_ALL :: Operators
pattern Operators_MASK_ALL = Operators' "MASK_ALL"

pattern Operators_MASK_FIRST_N :: Operators
pattern Operators_MASK_FIRST_N = Operators' "MASK_FIRST_N"

pattern Operators_MASK_LAST_N :: Operators
pattern Operators_MASK_LAST_N = Operators' "MASK_LAST_N"

pattern Operators_MULTIPLICATION :: Operators
pattern Operators_MULTIPLICATION = Operators' "MULTIPLICATION"

pattern Operators_NOT_EQUAL_TO :: Operators
pattern Operators_NOT_EQUAL_TO = Operators' "NOT_EQUAL_TO"

pattern Operators_NO_OP :: Operators
pattern Operators_NO_OP = Operators' "NO_OP"

pattern Operators_PROJECTION :: Operators
pattern Operators_PROJECTION = Operators' "PROJECTION"

pattern Operators_SUBTRACTION :: Operators
pattern Operators_SUBTRACTION = Operators' "SUBTRACTION"

pattern Operators_VALIDATE_NON_NEGATIVE :: Operators
pattern Operators_VALIDATE_NON_NEGATIVE = Operators' "VALIDATE_NON_NEGATIVE"

pattern Operators_VALIDATE_NON_NULL :: Operators
pattern Operators_VALIDATE_NON_NULL = Operators' "VALIDATE_NON_NULL"

pattern Operators_VALIDATE_NON_ZERO :: Operators
pattern Operators_VALIDATE_NON_ZERO = Operators' "VALIDATE_NON_ZERO"

pattern Operators_VALIDATE_NUMERIC :: Operators
pattern Operators_VALIDATE_NUMERIC = Operators' "VALIDATE_NUMERIC"

{-# COMPLETE
  Operators_ADDITION,
  Operators_BETWEEN,
  Operators_CONTAINS,
  Operators_DIVISION,
  Operators_EQUAL_TO,
  Operators_GREATER_THAN,
  Operators_GREATER_THAN_OR_EQUAL_TO,
  Operators_LESS_THAN,
  Operators_LESS_THAN_OR_EQUAL_TO,
  Operators_MASK_ALL,
  Operators_MASK_FIRST_N,
  Operators_MASK_LAST_N,
  Operators_MULTIPLICATION,
  Operators_NOT_EQUAL_TO,
  Operators_NO_OP,
  Operators_PROJECTION,
  Operators_SUBTRACTION,
  Operators_VALIDATE_NON_NEGATIVE,
  Operators_VALIDATE_NON_NULL,
  Operators_VALIDATE_NON_ZERO,
  Operators_VALIDATE_NUMERIC,
  Operators'
  #-}
