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
-- Module      : Amazonka.AppFlow.Types.MarketoConnectorOperator
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.MarketoConnectorOperator
  ( MarketoConnectorOperator
      ( ..,
        MarketoConnectorOperator_ADDITION,
        MarketoConnectorOperator_BETWEEN,
        MarketoConnectorOperator_DIVISION,
        MarketoConnectorOperator_GREATER_THAN,
        MarketoConnectorOperator_LESS_THAN,
        MarketoConnectorOperator_MASK_ALL,
        MarketoConnectorOperator_MASK_FIRST_N,
        MarketoConnectorOperator_MASK_LAST_N,
        MarketoConnectorOperator_MULTIPLICATION,
        MarketoConnectorOperator_NO_OP,
        MarketoConnectorOperator_PROJECTION,
        MarketoConnectorOperator_SUBTRACTION,
        MarketoConnectorOperator_VALIDATE_NON_NEGATIVE,
        MarketoConnectorOperator_VALIDATE_NON_NULL,
        MarketoConnectorOperator_VALIDATE_NON_ZERO,
        MarketoConnectorOperator_VALIDATE_NUMERIC
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype MarketoConnectorOperator = MarketoConnectorOperator'
  { fromMarketoConnectorOperator ::
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

pattern MarketoConnectorOperator_ADDITION :: MarketoConnectorOperator
pattern MarketoConnectorOperator_ADDITION = MarketoConnectorOperator' "ADDITION"

pattern MarketoConnectorOperator_BETWEEN :: MarketoConnectorOperator
pattern MarketoConnectorOperator_BETWEEN = MarketoConnectorOperator' "BETWEEN"

pattern MarketoConnectorOperator_DIVISION :: MarketoConnectorOperator
pattern MarketoConnectorOperator_DIVISION = MarketoConnectorOperator' "DIVISION"

pattern MarketoConnectorOperator_GREATER_THAN :: MarketoConnectorOperator
pattern MarketoConnectorOperator_GREATER_THAN = MarketoConnectorOperator' "GREATER_THAN"

pattern MarketoConnectorOperator_LESS_THAN :: MarketoConnectorOperator
pattern MarketoConnectorOperator_LESS_THAN = MarketoConnectorOperator' "LESS_THAN"

pattern MarketoConnectorOperator_MASK_ALL :: MarketoConnectorOperator
pattern MarketoConnectorOperator_MASK_ALL = MarketoConnectorOperator' "MASK_ALL"

pattern MarketoConnectorOperator_MASK_FIRST_N :: MarketoConnectorOperator
pattern MarketoConnectorOperator_MASK_FIRST_N = MarketoConnectorOperator' "MASK_FIRST_N"

pattern MarketoConnectorOperator_MASK_LAST_N :: MarketoConnectorOperator
pattern MarketoConnectorOperator_MASK_LAST_N = MarketoConnectorOperator' "MASK_LAST_N"

pattern MarketoConnectorOperator_MULTIPLICATION :: MarketoConnectorOperator
pattern MarketoConnectorOperator_MULTIPLICATION = MarketoConnectorOperator' "MULTIPLICATION"

pattern MarketoConnectorOperator_NO_OP :: MarketoConnectorOperator
pattern MarketoConnectorOperator_NO_OP = MarketoConnectorOperator' "NO_OP"

pattern MarketoConnectorOperator_PROJECTION :: MarketoConnectorOperator
pattern MarketoConnectorOperator_PROJECTION = MarketoConnectorOperator' "PROJECTION"

pattern MarketoConnectorOperator_SUBTRACTION :: MarketoConnectorOperator
pattern MarketoConnectorOperator_SUBTRACTION = MarketoConnectorOperator' "SUBTRACTION"

pattern MarketoConnectorOperator_VALIDATE_NON_NEGATIVE :: MarketoConnectorOperator
pattern MarketoConnectorOperator_VALIDATE_NON_NEGATIVE = MarketoConnectorOperator' "VALIDATE_NON_NEGATIVE"

pattern MarketoConnectorOperator_VALIDATE_NON_NULL :: MarketoConnectorOperator
pattern MarketoConnectorOperator_VALIDATE_NON_NULL = MarketoConnectorOperator' "VALIDATE_NON_NULL"

pattern MarketoConnectorOperator_VALIDATE_NON_ZERO :: MarketoConnectorOperator
pattern MarketoConnectorOperator_VALIDATE_NON_ZERO = MarketoConnectorOperator' "VALIDATE_NON_ZERO"

pattern MarketoConnectorOperator_VALIDATE_NUMERIC :: MarketoConnectorOperator
pattern MarketoConnectorOperator_VALIDATE_NUMERIC = MarketoConnectorOperator' "VALIDATE_NUMERIC"

{-# COMPLETE
  MarketoConnectorOperator_ADDITION,
  MarketoConnectorOperator_BETWEEN,
  MarketoConnectorOperator_DIVISION,
  MarketoConnectorOperator_GREATER_THAN,
  MarketoConnectorOperator_LESS_THAN,
  MarketoConnectorOperator_MASK_ALL,
  MarketoConnectorOperator_MASK_FIRST_N,
  MarketoConnectorOperator_MASK_LAST_N,
  MarketoConnectorOperator_MULTIPLICATION,
  MarketoConnectorOperator_NO_OP,
  MarketoConnectorOperator_PROJECTION,
  MarketoConnectorOperator_SUBTRACTION,
  MarketoConnectorOperator_VALIDATE_NON_NEGATIVE,
  MarketoConnectorOperator_VALIDATE_NON_NULL,
  MarketoConnectorOperator_VALIDATE_NON_ZERO,
  MarketoConnectorOperator_VALIDATE_NUMERIC,
  MarketoConnectorOperator'
  #-}
