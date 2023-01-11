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
-- Module      : Amazonka.AppFlow.Types.SAPODataConnectorOperator
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.SAPODataConnectorOperator
  ( SAPODataConnectorOperator
      ( ..,
        SAPODataConnectorOperator_ADDITION,
        SAPODataConnectorOperator_BETWEEN,
        SAPODataConnectorOperator_CONTAINS,
        SAPODataConnectorOperator_DIVISION,
        SAPODataConnectorOperator_EQUAL_TO,
        SAPODataConnectorOperator_GREATER_THAN,
        SAPODataConnectorOperator_GREATER_THAN_OR_EQUAL_TO,
        SAPODataConnectorOperator_LESS_THAN,
        SAPODataConnectorOperator_LESS_THAN_OR_EQUAL_TO,
        SAPODataConnectorOperator_MASK_ALL,
        SAPODataConnectorOperator_MASK_FIRST_N,
        SAPODataConnectorOperator_MASK_LAST_N,
        SAPODataConnectorOperator_MULTIPLICATION,
        SAPODataConnectorOperator_NOT_EQUAL_TO,
        SAPODataConnectorOperator_NO_OP,
        SAPODataConnectorOperator_PROJECTION,
        SAPODataConnectorOperator_SUBTRACTION,
        SAPODataConnectorOperator_VALIDATE_NON_NEGATIVE,
        SAPODataConnectorOperator_VALIDATE_NON_NULL,
        SAPODataConnectorOperator_VALIDATE_NON_ZERO,
        SAPODataConnectorOperator_VALIDATE_NUMERIC
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SAPODataConnectorOperator = SAPODataConnectorOperator'
  { fromSAPODataConnectorOperator ::
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

pattern SAPODataConnectorOperator_ADDITION :: SAPODataConnectorOperator
pattern SAPODataConnectorOperator_ADDITION = SAPODataConnectorOperator' "ADDITION"

pattern SAPODataConnectorOperator_BETWEEN :: SAPODataConnectorOperator
pattern SAPODataConnectorOperator_BETWEEN = SAPODataConnectorOperator' "BETWEEN"

pattern SAPODataConnectorOperator_CONTAINS :: SAPODataConnectorOperator
pattern SAPODataConnectorOperator_CONTAINS = SAPODataConnectorOperator' "CONTAINS"

pattern SAPODataConnectorOperator_DIVISION :: SAPODataConnectorOperator
pattern SAPODataConnectorOperator_DIVISION = SAPODataConnectorOperator' "DIVISION"

pattern SAPODataConnectorOperator_EQUAL_TO :: SAPODataConnectorOperator
pattern SAPODataConnectorOperator_EQUAL_TO = SAPODataConnectorOperator' "EQUAL_TO"

pattern SAPODataConnectorOperator_GREATER_THAN :: SAPODataConnectorOperator
pattern SAPODataConnectorOperator_GREATER_THAN = SAPODataConnectorOperator' "GREATER_THAN"

pattern SAPODataConnectorOperator_GREATER_THAN_OR_EQUAL_TO :: SAPODataConnectorOperator
pattern SAPODataConnectorOperator_GREATER_THAN_OR_EQUAL_TO = SAPODataConnectorOperator' "GREATER_THAN_OR_EQUAL_TO"

pattern SAPODataConnectorOperator_LESS_THAN :: SAPODataConnectorOperator
pattern SAPODataConnectorOperator_LESS_THAN = SAPODataConnectorOperator' "LESS_THAN"

pattern SAPODataConnectorOperator_LESS_THAN_OR_EQUAL_TO :: SAPODataConnectorOperator
pattern SAPODataConnectorOperator_LESS_THAN_OR_EQUAL_TO = SAPODataConnectorOperator' "LESS_THAN_OR_EQUAL_TO"

pattern SAPODataConnectorOperator_MASK_ALL :: SAPODataConnectorOperator
pattern SAPODataConnectorOperator_MASK_ALL = SAPODataConnectorOperator' "MASK_ALL"

pattern SAPODataConnectorOperator_MASK_FIRST_N :: SAPODataConnectorOperator
pattern SAPODataConnectorOperator_MASK_FIRST_N = SAPODataConnectorOperator' "MASK_FIRST_N"

pattern SAPODataConnectorOperator_MASK_LAST_N :: SAPODataConnectorOperator
pattern SAPODataConnectorOperator_MASK_LAST_N = SAPODataConnectorOperator' "MASK_LAST_N"

pattern SAPODataConnectorOperator_MULTIPLICATION :: SAPODataConnectorOperator
pattern SAPODataConnectorOperator_MULTIPLICATION = SAPODataConnectorOperator' "MULTIPLICATION"

pattern SAPODataConnectorOperator_NOT_EQUAL_TO :: SAPODataConnectorOperator
pattern SAPODataConnectorOperator_NOT_EQUAL_TO = SAPODataConnectorOperator' "NOT_EQUAL_TO"

pattern SAPODataConnectorOperator_NO_OP :: SAPODataConnectorOperator
pattern SAPODataConnectorOperator_NO_OP = SAPODataConnectorOperator' "NO_OP"

pattern SAPODataConnectorOperator_PROJECTION :: SAPODataConnectorOperator
pattern SAPODataConnectorOperator_PROJECTION = SAPODataConnectorOperator' "PROJECTION"

pattern SAPODataConnectorOperator_SUBTRACTION :: SAPODataConnectorOperator
pattern SAPODataConnectorOperator_SUBTRACTION = SAPODataConnectorOperator' "SUBTRACTION"

pattern SAPODataConnectorOperator_VALIDATE_NON_NEGATIVE :: SAPODataConnectorOperator
pattern SAPODataConnectorOperator_VALIDATE_NON_NEGATIVE = SAPODataConnectorOperator' "VALIDATE_NON_NEGATIVE"

pattern SAPODataConnectorOperator_VALIDATE_NON_NULL :: SAPODataConnectorOperator
pattern SAPODataConnectorOperator_VALIDATE_NON_NULL = SAPODataConnectorOperator' "VALIDATE_NON_NULL"

pattern SAPODataConnectorOperator_VALIDATE_NON_ZERO :: SAPODataConnectorOperator
pattern SAPODataConnectorOperator_VALIDATE_NON_ZERO = SAPODataConnectorOperator' "VALIDATE_NON_ZERO"

pattern SAPODataConnectorOperator_VALIDATE_NUMERIC :: SAPODataConnectorOperator
pattern SAPODataConnectorOperator_VALIDATE_NUMERIC = SAPODataConnectorOperator' "VALIDATE_NUMERIC"

{-# COMPLETE
  SAPODataConnectorOperator_ADDITION,
  SAPODataConnectorOperator_BETWEEN,
  SAPODataConnectorOperator_CONTAINS,
  SAPODataConnectorOperator_DIVISION,
  SAPODataConnectorOperator_EQUAL_TO,
  SAPODataConnectorOperator_GREATER_THAN,
  SAPODataConnectorOperator_GREATER_THAN_OR_EQUAL_TO,
  SAPODataConnectorOperator_LESS_THAN,
  SAPODataConnectorOperator_LESS_THAN_OR_EQUAL_TO,
  SAPODataConnectorOperator_MASK_ALL,
  SAPODataConnectorOperator_MASK_FIRST_N,
  SAPODataConnectorOperator_MASK_LAST_N,
  SAPODataConnectorOperator_MULTIPLICATION,
  SAPODataConnectorOperator_NOT_EQUAL_TO,
  SAPODataConnectorOperator_NO_OP,
  SAPODataConnectorOperator_PROJECTION,
  SAPODataConnectorOperator_SUBTRACTION,
  SAPODataConnectorOperator_VALIDATE_NON_NEGATIVE,
  SAPODataConnectorOperator_VALIDATE_NON_NULL,
  SAPODataConnectorOperator_VALIDATE_NON_ZERO,
  SAPODataConnectorOperator_VALIDATE_NUMERIC,
  SAPODataConnectorOperator'
  #-}
