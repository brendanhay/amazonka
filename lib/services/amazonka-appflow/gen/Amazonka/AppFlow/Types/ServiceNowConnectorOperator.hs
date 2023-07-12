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
-- Module      : Amazonka.AppFlow.Types.ServiceNowConnectorOperator
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.ServiceNowConnectorOperator
  ( ServiceNowConnectorOperator
      ( ..,
        ServiceNowConnectorOperator_ADDITION,
        ServiceNowConnectorOperator_BETWEEN,
        ServiceNowConnectorOperator_CONTAINS,
        ServiceNowConnectorOperator_DIVISION,
        ServiceNowConnectorOperator_EQUAL_TO,
        ServiceNowConnectorOperator_GREATER_THAN,
        ServiceNowConnectorOperator_GREATER_THAN_OR_EQUAL_TO,
        ServiceNowConnectorOperator_LESS_THAN,
        ServiceNowConnectorOperator_LESS_THAN_OR_EQUAL_TO,
        ServiceNowConnectorOperator_MASK_ALL,
        ServiceNowConnectorOperator_MASK_FIRST_N,
        ServiceNowConnectorOperator_MASK_LAST_N,
        ServiceNowConnectorOperator_MULTIPLICATION,
        ServiceNowConnectorOperator_NOT_EQUAL_TO,
        ServiceNowConnectorOperator_NO_OP,
        ServiceNowConnectorOperator_PROJECTION,
        ServiceNowConnectorOperator_SUBTRACTION,
        ServiceNowConnectorOperator_VALIDATE_NON_NEGATIVE,
        ServiceNowConnectorOperator_VALIDATE_NON_NULL,
        ServiceNowConnectorOperator_VALIDATE_NON_ZERO,
        ServiceNowConnectorOperator_VALIDATE_NUMERIC
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ServiceNowConnectorOperator = ServiceNowConnectorOperator'
  { fromServiceNowConnectorOperator ::
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

pattern ServiceNowConnectorOperator_ADDITION :: ServiceNowConnectorOperator
pattern ServiceNowConnectorOperator_ADDITION = ServiceNowConnectorOperator' "ADDITION"

pattern ServiceNowConnectorOperator_BETWEEN :: ServiceNowConnectorOperator
pattern ServiceNowConnectorOperator_BETWEEN = ServiceNowConnectorOperator' "BETWEEN"

pattern ServiceNowConnectorOperator_CONTAINS :: ServiceNowConnectorOperator
pattern ServiceNowConnectorOperator_CONTAINS = ServiceNowConnectorOperator' "CONTAINS"

pattern ServiceNowConnectorOperator_DIVISION :: ServiceNowConnectorOperator
pattern ServiceNowConnectorOperator_DIVISION = ServiceNowConnectorOperator' "DIVISION"

pattern ServiceNowConnectorOperator_EQUAL_TO :: ServiceNowConnectorOperator
pattern ServiceNowConnectorOperator_EQUAL_TO = ServiceNowConnectorOperator' "EQUAL_TO"

pattern ServiceNowConnectorOperator_GREATER_THAN :: ServiceNowConnectorOperator
pattern ServiceNowConnectorOperator_GREATER_THAN = ServiceNowConnectorOperator' "GREATER_THAN"

pattern ServiceNowConnectorOperator_GREATER_THAN_OR_EQUAL_TO :: ServiceNowConnectorOperator
pattern ServiceNowConnectorOperator_GREATER_THAN_OR_EQUAL_TO = ServiceNowConnectorOperator' "GREATER_THAN_OR_EQUAL_TO"

pattern ServiceNowConnectorOperator_LESS_THAN :: ServiceNowConnectorOperator
pattern ServiceNowConnectorOperator_LESS_THAN = ServiceNowConnectorOperator' "LESS_THAN"

pattern ServiceNowConnectorOperator_LESS_THAN_OR_EQUAL_TO :: ServiceNowConnectorOperator
pattern ServiceNowConnectorOperator_LESS_THAN_OR_EQUAL_TO = ServiceNowConnectorOperator' "LESS_THAN_OR_EQUAL_TO"

pattern ServiceNowConnectorOperator_MASK_ALL :: ServiceNowConnectorOperator
pattern ServiceNowConnectorOperator_MASK_ALL = ServiceNowConnectorOperator' "MASK_ALL"

pattern ServiceNowConnectorOperator_MASK_FIRST_N :: ServiceNowConnectorOperator
pattern ServiceNowConnectorOperator_MASK_FIRST_N = ServiceNowConnectorOperator' "MASK_FIRST_N"

pattern ServiceNowConnectorOperator_MASK_LAST_N :: ServiceNowConnectorOperator
pattern ServiceNowConnectorOperator_MASK_LAST_N = ServiceNowConnectorOperator' "MASK_LAST_N"

pattern ServiceNowConnectorOperator_MULTIPLICATION :: ServiceNowConnectorOperator
pattern ServiceNowConnectorOperator_MULTIPLICATION = ServiceNowConnectorOperator' "MULTIPLICATION"

pattern ServiceNowConnectorOperator_NOT_EQUAL_TO :: ServiceNowConnectorOperator
pattern ServiceNowConnectorOperator_NOT_EQUAL_TO = ServiceNowConnectorOperator' "NOT_EQUAL_TO"

pattern ServiceNowConnectorOperator_NO_OP :: ServiceNowConnectorOperator
pattern ServiceNowConnectorOperator_NO_OP = ServiceNowConnectorOperator' "NO_OP"

pattern ServiceNowConnectorOperator_PROJECTION :: ServiceNowConnectorOperator
pattern ServiceNowConnectorOperator_PROJECTION = ServiceNowConnectorOperator' "PROJECTION"

pattern ServiceNowConnectorOperator_SUBTRACTION :: ServiceNowConnectorOperator
pattern ServiceNowConnectorOperator_SUBTRACTION = ServiceNowConnectorOperator' "SUBTRACTION"

pattern ServiceNowConnectorOperator_VALIDATE_NON_NEGATIVE :: ServiceNowConnectorOperator
pattern ServiceNowConnectorOperator_VALIDATE_NON_NEGATIVE = ServiceNowConnectorOperator' "VALIDATE_NON_NEGATIVE"

pattern ServiceNowConnectorOperator_VALIDATE_NON_NULL :: ServiceNowConnectorOperator
pattern ServiceNowConnectorOperator_VALIDATE_NON_NULL = ServiceNowConnectorOperator' "VALIDATE_NON_NULL"

pattern ServiceNowConnectorOperator_VALIDATE_NON_ZERO :: ServiceNowConnectorOperator
pattern ServiceNowConnectorOperator_VALIDATE_NON_ZERO = ServiceNowConnectorOperator' "VALIDATE_NON_ZERO"

pattern ServiceNowConnectorOperator_VALIDATE_NUMERIC :: ServiceNowConnectorOperator
pattern ServiceNowConnectorOperator_VALIDATE_NUMERIC = ServiceNowConnectorOperator' "VALIDATE_NUMERIC"

{-# COMPLETE
  ServiceNowConnectorOperator_ADDITION,
  ServiceNowConnectorOperator_BETWEEN,
  ServiceNowConnectorOperator_CONTAINS,
  ServiceNowConnectorOperator_DIVISION,
  ServiceNowConnectorOperator_EQUAL_TO,
  ServiceNowConnectorOperator_GREATER_THAN,
  ServiceNowConnectorOperator_GREATER_THAN_OR_EQUAL_TO,
  ServiceNowConnectorOperator_LESS_THAN,
  ServiceNowConnectorOperator_LESS_THAN_OR_EQUAL_TO,
  ServiceNowConnectorOperator_MASK_ALL,
  ServiceNowConnectorOperator_MASK_FIRST_N,
  ServiceNowConnectorOperator_MASK_LAST_N,
  ServiceNowConnectorOperator_MULTIPLICATION,
  ServiceNowConnectorOperator_NOT_EQUAL_TO,
  ServiceNowConnectorOperator_NO_OP,
  ServiceNowConnectorOperator_PROJECTION,
  ServiceNowConnectorOperator_SUBTRACTION,
  ServiceNowConnectorOperator_VALIDATE_NON_NEGATIVE,
  ServiceNowConnectorOperator_VALIDATE_NON_NULL,
  ServiceNowConnectorOperator_VALIDATE_NON_ZERO,
  ServiceNowConnectorOperator_VALIDATE_NUMERIC,
  ServiceNowConnectorOperator'
  #-}
