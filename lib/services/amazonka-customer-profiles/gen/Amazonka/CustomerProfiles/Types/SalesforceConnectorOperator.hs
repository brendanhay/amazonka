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
-- Module      : Amazonka.CustomerProfiles.Types.SalesforceConnectorOperator
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.SalesforceConnectorOperator
  ( SalesforceConnectorOperator
      ( ..,
        SalesforceConnectorOperator_ADDITION,
        SalesforceConnectorOperator_BETWEEN,
        SalesforceConnectorOperator_CONTAINS,
        SalesforceConnectorOperator_DIVISION,
        SalesforceConnectorOperator_EQUAL_TO,
        SalesforceConnectorOperator_GREATER_THAN,
        SalesforceConnectorOperator_GREATER_THAN_OR_EQUAL_TO,
        SalesforceConnectorOperator_LESS_THAN,
        SalesforceConnectorOperator_LESS_THAN_OR_EQUAL_TO,
        SalesforceConnectorOperator_MASK_ALL,
        SalesforceConnectorOperator_MASK_FIRST_N,
        SalesforceConnectorOperator_MASK_LAST_N,
        SalesforceConnectorOperator_MULTIPLICATION,
        SalesforceConnectorOperator_NOT_EQUAL_TO,
        SalesforceConnectorOperator_NO_OP,
        SalesforceConnectorOperator_PROJECTION,
        SalesforceConnectorOperator_SUBTRACTION,
        SalesforceConnectorOperator_VALIDATE_NON_NEGATIVE,
        SalesforceConnectorOperator_VALIDATE_NON_NULL,
        SalesforceConnectorOperator_VALIDATE_NON_ZERO,
        SalesforceConnectorOperator_VALIDATE_NUMERIC
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SalesforceConnectorOperator = SalesforceConnectorOperator'
  { fromSalesforceConnectorOperator ::
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

pattern SalesforceConnectorOperator_ADDITION :: SalesforceConnectorOperator
pattern SalesforceConnectorOperator_ADDITION = SalesforceConnectorOperator' "ADDITION"

pattern SalesforceConnectorOperator_BETWEEN :: SalesforceConnectorOperator
pattern SalesforceConnectorOperator_BETWEEN = SalesforceConnectorOperator' "BETWEEN"

pattern SalesforceConnectorOperator_CONTAINS :: SalesforceConnectorOperator
pattern SalesforceConnectorOperator_CONTAINS = SalesforceConnectorOperator' "CONTAINS"

pattern SalesforceConnectorOperator_DIVISION :: SalesforceConnectorOperator
pattern SalesforceConnectorOperator_DIVISION = SalesforceConnectorOperator' "DIVISION"

pattern SalesforceConnectorOperator_EQUAL_TO :: SalesforceConnectorOperator
pattern SalesforceConnectorOperator_EQUAL_TO = SalesforceConnectorOperator' "EQUAL_TO"

pattern SalesforceConnectorOperator_GREATER_THAN :: SalesforceConnectorOperator
pattern SalesforceConnectorOperator_GREATER_THAN = SalesforceConnectorOperator' "GREATER_THAN"

pattern SalesforceConnectorOperator_GREATER_THAN_OR_EQUAL_TO :: SalesforceConnectorOperator
pattern SalesforceConnectorOperator_GREATER_THAN_OR_EQUAL_TO = SalesforceConnectorOperator' "GREATER_THAN_OR_EQUAL_TO"

pattern SalesforceConnectorOperator_LESS_THAN :: SalesforceConnectorOperator
pattern SalesforceConnectorOperator_LESS_THAN = SalesforceConnectorOperator' "LESS_THAN"

pattern SalesforceConnectorOperator_LESS_THAN_OR_EQUAL_TO :: SalesforceConnectorOperator
pattern SalesforceConnectorOperator_LESS_THAN_OR_EQUAL_TO = SalesforceConnectorOperator' "LESS_THAN_OR_EQUAL_TO"

pattern SalesforceConnectorOperator_MASK_ALL :: SalesforceConnectorOperator
pattern SalesforceConnectorOperator_MASK_ALL = SalesforceConnectorOperator' "MASK_ALL"

pattern SalesforceConnectorOperator_MASK_FIRST_N :: SalesforceConnectorOperator
pattern SalesforceConnectorOperator_MASK_FIRST_N = SalesforceConnectorOperator' "MASK_FIRST_N"

pattern SalesforceConnectorOperator_MASK_LAST_N :: SalesforceConnectorOperator
pattern SalesforceConnectorOperator_MASK_LAST_N = SalesforceConnectorOperator' "MASK_LAST_N"

pattern SalesforceConnectorOperator_MULTIPLICATION :: SalesforceConnectorOperator
pattern SalesforceConnectorOperator_MULTIPLICATION = SalesforceConnectorOperator' "MULTIPLICATION"

pattern SalesforceConnectorOperator_NOT_EQUAL_TO :: SalesforceConnectorOperator
pattern SalesforceConnectorOperator_NOT_EQUAL_TO = SalesforceConnectorOperator' "NOT_EQUAL_TO"

pattern SalesforceConnectorOperator_NO_OP :: SalesforceConnectorOperator
pattern SalesforceConnectorOperator_NO_OP = SalesforceConnectorOperator' "NO_OP"

pattern SalesforceConnectorOperator_PROJECTION :: SalesforceConnectorOperator
pattern SalesforceConnectorOperator_PROJECTION = SalesforceConnectorOperator' "PROJECTION"

pattern SalesforceConnectorOperator_SUBTRACTION :: SalesforceConnectorOperator
pattern SalesforceConnectorOperator_SUBTRACTION = SalesforceConnectorOperator' "SUBTRACTION"

pattern SalesforceConnectorOperator_VALIDATE_NON_NEGATIVE :: SalesforceConnectorOperator
pattern SalesforceConnectorOperator_VALIDATE_NON_NEGATIVE = SalesforceConnectorOperator' "VALIDATE_NON_NEGATIVE"

pattern SalesforceConnectorOperator_VALIDATE_NON_NULL :: SalesforceConnectorOperator
pattern SalesforceConnectorOperator_VALIDATE_NON_NULL = SalesforceConnectorOperator' "VALIDATE_NON_NULL"

pattern SalesforceConnectorOperator_VALIDATE_NON_ZERO :: SalesforceConnectorOperator
pattern SalesforceConnectorOperator_VALIDATE_NON_ZERO = SalesforceConnectorOperator' "VALIDATE_NON_ZERO"

pattern SalesforceConnectorOperator_VALIDATE_NUMERIC :: SalesforceConnectorOperator
pattern SalesforceConnectorOperator_VALIDATE_NUMERIC = SalesforceConnectorOperator' "VALIDATE_NUMERIC"

{-# COMPLETE
  SalesforceConnectorOperator_ADDITION,
  SalesforceConnectorOperator_BETWEEN,
  SalesforceConnectorOperator_CONTAINS,
  SalesforceConnectorOperator_DIVISION,
  SalesforceConnectorOperator_EQUAL_TO,
  SalesforceConnectorOperator_GREATER_THAN,
  SalesforceConnectorOperator_GREATER_THAN_OR_EQUAL_TO,
  SalesforceConnectorOperator_LESS_THAN,
  SalesforceConnectorOperator_LESS_THAN_OR_EQUAL_TO,
  SalesforceConnectorOperator_MASK_ALL,
  SalesforceConnectorOperator_MASK_FIRST_N,
  SalesforceConnectorOperator_MASK_LAST_N,
  SalesforceConnectorOperator_MULTIPLICATION,
  SalesforceConnectorOperator_NOT_EQUAL_TO,
  SalesforceConnectorOperator_NO_OP,
  SalesforceConnectorOperator_PROJECTION,
  SalesforceConnectorOperator_SUBTRACTION,
  SalesforceConnectorOperator_VALIDATE_NON_NEGATIVE,
  SalesforceConnectorOperator_VALIDATE_NON_NULL,
  SalesforceConnectorOperator_VALIDATE_NON_ZERO,
  SalesforceConnectorOperator_VALIDATE_NUMERIC,
  SalesforceConnectorOperator'
  #-}
