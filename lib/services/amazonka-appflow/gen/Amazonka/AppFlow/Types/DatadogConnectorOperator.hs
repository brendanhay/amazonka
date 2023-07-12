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
-- Module      : Amazonka.AppFlow.Types.DatadogConnectorOperator
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.DatadogConnectorOperator
  ( DatadogConnectorOperator
      ( ..,
        DatadogConnectorOperator_ADDITION,
        DatadogConnectorOperator_BETWEEN,
        DatadogConnectorOperator_DIVISION,
        DatadogConnectorOperator_EQUAL_TO,
        DatadogConnectorOperator_MASK_ALL,
        DatadogConnectorOperator_MASK_FIRST_N,
        DatadogConnectorOperator_MASK_LAST_N,
        DatadogConnectorOperator_MULTIPLICATION,
        DatadogConnectorOperator_NO_OP,
        DatadogConnectorOperator_PROJECTION,
        DatadogConnectorOperator_SUBTRACTION,
        DatadogConnectorOperator_VALIDATE_NON_NEGATIVE,
        DatadogConnectorOperator_VALIDATE_NON_NULL,
        DatadogConnectorOperator_VALIDATE_NON_ZERO,
        DatadogConnectorOperator_VALIDATE_NUMERIC
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DatadogConnectorOperator = DatadogConnectorOperator'
  { fromDatadogConnectorOperator ::
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

pattern DatadogConnectorOperator_ADDITION :: DatadogConnectorOperator
pattern DatadogConnectorOperator_ADDITION = DatadogConnectorOperator' "ADDITION"

pattern DatadogConnectorOperator_BETWEEN :: DatadogConnectorOperator
pattern DatadogConnectorOperator_BETWEEN = DatadogConnectorOperator' "BETWEEN"

pattern DatadogConnectorOperator_DIVISION :: DatadogConnectorOperator
pattern DatadogConnectorOperator_DIVISION = DatadogConnectorOperator' "DIVISION"

pattern DatadogConnectorOperator_EQUAL_TO :: DatadogConnectorOperator
pattern DatadogConnectorOperator_EQUAL_TO = DatadogConnectorOperator' "EQUAL_TO"

pattern DatadogConnectorOperator_MASK_ALL :: DatadogConnectorOperator
pattern DatadogConnectorOperator_MASK_ALL = DatadogConnectorOperator' "MASK_ALL"

pattern DatadogConnectorOperator_MASK_FIRST_N :: DatadogConnectorOperator
pattern DatadogConnectorOperator_MASK_FIRST_N = DatadogConnectorOperator' "MASK_FIRST_N"

pattern DatadogConnectorOperator_MASK_LAST_N :: DatadogConnectorOperator
pattern DatadogConnectorOperator_MASK_LAST_N = DatadogConnectorOperator' "MASK_LAST_N"

pattern DatadogConnectorOperator_MULTIPLICATION :: DatadogConnectorOperator
pattern DatadogConnectorOperator_MULTIPLICATION = DatadogConnectorOperator' "MULTIPLICATION"

pattern DatadogConnectorOperator_NO_OP :: DatadogConnectorOperator
pattern DatadogConnectorOperator_NO_OP = DatadogConnectorOperator' "NO_OP"

pattern DatadogConnectorOperator_PROJECTION :: DatadogConnectorOperator
pattern DatadogConnectorOperator_PROJECTION = DatadogConnectorOperator' "PROJECTION"

pattern DatadogConnectorOperator_SUBTRACTION :: DatadogConnectorOperator
pattern DatadogConnectorOperator_SUBTRACTION = DatadogConnectorOperator' "SUBTRACTION"

pattern DatadogConnectorOperator_VALIDATE_NON_NEGATIVE :: DatadogConnectorOperator
pattern DatadogConnectorOperator_VALIDATE_NON_NEGATIVE = DatadogConnectorOperator' "VALIDATE_NON_NEGATIVE"

pattern DatadogConnectorOperator_VALIDATE_NON_NULL :: DatadogConnectorOperator
pattern DatadogConnectorOperator_VALIDATE_NON_NULL = DatadogConnectorOperator' "VALIDATE_NON_NULL"

pattern DatadogConnectorOperator_VALIDATE_NON_ZERO :: DatadogConnectorOperator
pattern DatadogConnectorOperator_VALIDATE_NON_ZERO = DatadogConnectorOperator' "VALIDATE_NON_ZERO"

pattern DatadogConnectorOperator_VALIDATE_NUMERIC :: DatadogConnectorOperator
pattern DatadogConnectorOperator_VALIDATE_NUMERIC = DatadogConnectorOperator' "VALIDATE_NUMERIC"

{-# COMPLETE
  DatadogConnectorOperator_ADDITION,
  DatadogConnectorOperator_BETWEEN,
  DatadogConnectorOperator_DIVISION,
  DatadogConnectorOperator_EQUAL_TO,
  DatadogConnectorOperator_MASK_ALL,
  DatadogConnectorOperator_MASK_FIRST_N,
  DatadogConnectorOperator_MASK_LAST_N,
  DatadogConnectorOperator_MULTIPLICATION,
  DatadogConnectorOperator_NO_OP,
  DatadogConnectorOperator_PROJECTION,
  DatadogConnectorOperator_SUBTRACTION,
  DatadogConnectorOperator_VALIDATE_NON_NEGATIVE,
  DatadogConnectorOperator_VALIDATE_NON_NULL,
  DatadogConnectorOperator_VALIDATE_NON_ZERO,
  DatadogConnectorOperator_VALIDATE_NUMERIC,
  DatadogConnectorOperator'
  #-}
