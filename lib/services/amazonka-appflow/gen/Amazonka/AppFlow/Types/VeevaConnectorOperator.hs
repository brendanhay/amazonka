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
-- Module      : Amazonka.AppFlow.Types.VeevaConnectorOperator
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.VeevaConnectorOperator
  ( VeevaConnectorOperator
      ( ..,
        VeevaConnectorOperator_ADDITION,
        VeevaConnectorOperator_BETWEEN,
        VeevaConnectorOperator_CONTAINS,
        VeevaConnectorOperator_DIVISION,
        VeevaConnectorOperator_EQUAL_TO,
        VeevaConnectorOperator_GREATER_THAN,
        VeevaConnectorOperator_GREATER_THAN_OR_EQUAL_TO,
        VeevaConnectorOperator_LESS_THAN,
        VeevaConnectorOperator_LESS_THAN_OR_EQUAL_TO,
        VeevaConnectorOperator_MASK_ALL,
        VeevaConnectorOperator_MASK_FIRST_N,
        VeevaConnectorOperator_MASK_LAST_N,
        VeevaConnectorOperator_MULTIPLICATION,
        VeevaConnectorOperator_NOT_EQUAL_TO,
        VeevaConnectorOperator_NO_OP,
        VeevaConnectorOperator_PROJECTION,
        VeevaConnectorOperator_SUBTRACTION,
        VeevaConnectorOperator_VALIDATE_NON_NEGATIVE,
        VeevaConnectorOperator_VALIDATE_NON_NULL,
        VeevaConnectorOperator_VALIDATE_NON_ZERO,
        VeevaConnectorOperator_VALIDATE_NUMERIC
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype VeevaConnectorOperator = VeevaConnectorOperator'
  { fromVeevaConnectorOperator ::
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

pattern VeevaConnectorOperator_ADDITION :: VeevaConnectorOperator
pattern VeevaConnectorOperator_ADDITION = VeevaConnectorOperator' "ADDITION"

pattern VeevaConnectorOperator_BETWEEN :: VeevaConnectorOperator
pattern VeevaConnectorOperator_BETWEEN = VeevaConnectorOperator' "BETWEEN"

pattern VeevaConnectorOperator_CONTAINS :: VeevaConnectorOperator
pattern VeevaConnectorOperator_CONTAINS = VeevaConnectorOperator' "CONTAINS"

pattern VeevaConnectorOperator_DIVISION :: VeevaConnectorOperator
pattern VeevaConnectorOperator_DIVISION = VeevaConnectorOperator' "DIVISION"

pattern VeevaConnectorOperator_EQUAL_TO :: VeevaConnectorOperator
pattern VeevaConnectorOperator_EQUAL_TO = VeevaConnectorOperator' "EQUAL_TO"

pattern VeevaConnectorOperator_GREATER_THAN :: VeevaConnectorOperator
pattern VeevaConnectorOperator_GREATER_THAN = VeevaConnectorOperator' "GREATER_THAN"

pattern VeevaConnectorOperator_GREATER_THAN_OR_EQUAL_TO :: VeevaConnectorOperator
pattern VeevaConnectorOperator_GREATER_THAN_OR_EQUAL_TO = VeevaConnectorOperator' "GREATER_THAN_OR_EQUAL_TO"

pattern VeevaConnectorOperator_LESS_THAN :: VeevaConnectorOperator
pattern VeevaConnectorOperator_LESS_THAN = VeevaConnectorOperator' "LESS_THAN"

pattern VeevaConnectorOperator_LESS_THAN_OR_EQUAL_TO :: VeevaConnectorOperator
pattern VeevaConnectorOperator_LESS_THAN_OR_EQUAL_TO = VeevaConnectorOperator' "LESS_THAN_OR_EQUAL_TO"

pattern VeevaConnectorOperator_MASK_ALL :: VeevaConnectorOperator
pattern VeevaConnectorOperator_MASK_ALL = VeevaConnectorOperator' "MASK_ALL"

pattern VeevaConnectorOperator_MASK_FIRST_N :: VeevaConnectorOperator
pattern VeevaConnectorOperator_MASK_FIRST_N = VeevaConnectorOperator' "MASK_FIRST_N"

pattern VeevaConnectorOperator_MASK_LAST_N :: VeevaConnectorOperator
pattern VeevaConnectorOperator_MASK_LAST_N = VeevaConnectorOperator' "MASK_LAST_N"

pattern VeevaConnectorOperator_MULTIPLICATION :: VeevaConnectorOperator
pattern VeevaConnectorOperator_MULTIPLICATION = VeevaConnectorOperator' "MULTIPLICATION"

pattern VeevaConnectorOperator_NOT_EQUAL_TO :: VeevaConnectorOperator
pattern VeevaConnectorOperator_NOT_EQUAL_TO = VeevaConnectorOperator' "NOT_EQUAL_TO"

pattern VeevaConnectorOperator_NO_OP :: VeevaConnectorOperator
pattern VeevaConnectorOperator_NO_OP = VeevaConnectorOperator' "NO_OP"

pattern VeevaConnectorOperator_PROJECTION :: VeevaConnectorOperator
pattern VeevaConnectorOperator_PROJECTION = VeevaConnectorOperator' "PROJECTION"

pattern VeevaConnectorOperator_SUBTRACTION :: VeevaConnectorOperator
pattern VeevaConnectorOperator_SUBTRACTION = VeevaConnectorOperator' "SUBTRACTION"

pattern VeevaConnectorOperator_VALIDATE_NON_NEGATIVE :: VeevaConnectorOperator
pattern VeevaConnectorOperator_VALIDATE_NON_NEGATIVE = VeevaConnectorOperator' "VALIDATE_NON_NEGATIVE"

pattern VeevaConnectorOperator_VALIDATE_NON_NULL :: VeevaConnectorOperator
pattern VeevaConnectorOperator_VALIDATE_NON_NULL = VeevaConnectorOperator' "VALIDATE_NON_NULL"

pattern VeevaConnectorOperator_VALIDATE_NON_ZERO :: VeevaConnectorOperator
pattern VeevaConnectorOperator_VALIDATE_NON_ZERO = VeevaConnectorOperator' "VALIDATE_NON_ZERO"

pattern VeevaConnectorOperator_VALIDATE_NUMERIC :: VeevaConnectorOperator
pattern VeevaConnectorOperator_VALIDATE_NUMERIC = VeevaConnectorOperator' "VALIDATE_NUMERIC"

{-# COMPLETE
  VeevaConnectorOperator_ADDITION,
  VeevaConnectorOperator_BETWEEN,
  VeevaConnectorOperator_CONTAINS,
  VeevaConnectorOperator_DIVISION,
  VeevaConnectorOperator_EQUAL_TO,
  VeevaConnectorOperator_GREATER_THAN,
  VeevaConnectorOperator_GREATER_THAN_OR_EQUAL_TO,
  VeevaConnectorOperator_LESS_THAN,
  VeevaConnectorOperator_LESS_THAN_OR_EQUAL_TO,
  VeevaConnectorOperator_MASK_ALL,
  VeevaConnectorOperator_MASK_FIRST_N,
  VeevaConnectorOperator_MASK_LAST_N,
  VeevaConnectorOperator_MULTIPLICATION,
  VeevaConnectorOperator_NOT_EQUAL_TO,
  VeevaConnectorOperator_NO_OP,
  VeevaConnectorOperator_PROJECTION,
  VeevaConnectorOperator_SUBTRACTION,
  VeevaConnectorOperator_VALIDATE_NON_NEGATIVE,
  VeevaConnectorOperator_VALIDATE_NON_NULL,
  VeevaConnectorOperator_VALIDATE_NON_ZERO,
  VeevaConnectorOperator_VALIDATE_NUMERIC,
  VeevaConnectorOperator'
  #-}
