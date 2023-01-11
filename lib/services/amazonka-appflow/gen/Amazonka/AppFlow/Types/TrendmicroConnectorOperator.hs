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
-- Module      : Amazonka.AppFlow.Types.TrendmicroConnectorOperator
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.TrendmicroConnectorOperator
  ( TrendmicroConnectorOperator
      ( ..,
        TrendmicroConnectorOperator_ADDITION,
        TrendmicroConnectorOperator_DIVISION,
        TrendmicroConnectorOperator_EQUAL_TO,
        TrendmicroConnectorOperator_MASK_ALL,
        TrendmicroConnectorOperator_MASK_FIRST_N,
        TrendmicroConnectorOperator_MASK_LAST_N,
        TrendmicroConnectorOperator_MULTIPLICATION,
        TrendmicroConnectorOperator_NO_OP,
        TrendmicroConnectorOperator_PROJECTION,
        TrendmicroConnectorOperator_SUBTRACTION,
        TrendmicroConnectorOperator_VALIDATE_NON_NEGATIVE,
        TrendmicroConnectorOperator_VALIDATE_NON_NULL,
        TrendmicroConnectorOperator_VALIDATE_NON_ZERO,
        TrendmicroConnectorOperator_VALIDATE_NUMERIC
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TrendmicroConnectorOperator = TrendmicroConnectorOperator'
  { fromTrendmicroConnectorOperator ::
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

pattern TrendmicroConnectorOperator_ADDITION :: TrendmicroConnectorOperator
pattern TrendmicroConnectorOperator_ADDITION = TrendmicroConnectorOperator' "ADDITION"

pattern TrendmicroConnectorOperator_DIVISION :: TrendmicroConnectorOperator
pattern TrendmicroConnectorOperator_DIVISION = TrendmicroConnectorOperator' "DIVISION"

pattern TrendmicroConnectorOperator_EQUAL_TO :: TrendmicroConnectorOperator
pattern TrendmicroConnectorOperator_EQUAL_TO = TrendmicroConnectorOperator' "EQUAL_TO"

pattern TrendmicroConnectorOperator_MASK_ALL :: TrendmicroConnectorOperator
pattern TrendmicroConnectorOperator_MASK_ALL = TrendmicroConnectorOperator' "MASK_ALL"

pattern TrendmicroConnectorOperator_MASK_FIRST_N :: TrendmicroConnectorOperator
pattern TrendmicroConnectorOperator_MASK_FIRST_N = TrendmicroConnectorOperator' "MASK_FIRST_N"

pattern TrendmicroConnectorOperator_MASK_LAST_N :: TrendmicroConnectorOperator
pattern TrendmicroConnectorOperator_MASK_LAST_N = TrendmicroConnectorOperator' "MASK_LAST_N"

pattern TrendmicroConnectorOperator_MULTIPLICATION :: TrendmicroConnectorOperator
pattern TrendmicroConnectorOperator_MULTIPLICATION = TrendmicroConnectorOperator' "MULTIPLICATION"

pattern TrendmicroConnectorOperator_NO_OP :: TrendmicroConnectorOperator
pattern TrendmicroConnectorOperator_NO_OP = TrendmicroConnectorOperator' "NO_OP"

pattern TrendmicroConnectorOperator_PROJECTION :: TrendmicroConnectorOperator
pattern TrendmicroConnectorOperator_PROJECTION = TrendmicroConnectorOperator' "PROJECTION"

pattern TrendmicroConnectorOperator_SUBTRACTION :: TrendmicroConnectorOperator
pattern TrendmicroConnectorOperator_SUBTRACTION = TrendmicroConnectorOperator' "SUBTRACTION"

pattern TrendmicroConnectorOperator_VALIDATE_NON_NEGATIVE :: TrendmicroConnectorOperator
pattern TrendmicroConnectorOperator_VALIDATE_NON_NEGATIVE = TrendmicroConnectorOperator' "VALIDATE_NON_NEGATIVE"

pattern TrendmicroConnectorOperator_VALIDATE_NON_NULL :: TrendmicroConnectorOperator
pattern TrendmicroConnectorOperator_VALIDATE_NON_NULL = TrendmicroConnectorOperator' "VALIDATE_NON_NULL"

pattern TrendmicroConnectorOperator_VALIDATE_NON_ZERO :: TrendmicroConnectorOperator
pattern TrendmicroConnectorOperator_VALIDATE_NON_ZERO = TrendmicroConnectorOperator' "VALIDATE_NON_ZERO"

pattern TrendmicroConnectorOperator_VALIDATE_NUMERIC :: TrendmicroConnectorOperator
pattern TrendmicroConnectorOperator_VALIDATE_NUMERIC = TrendmicroConnectorOperator' "VALIDATE_NUMERIC"

{-# COMPLETE
  TrendmicroConnectorOperator_ADDITION,
  TrendmicroConnectorOperator_DIVISION,
  TrendmicroConnectorOperator_EQUAL_TO,
  TrendmicroConnectorOperator_MASK_ALL,
  TrendmicroConnectorOperator_MASK_FIRST_N,
  TrendmicroConnectorOperator_MASK_LAST_N,
  TrendmicroConnectorOperator_MULTIPLICATION,
  TrendmicroConnectorOperator_NO_OP,
  TrendmicroConnectorOperator_PROJECTION,
  TrendmicroConnectorOperator_SUBTRACTION,
  TrendmicroConnectorOperator_VALIDATE_NON_NEGATIVE,
  TrendmicroConnectorOperator_VALIDATE_NON_NULL,
  TrendmicroConnectorOperator_VALIDATE_NON_ZERO,
  TrendmicroConnectorOperator_VALIDATE_NUMERIC,
  TrendmicroConnectorOperator'
  #-}
