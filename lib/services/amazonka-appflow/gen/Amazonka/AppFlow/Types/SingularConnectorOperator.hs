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
-- Module      : Amazonka.AppFlow.Types.SingularConnectorOperator
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.SingularConnectorOperator
  ( SingularConnectorOperator
      ( ..,
        SingularConnectorOperator_ADDITION,
        SingularConnectorOperator_DIVISION,
        SingularConnectorOperator_EQUAL_TO,
        SingularConnectorOperator_MASK_ALL,
        SingularConnectorOperator_MASK_FIRST_N,
        SingularConnectorOperator_MASK_LAST_N,
        SingularConnectorOperator_MULTIPLICATION,
        SingularConnectorOperator_NO_OP,
        SingularConnectorOperator_PROJECTION,
        SingularConnectorOperator_SUBTRACTION,
        SingularConnectorOperator_VALIDATE_NON_NEGATIVE,
        SingularConnectorOperator_VALIDATE_NON_NULL,
        SingularConnectorOperator_VALIDATE_NON_ZERO,
        SingularConnectorOperator_VALIDATE_NUMERIC
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SingularConnectorOperator = SingularConnectorOperator'
  { fromSingularConnectorOperator ::
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

pattern SingularConnectorOperator_ADDITION :: SingularConnectorOperator
pattern SingularConnectorOperator_ADDITION = SingularConnectorOperator' "ADDITION"

pattern SingularConnectorOperator_DIVISION :: SingularConnectorOperator
pattern SingularConnectorOperator_DIVISION = SingularConnectorOperator' "DIVISION"

pattern SingularConnectorOperator_EQUAL_TO :: SingularConnectorOperator
pattern SingularConnectorOperator_EQUAL_TO = SingularConnectorOperator' "EQUAL_TO"

pattern SingularConnectorOperator_MASK_ALL :: SingularConnectorOperator
pattern SingularConnectorOperator_MASK_ALL = SingularConnectorOperator' "MASK_ALL"

pattern SingularConnectorOperator_MASK_FIRST_N :: SingularConnectorOperator
pattern SingularConnectorOperator_MASK_FIRST_N = SingularConnectorOperator' "MASK_FIRST_N"

pattern SingularConnectorOperator_MASK_LAST_N :: SingularConnectorOperator
pattern SingularConnectorOperator_MASK_LAST_N = SingularConnectorOperator' "MASK_LAST_N"

pattern SingularConnectorOperator_MULTIPLICATION :: SingularConnectorOperator
pattern SingularConnectorOperator_MULTIPLICATION = SingularConnectorOperator' "MULTIPLICATION"

pattern SingularConnectorOperator_NO_OP :: SingularConnectorOperator
pattern SingularConnectorOperator_NO_OP = SingularConnectorOperator' "NO_OP"

pattern SingularConnectorOperator_PROJECTION :: SingularConnectorOperator
pattern SingularConnectorOperator_PROJECTION = SingularConnectorOperator' "PROJECTION"

pattern SingularConnectorOperator_SUBTRACTION :: SingularConnectorOperator
pattern SingularConnectorOperator_SUBTRACTION = SingularConnectorOperator' "SUBTRACTION"

pattern SingularConnectorOperator_VALIDATE_NON_NEGATIVE :: SingularConnectorOperator
pattern SingularConnectorOperator_VALIDATE_NON_NEGATIVE = SingularConnectorOperator' "VALIDATE_NON_NEGATIVE"

pattern SingularConnectorOperator_VALIDATE_NON_NULL :: SingularConnectorOperator
pattern SingularConnectorOperator_VALIDATE_NON_NULL = SingularConnectorOperator' "VALIDATE_NON_NULL"

pattern SingularConnectorOperator_VALIDATE_NON_ZERO :: SingularConnectorOperator
pattern SingularConnectorOperator_VALIDATE_NON_ZERO = SingularConnectorOperator' "VALIDATE_NON_ZERO"

pattern SingularConnectorOperator_VALIDATE_NUMERIC :: SingularConnectorOperator
pattern SingularConnectorOperator_VALIDATE_NUMERIC = SingularConnectorOperator' "VALIDATE_NUMERIC"

{-# COMPLETE
  SingularConnectorOperator_ADDITION,
  SingularConnectorOperator_DIVISION,
  SingularConnectorOperator_EQUAL_TO,
  SingularConnectorOperator_MASK_ALL,
  SingularConnectorOperator_MASK_FIRST_N,
  SingularConnectorOperator_MASK_LAST_N,
  SingularConnectorOperator_MULTIPLICATION,
  SingularConnectorOperator_NO_OP,
  SingularConnectorOperator_PROJECTION,
  SingularConnectorOperator_SUBTRACTION,
  SingularConnectorOperator_VALIDATE_NON_NEGATIVE,
  SingularConnectorOperator_VALIDATE_NON_NULL,
  SingularConnectorOperator_VALIDATE_NON_ZERO,
  SingularConnectorOperator_VALIDATE_NUMERIC,
  SingularConnectorOperator'
  #-}
