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
-- Module      : Amazonka.AppFlow.Types.ZendeskConnectorOperator
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.ZendeskConnectorOperator
  ( ZendeskConnectorOperator
      ( ..,
        ZendeskConnectorOperator_ADDITION,
        ZendeskConnectorOperator_DIVISION,
        ZendeskConnectorOperator_GREATER_THAN,
        ZendeskConnectorOperator_MASK_ALL,
        ZendeskConnectorOperator_MASK_FIRST_N,
        ZendeskConnectorOperator_MASK_LAST_N,
        ZendeskConnectorOperator_MULTIPLICATION,
        ZendeskConnectorOperator_NO_OP,
        ZendeskConnectorOperator_PROJECTION,
        ZendeskConnectorOperator_SUBTRACTION,
        ZendeskConnectorOperator_VALIDATE_NON_NEGATIVE,
        ZendeskConnectorOperator_VALIDATE_NON_NULL,
        ZendeskConnectorOperator_VALIDATE_NON_ZERO,
        ZendeskConnectorOperator_VALIDATE_NUMERIC
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ZendeskConnectorOperator = ZendeskConnectorOperator'
  { fromZendeskConnectorOperator ::
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

pattern ZendeskConnectorOperator_ADDITION :: ZendeskConnectorOperator
pattern ZendeskConnectorOperator_ADDITION = ZendeskConnectorOperator' "ADDITION"

pattern ZendeskConnectorOperator_DIVISION :: ZendeskConnectorOperator
pattern ZendeskConnectorOperator_DIVISION = ZendeskConnectorOperator' "DIVISION"

pattern ZendeskConnectorOperator_GREATER_THAN :: ZendeskConnectorOperator
pattern ZendeskConnectorOperator_GREATER_THAN = ZendeskConnectorOperator' "GREATER_THAN"

pattern ZendeskConnectorOperator_MASK_ALL :: ZendeskConnectorOperator
pattern ZendeskConnectorOperator_MASK_ALL = ZendeskConnectorOperator' "MASK_ALL"

pattern ZendeskConnectorOperator_MASK_FIRST_N :: ZendeskConnectorOperator
pattern ZendeskConnectorOperator_MASK_FIRST_N = ZendeskConnectorOperator' "MASK_FIRST_N"

pattern ZendeskConnectorOperator_MASK_LAST_N :: ZendeskConnectorOperator
pattern ZendeskConnectorOperator_MASK_LAST_N = ZendeskConnectorOperator' "MASK_LAST_N"

pattern ZendeskConnectorOperator_MULTIPLICATION :: ZendeskConnectorOperator
pattern ZendeskConnectorOperator_MULTIPLICATION = ZendeskConnectorOperator' "MULTIPLICATION"

pattern ZendeskConnectorOperator_NO_OP :: ZendeskConnectorOperator
pattern ZendeskConnectorOperator_NO_OP = ZendeskConnectorOperator' "NO_OP"

pattern ZendeskConnectorOperator_PROJECTION :: ZendeskConnectorOperator
pattern ZendeskConnectorOperator_PROJECTION = ZendeskConnectorOperator' "PROJECTION"

pattern ZendeskConnectorOperator_SUBTRACTION :: ZendeskConnectorOperator
pattern ZendeskConnectorOperator_SUBTRACTION = ZendeskConnectorOperator' "SUBTRACTION"

pattern ZendeskConnectorOperator_VALIDATE_NON_NEGATIVE :: ZendeskConnectorOperator
pattern ZendeskConnectorOperator_VALIDATE_NON_NEGATIVE = ZendeskConnectorOperator' "VALIDATE_NON_NEGATIVE"

pattern ZendeskConnectorOperator_VALIDATE_NON_NULL :: ZendeskConnectorOperator
pattern ZendeskConnectorOperator_VALIDATE_NON_NULL = ZendeskConnectorOperator' "VALIDATE_NON_NULL"

pattern ZendeskConnectorOperator_VALIDATE_NON_ZERO :: ZendeskConnectorOperator
pattern ZendeskConnectorOperator_VALIDATE_NON_ZERO = ZendeskConnectorOperator' "VALIDATE_NON_ZERO"

pattern ZendeskConnectorOperator_VALIDATE_NUMERIC :: ZendeskConnectorOperator
pattern ZendeskConnectorOperator_VALIDATE_NUMERIC = ZendeskConnectorOperator' "VALIDATE_NUMERIC"

{-# COMPLETE
  ZendeskConnectorOperator_ADDITION,
  ZendeskConnectorOperator_DIVISION,
  ZendeskConnectorOperator_GREATER_THAN,
  ZendeskConnectorOperator_MASK_ALL,
  ZendeskConnectorOperator_MASK_FIRST_N,
  ZendeskConnectorOperator_MASK_LAST_N,
  ZendeskConnectorOperator_MULTIPLICATION,
  ZendeskConnectorOperator_NO_OP,
  ZendeskConnectorOperator_PROJECTION,
  ZendeskConnectorOperator_SUBTRACTION,
  ZendeskConnectorOperator_VALIDATE_NON_NEGATIVE,
  ZendeskConnectorOperator_VALIDATE_NON_NULL,
  ZendeskConnectorOperator_VALIDATE_NON_ZERO,
  ZendeskConnectorOperator_VALIDATE_NUMERIC,
  ZendeskConnectorOperator'
  #-}
