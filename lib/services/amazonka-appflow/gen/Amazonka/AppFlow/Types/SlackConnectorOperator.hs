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
-- Module      : Amazonka.AppFlow.Types.SlackConnectorOperator
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.SlackConnectorOperator
  ( SlackConnectorOperator
      ( ..,
        SlackConnectorOperator_ADDITION,
        SlackConnectorOperator_BETWEEN,
        SlackConnectorOperator_DIVISION,
        SlackConnectorOperator_EQUAL_TO,
        SlackConnectorOperator_GREATER_THAN,
        SlackConnectorOperator_GREATER_THAN_OR_EQUAL_TO,
        SlackConnectorOperator_LESS_THAN,
        SlackConnectorOperator_LESS_THAN_OR_EQUAL_TO,
        SlackConnectorOperator_MASK_ALL,
        SlackConnectorOperator_MASK_FIRST_N,
        SlackConnectorOperator_MASK_LAST_N,
        SlackConnectorOperator_MULTIPLICATION,
        SlackConnectorOperator_NO_OP,
        SlackConnectorOperator_PROJECTION,
        SlackConnectorOperator_SUBTRACTION,
        SlackConnectorOperator_VALIDATE_NON_NEGATIVE,
        SlackConnectorOperator_VALIDATE_NON_NULL,
        SlackConnectorOperator_VALIDATE_NON_ZERO,
        SlackConnectorOperator_VALIDATE_NUMERIC
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SlackConnectorOperator = SlackConnectorOperator'
  { fromSlackConnectorOperator ::
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

pattern SlackConnectorOperator_ADDITION :: SlackConnectorOperator
pattern SlackConnectorOperator_ADDITION = SlackConnectorOperator' "ADDITION"

pattern SlackConnectorOperator_BETWEEN :: SlackConnectorOperator
pattern SlackConnectorOperator_BETWEEN = SlackConnectorOperator' "BETWEEN"

pattern SlackConnectorOperator_DIVISION :: SlackConnectorOperator
pattern SlackConnectorOperator_DIVISION = SlackConnectorOperator' "DIVISION"

pattern SlackConnectorOperator_EQUAL_TO :: SlackConnectorOperator
pattern SlackConnectorOperator_EQUAL_TO = SlackConnectorOperator' "EQUAL_TO"

pattern SlackConnectorOperator_GREATER_THAN :: SlackConnectorOperator
pattern SlackConnectorOperator_GREATER_THAN = SlackConnectorOperator' "GREATER_THAN"

pattern SlackConnectorOperator_GREATER_THAN_OR_EQUAL_TO :: SlackConnectorOperator
pattern SlackConnectorOperator_GREATER_THAN_OR_EQUAL_TO = SlackConnectorOperator' "GREATER_THAN_OR_EQUAL_TO"

pattern SlackConnectorOperator_LESS_THAN :: SlackConnectorOperator
pattern SlackConnectorOperator_LESS_THAN = SlackConnectorOperator' "LESS_THAN"

pattern SlackConnectorOperator_LESS_THAN_OR_EQUAL_TO :: SlackConnectorOperator
pattern SlackConnectorOperator_LESS_THAN_OR_EQUAL_TO = SlackConnectorOperator' "LESS_THAN_OR_EQUAL_TO"

pattern SlackConnectorOperator_MASK_ALL :: SlackConnectorOperator
pattern SlackConnectorOperator_MASK_ALL = SlackConnectorOperator' "MASK_ALL"

pattern SlackConnectorOperator_MASK_FIRST_N :: SlackConnectorOperator
pattern SlackConnectorOperator_MASK_FIRST_N = SlackConnectorOperator' "MASK_FIRST_N"

pattern SlackConnectorOperator_MASK_LAST_N :: SlackConnectorOperator
pattern SlackConnectorOperator_MASK_LAST_N = SlackConnectorOperator' "MASK_LAST_N"

pattern SlackConnectorOperator_MULTIPLICATION :: SlackConnectorOperator
pattern SlackConnectorOperator_MULTIPLICATION = SlackConnectorOperator' "MULTIPLICATION"

pattern SlackConnectorOperator_NO_OP :: SlackConnectorOperator
pattern SlackConnectorOperator_NO_OP = SlackConnectorOperator' "NO_OP"

pattern SlackConnectorOperator_PROJECTION :: SlackConnectorOperator
pattern SlackConnectorOperator_PROJECTION = SlackConnectorOperator' "PROJECTION"

pattern SlackConnectorOperator_SUBTRACTION :: SlackConnectorOperator
pattern SlackConnectorOperator_SUBTRACTION = SlackConnectorOperator' "SUBTRACTION"

pattern SlackConnectorOperator_VALIDATE_NON_NEGATIVE :: SlackConnectorOperator
pattern SlackConnectorOperator_VALIDATE_NON_NEGATIVE = SlackConnectorOperator' "VALIDATE_NON_NEGATIVE"

pattern SlackConnectorOperator_VALIDATE_NON_NULL :: SlackConnectorOperator
pattern SlackConnectorOperator_VALIDATE_NON_NULL = SlackConnectorOperator' "VALIDATE_NON_NULL"

pattern SlackConnectorOperator_VALIDATE_NON_ZERO :: SlackConnectorOperator
pattern SlackConnectorOperator_VALIDATE_NON_ZERO = SlackConnectorOperator' "VALIDATE_NON_ZERO"

pattern SlackConnectorOperator_VALIDATE_NUMERIC :: SlackConnectorOperator
pattern SlackConnectorOperator_VALIDATE_NUMERIC = SlackConnectorOperator' "VALIDATE_NUMERIC"

{-# COMPLETE
  SlackConnectorOperator_ADDITION,
  SlackConnectorOperator_BETWEEN,
  SlackConnectorOperator_DIVISION,
  SlackConnectorOperator_EQUAL_TO,
  SlackConnectorOperator_GREATER_THAN,
  SlackConnectorOperator_GREATER_THAN_OR_EQUAL_TO,
  SlackConnectorOperator_LESS_THAN,
  SlackConnectorOperator_LESS_THAN_OR_EQUAL_TO,
  SlackConnectorOperator_MASK_ALL,
  SlackConnectorOperator_MASK_FIRST_N,
  SlackConnectorOperator_MASK_LAST_N,
  SlackConnectorOperator_MULTIPLICATION,
  SlackConnectorOperator_NO_OP,
  SlackConnectorOperator_PROJECTION,
  SlackConnectorOperator_SUBTRACTION,
  SlackConnectorOperator_VALIDATE_NON_NEGATIVE,
  SlackConnectorOperator_VALIDATE_NON_NULL,
  SlackConnectorOperator_VALIDATE_NON_ZERO,
  SlackConnectorOperator_VALIDATE_NUMERIC,
  SlackConnectorOperator'
  #-}
