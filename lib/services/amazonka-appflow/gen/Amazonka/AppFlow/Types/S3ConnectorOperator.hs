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
-- Module      : Amazonka.AppFlow.Types.S3ConnectorOperator
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.S3ConnectorOperator
  ( S3ConnectorOperator
      ( ..,
        S3ConnectorOperator_ADDITION,
        S3ConnectorOperator_BETWEEN,
        S3ConnectorOperator_DIVISION,
        S3ConnectorOperator_EQUAL_TO,
        S3ConnectorOperator_GREATER_THAN,
        S3ConnectorOperator_GREATER_THAN_OR_EQUAL_TO,
        S3ConnectorOperator_LESS_THAN,
        S3ConnectorOperator_LESS_THAN_OR_EQUAL_TO,
        S3ConnectorOperator_MASK_ALL,
        S3ConnectorOperator_MASK_FIRST_N,
        S3ConnectorOperator_MASK_LAST_N,
        S3ConnectorOperator_MULTIPLICATION,
        S3ConnectorOperator_NOT_EQUAL_TO,
        S3ConnectorOperator_NO_OP,
        S3ConnectorOperator_PROJECTION,
        S3ConnectorOperator_SUBTRACTION,
        S3ConnectorOperator_VALIDATE_NON_NEGATIVE,
        S3ConnectorOperator_VALIDATE_NON_NULL,
        S3ConnectorOperator_VALIDATE_NON_ZERO,
        S3ConnectorOperator_VALIDATE_NUMERIC
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype S3ConnectorOperator = S3ConnectorOperator'
  { fromS3ConnectorOperator ::
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

pattern S3ConnectorOperator_ADDITION :: S3ConnectorOperator
pattern S3ConnectorOperator_ADDITION = S3ConnectorOperator' "ADDITION"

pattern S3ConnectorOperator_BETWEEN :: S3ConnectorOperator
pattern S3ConnectorOperator_BETWEEN = S3ConnectorOperator' "BETWEEN"

pattern S3ConnectorOperator_DIVISION :: S3ConnectorOperator
pattern S3ConnectorOperator_DIVISION = S3ConnectorOperator' "DIVISION"

pattern S3ConnectorOperator_EQUAL_TO :: S3ConnectorOperator
pattern S3ConnectorOperator_EQUAL_TO = S3ConnectorOperator' "EQUAL_TO"

pattern S3ConnectorOperator_GREATER_THAN :: S3ConnectorOperator
pattern S3ConnectorOperator_GREATER_THAN = S3ConnectorOperator' "GREATER_THAN"

pattern S3ConnectorOperator_GREATER_THAN_OR_EQUAL_TO :: S3ConnectorOperator
pattern S3ConnectorOperator_GREATER_THAN_OR_EQUAL_TO = S3ConnectorOperator' "GREATER_THAN_OR_EQUAL_TO"

pattern S3ConnectorOperator_LESS_THAN :: S3ConnectorOperator
pattern S3ConnectorOperator_LESS_THAN = S3ConnectorOperator' "LESS_THAN"

pattern S3ConnectorOperator_LESS_THAN_OR_EQUAL_TO :: S3ConnectorOperator
pattern S3ConnectorOperator_LESS_THAN_OR_EQUAL_TO = S3ConnectorOperator' "LESS_THAN_OR_EQUAL_TO"

pattern S3ConnectorOperator_MASK_ALL :: S3ConnectorOperator
pattern S3ConnectorOperator_MASK_ALL = S3ConnectorOperator' "MASK_ALL"

pattern S3ConnectorOperator_MASK_FIRST_N :: S3ConnectorOperator
pattern S3ConnectorOperator_MASK_FIRST_N = S3ConnectorOperator' "MASK_FIRST_N"

pattern S3ConnectorOperator_MASK_LAST_N :: S3ConnectorOperator
pattern S3ConnectorOperator_MASK_LAST_N = S3ConnectorOperator' "MASK_LAST_N"

pattern S3ConnectorOperator_MULTIPLICATION :: S3ConnectorOperator
pattern S3ConnectorOperator_MULTIPLICATION = S3ConnectorOperator' "MULTIPLICATION"

pattern S3ConnectorOperator_NOT_EQUAL_TO :: S3ConnectorOperator
pattern S3ConnectorOperator_NOT_EQUAL_TO = S3ConnectorOperator' "NOT_EQUAL_TO"

pattern S3ConnectorOperator_NO_OP :: S3ConnectorOperator
pattern S3ConnectorOperator_NO_OP = S3ConnectorOperator' "NO_OP"

pattern S3ConnectorOperator_PROJECTION :: S3ConnectorOperator
pattern S3ConnectorOperator_PROJECTION = S3ConnectorOperator' "PROJECTION"

pattern S3ConnectorOperator_SUBTRACTION :: S3ConnectorOperator
pattern S3ConnectorOperator_SUBTRACTION = S3ConnectorOperator' "SUBTRACTION"

pattern S3ConnectorOperator_VALIDATE_NON_NEGATIVE :: S3ConnectorOperator
pattern S3ConnectorOperator_VALIDATE_NON_NEGATIVE = S3ConnectorOperator' "VALIDATE_NON_NEGATIVE"

pattern S3ConnectorOperator_VALIDATE_NON_NULL :: S3ConnectorOperator
pattern S3ConnectorOperator_VALIDATE_NON_NULL = S3ConnectorOperator' "VALIDATE_NON_NULL"

pattern S3ConnectorOperator_VALIDATE_NON_ZERO :: S3ConnectorOperator
pattern S3ConnectorOperator_VALIDATE_NON_ZERO = S3ConnectorOperator' "VALIDATE_NON_ZERO"

pattern S3ConnectorOperator_VALIDATE_NUMERIC :: S3ConnectorOperator
pattern S3ConnectorOperator_VALIDATE_NUMERIC = S3ConnectorOperator' "VALIDATE_NUMERIC"

{-# COMPLETE
  S3ConnectorOperator_ADDITION,
  S3ConnectorOperator_BETWEEN,
  S3ConnectorOperator_DIVISION,
  S3ConnectorOperator_EQUAL_TO,
  S3ConnectorOperator_GREATER_THAN,
  S3ConnectorOperator_GREATER_THAN_OR_EQUAL_TO,
  S3ConnectorOperator_LESS_THAN,
  S3ConnectorOperator_LESS_THAN_OR_EQUAL_TO,
  S3ConnectorOperator_MASK_ALL,
  S3ConnectorOperator_MASK_FIRST_N,
  S3ConnectorOperator_MASK_LAST_N,
  S3ConnectorOperator_MULTIPLICATION,
  S3ConnectorOperator_NOT_EQUAL_TO,
  S3ConnectorOperator_NO_OP,
  S3ConnectorOperator_PROJECTION,
  S3ConnectorOperator_SUBTRACTION,
  S3ConnectorOperator_VALIDATE_NON_NEGATIVE,
  S3ConnectorOperator_VALIDATE_NON_NULL,
  S3ConnectorOperator_VALIDATE_NON_ZERO,
  S3ConnectorOperator_VALIDATE_NUMERIC,
  S3ConnectorOperator'
  #-}
