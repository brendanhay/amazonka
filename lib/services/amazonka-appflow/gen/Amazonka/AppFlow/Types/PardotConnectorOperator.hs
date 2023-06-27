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
-- Module      : Amazonka.AppFlow.Types.PardotConnectorOperator
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.PardotConnectorOperator
  ( PardotConnectorOperator
      ( ..,
        PardotConnectorOperator_ADDITION,
        PardotConnectorOperator_DIVISION,
        PardotConnectorOperator_EQUAL_TO,
        PardotConnectorOperator_MASK_ALL,
        PardotConnectorOperator_MASK_FIRST_N,
        PardotConnectorOperator_MASK_LAST_N,
        PardotConnectorOperator_MULTIPLICATION,
        PardotConnectorOperator_NO_OP,
        PardotConnectorOperator_PROJECTION,
        PardotConnectorOperator_SUBTRACTION,
        PardotConnectorOperator_VALIDATE_NON_NEGATIVE,
        PardotConnectorOperator_VALIDATE_NON_NULL,
        PardotConnectorOperator_VALIDATE_NON_ZERO,
        PardotConnectorOperator_VALIDATE_NUMERIC
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PardotConnectorOperator = PardotConnectorOperator'
  { fromPardotConnectorOperator ::
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

pattern PardotConnectorOperator_ADDITION :: PardotConnectorOperator
pattern PardotConnectorOperator_ADDITION = PardotConnectorOperator' "ADDITION"

pattern PardotConnectorOperator_DIVISION :: PardotConnectorOperator
pattern PardotConnectorOperator_DIVISION = PardotConnectorOperator' "DIVISION"

pattern PardotConnectorOperator_EQUAL_TO :: PardotConnectorOperator
pattern PardotConnectorOperator_EQUAL_TO = PardotConnectorOperator' "EQUAL_TO"

pattern PardotConnectorOperator_MASK_ALL :: PardotConnectorOperator
pattern PardotConnectorOperator_MASK_ALL = PardotConnectorOperator' "MASK_ALL"

pattern PardotConnectorOperator_MASK_FIRST_N :: PardotConnectorOperator
pattern PardotConnectorOperator_MASK_FIRST_N = PardotConnectorOperator' "MASK_FIRST_N"

pattern PardotConnectorOperator_MASK_LAST_N :: PardotConnectorOperator
pattern PardotConnectorOperator_MASK_LAST_N = PardotConnectorOperator' "MASK_LAST_N"

pattern PardotConnectorOperator_MULTIPLICATION :: PardotConnectorOperator
pattern PardotConnectorOperator_MULTIPLICATION = PardotConnectorOperator' "MULTIPLICATION"

pattern PardotConnectorOperator_NO_OP :: PardotConnectorOperator
pattern PardotConnectorOperator_NO_OP = PardotConnectorOperator' "NO_OP"

pattern PardotConnectorOperator_PROJECTION :: PardotConnectorOperator
pattern PardotConnectorOperator_PROJECTION = PardotConnectorOperator' "PROJECTION"

pattern PardotConnectorOperator_SUBTRACTION :: PardotConnectorOperator
pattern PardotConnectorOperator_SUBTRACTION = PardotConnectorOperator' "SUBTRACTION"

pattern PardotConnectorOperator_VALIDATE_NON_NEGATIVE :: PardotConnectorOperator
pattern PardotConnectorOperator_VALIDATE_NON_NEGATIVE = PardotConnectorOperator' "VALIDATE_NON_NEGATIVE"

pattern PardotConnectorOperator_VALIDATE_NON_NULL :: PardotConnectorOperator
pattern PardotConnectorOperator_VALIDATE_NON_NULL = PardotConnectorOperator' "VALIDATE_NON_NULL"

pattern PardotConnectorOperator_VALIDATE_NON_ZERO :: PardotConnectorOperator
pattern PardotConnectorOperator_VALIDATE_NON_ZERO = PardotConnectorOperator' "VALIDATE_NON_ZERO"

pattern PardotConnectorOperator_VALIDATE_NUMERIC :: PardotConnectorOperator
pattern PardotConnectorOperator_VALIDATE_NUMERIC = PardotConnectorOperator' "VALIDATE_NUMERIC"

{-# COMPLETE
  PardotConnectorOperator_ADDITION,
  PardotConnectorOperator_DIVISION,
  PardotConnectorOperator_EQUAL_TO,
  PardotConnectorOperator_MASK_ALL,
  PardotConnectorOperator_MASK_FIRST_N,
  PardotConnectorOperator_MASK_LAST_N,
  PardotConnectorOperator_MULTIPLICATION,
  PardotConnectorOperator_NO_OP,
  PardotConnectorOperator_PROJECTION,
  PardotConnectorOperator_SUBTRACTION,
  PardotConnectorOperator_VALIDATE_NON_NEGATIVE,
  PardotConnectorOperator_VALIDATE_NON_NULL,
  PardotConnectorOperator_VALIDATE_NON_ZERO,
  PardotConnectorOperator_VALIDATE_NUMERIC,
  PardotConnectorOperator'
  #-}
