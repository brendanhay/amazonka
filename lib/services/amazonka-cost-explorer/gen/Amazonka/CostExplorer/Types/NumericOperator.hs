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
-- Module      : Amazonka.CostExplorer.Types.NumericOperator
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.NumericOperator
  ( NumericOperator
      ( ..,
        NumericOperator_BETWEEN,
        NumericOperator_EQUAL,
        NumericOperator_GREATER_THAN,
        NumericOperator_GREATER_THAN_OR_EQUAL,
        NumericOperator_LESS_THAN,
        NumericOperator_LESS_THAN_OR_EQUAL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype NumericOperator = NumericOperator'
  { fromNumericOperator ::
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

pattern NumericOperator_BETWEEN :: NumericOperator
pattern NumericOperator_BETWEEN = NumericOperator' "BETWEEN"

pattern NumericOperator_EQUAL :: NumericOperator
pattern NumericOperator_EQUAL = NumericOperator' "EQUAL"

pattern NumericOperator_GREATER_THAN :: NumericOperator
pattern NumericOperator_GREATER_THAN = NumericOperator' "GREATER_THAN"

pattern NumericOperator_GREATER_THAN_OR_EQUAL :: NumericOperator
pattern NumericOperator_GREATER_THAN_OR_EQUAL = NumericOperator' "GREATER_THAN_OR_EQUAL"

pattern NumericOperator_LESS_THAN :: NumericOperator
pattern NumericOperator_LESS_THAN = NumericOperator' "LESS_THAN"

pattern NumericOperator_LESS_THAN_OR_EQUAL :: NumericOperator
pattern NumericOperator_LESS_THAN_OR_EQUAL = NumericOperator' "LESS_THAN_OR_EQUAL"

{-# COMPLETE
  NumericOperator_BETWEEN,
  NumericOperator_EQUAL,
  NumericOperator_GREATER_THAN,
  NumericOperator_GREATER_THAN_OR_EQUAL,
  NumericOperator_LESS_THAN,
  NumericOperator_LESS_THAN_OR_EQUAL,
  NumericOperator'
  #-}
