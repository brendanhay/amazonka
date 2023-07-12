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
-- Module      : Amazonka.IoT.Types.ComparisonOperator
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.ComparisonOperator
  ( ComparisonOperator
      ( ..,
        ComparisonOperator_Greater_than,
        ComparisonOperator_Greater_than_equals,
        ComparisonOperator_In_cidr_set,
        ComparisonOperator_In_port_set,
        ComparisonOperator_In_set,
        ComparisonOperator_Less_than,
        ComparisonOperator_Less_than_equals,
        ComparisonOperator_Not_in_cidr_set,
        ComparisonOperator_Not_in_port_set,
        ComparisonOperator_Not_in_set
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ComparisonOperator = ComparisonOperator'
  { fromComparisonOperator ::
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

pattern ComparisonOperator_Greater_than :: ComparisonOperator
pattern ComparisonOperator_Greater_than = ComparisonOperator' "greater-than"

pattern ComparisonOperator_Greater_than_equals :: ComparisonOperator
pattern ComparisonOperator_Greater_than_equals = ComparisonOperator' "greater-than-equals"

pattern ComparisonOperator_In_cidr_set :: ComparisonOperator
pattern ComparisonOperator_In_cidr_set = ComparisonOperator' "in-cidr-set"

pattern ComparisonOperator_In_port_set :: ComparisonOperator
pattern ComparisonOperator_In_port_set = ComparisonOperator' "in-port-set"

pattern ComparisonOperator_In_set :: ComparisonOperator
pattern ComparisonOperator_In_set = ComparisonOperator' "in-set"

pattern ComparisonOperator_Less_than :: ComparisonOperator
pattern ComparisonOperator_Less_than = ComparisonOperator' "less-than"

pattern ComparisonOperator_Less_than_equals :: ComparisonOperator
pattern ComparisonOperator_Less_than_equals = ComparisonOperator' "less-than-equals"

pattern ComparisonOperator_Not_in_cidr_set :: ComparisonOperator
pattern ComparisonOperator_Not_in_cidr_set = ComparisonOperator' "not-in-cidr-set"

pattern ComparisonOperator_Not_in_port_set :: ComparisonOperator
pattern ComparisonOperator_Not_in_port_set = ComparisonOperator' "not-in-port-set"

pattern ComparisonOperator_Not_in_set :: ComparisonOperator
pattern ComparisonOperator_Not_in_set = ComparisonOperator' "not-in-set"

{-# COMPLETE
  ComparisonOperator_Greater_than,
  ComparisonOperator_Greater_than_equals,
  ComparisonOperator_In_cidr_set,
  ComparisonOperator_In_port_set,
  ComparisonOperator_In_set,
  ComparisonOperator_Less_than,
  ComparisonOperator_Less_than_equals,
  ComparisonOperator_Not_in_cidr_set,
  ComparisonOperator_Not_in_port_set,
  ComparisonOperator_Not_in_set,
  ComparisonOperator'
  #-}
