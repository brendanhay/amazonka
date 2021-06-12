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
-- Module      : Network.AWS.IoT.Types.ComparisonOperator
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ComparisonOperator
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

import qualified Network.AWS.Core as Core

newtype ComparisonOperator = ComparisonOperator'
  { fromComparisonOperator ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
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
