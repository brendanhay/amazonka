{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype ComparisonOperator = ComparisonOperator'
  { fromComparisonOperator ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
