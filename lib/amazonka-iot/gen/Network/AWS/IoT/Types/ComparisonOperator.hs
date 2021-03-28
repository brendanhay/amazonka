{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ComparisonOperator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.ComparisonOperator
  ( ComparisonOperator
    ( ComparisonOperator'
    , ComparisonOperatorLessThan
    , ComparisonOperatorLessThanEquals
    , ComparisonOperatorGreaterThan
    , ComparisonOperatorGreaterThanEquals
    , ComparisonOperatorInCidrSet
    , ComparisonOperatorNotInCidrSet
    , ComparisonOperatorInPortSet
    , ComparisonOperatorNotInPortSet
    , fromComparisonOperator
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ComparisonOperator = ComparisonOperator'{fromComparisonOperator
                                                 :: Core.Text}
                               deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                               Core.Generic)
                               deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                 Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                 Core.FromJSON, Core.ToXML, Core.FromXML,
                                                 Core.ToText, Core.FromText, Core.ToByteString,
                                                 Core.ToQuery, Core.ToHeader)

pattern ComparisonOperatorLessThan :: ComparisonOperator
pattern ComparisonOperatorLessThan = ComparisonOperator' "less-than"

pattern ComparisonOperatorLessThanEquals :: ComparisonOperator
pattern ComparisonOperatorLessThanEquals = ComparisonOperator' "less-than-equals"

pattern ComparisonOperatorGreaterThan :: ComparisonOperator
pattern ComparisonOperatorGreaterThan = ComparisonOperator' "greater-than"

pattern ComparisonOperatorGreaterThanEquals :: ComparisonOperator
pattern ComparisonOperatorGreaterThanEquals = ComparisonOperator' "greater-than-equals"

pattern ComparisonOperatorInCidrSet :: ComparisonOperator
pattern ComparisonOperatorInCidrSet = ComparisonOperator' "in-cidr-set"

pattern ComparisonOperatorNotInCidrSet :: ComparisonOperator
pattern ComparisonOperatorNotInCidrSet = ComparisonOperator' "not-in-cidr-set"

pattern ComparisonOperatorInPortSet :: ComparisonOperator
pattern ComparisonOperatorInPortSet = ComparisonOperator' "in-port-set"

pattern ComparisonOperatorNotInPortSet :: ComparisonOperator
pattern ComparisonOperatorNotInPortSet = ComparisonOperator' "not-in-port-set"

{-# COMPLETE 
  ComparisonOperatorLessThan,

  ComparisonOperatorLessThanEquals,

  ComparisonOperatorGreaterThan,

  ComparisonOperatorGreaterThanEquals,

  ComparisonOperatorInCidrSet,

  ComparisonOperatorNotInCidrSet,

  ComparisonOperatorInPortSet,

  ComparisonOperatorNotInPortSet,
  ComparisonOperator'
  #-}
