{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.RuleOperator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DeviceFarm.Types.RuleOperator
  ( RuleOperator
    ( RuleOperator'
    , RuleOperatorEquals
    , RuleOperatorLessThan
    , RuleOperatorLessThanOrEquals
    , RuleOperatorGreaterThan
    , RuleOperatorGreaterThanOrEquals
    , RuleOperatorIN
    , RuleOperatorNotIn
    , RuleOperatorContains
    , fromRuleOperator
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype RuleOperator = RuleOperator'{fromRuleOperator :: Core.Text}
                         deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                         Core.Generic)
                         deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                           Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                           Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                           Core.FromText, Core.ToByteString, Core.ToQuery,
                                           Core.ToHeader)

pattern RuleOperatorEquals :: RuleOperator
pattern RuleOperatorEquals = RuleOperator' "EQUALS"

pattern RuleOperatorLessThan :: RuleOperator
pattern RuleOperatorLessThan = RuleOperator' "LESS_THAN"

pattern RuleOperatorLessThanOrEquals :: RuleOperator
pattern RuleOperatorLessThanOrEquals = RuleOperator' "LESS_THAN_OR_EQUALS"

pattern RuleOperatorGreaterThan :: RuleOperator
pattern RuleOperatorGreaterThan = RuleOperator' "GREATER_THAN"

pattern RuleOperatorGreaterThanOrEquals :: RuleOperator
pattern RuleOperatorGreaterThanOrEquals = RuleOperator' "GREATER_THAN_OR_EQUALS"

pattern RuleOperatorIN :: RuleOperator
pattern RuleOperatorIN = RuleOperator' "IN"

pattern RuleOperatorNotIn :: RuleOperator
pattern RuleOperatorNotIn = RuleOperator' "NOT_IN"

pattern RuleOperatorContains :: RuleOperator
pattern RuleOperatorContains = RuleOperator' "CONTAINS"

{-# COMPLETE 
  RuleOperatorEquals,

  RuleOperatorLessThan,

  RuleOperatorLessThanOrEquals,

  RuleOperatorGreaterThan,

  RuleOperatorGreaterThanOrEquals,

  RuleOperatorIN,

  RuleOperatorNotIn,

  RuleOperatorContains,
  RuleOperator'
  #-}
