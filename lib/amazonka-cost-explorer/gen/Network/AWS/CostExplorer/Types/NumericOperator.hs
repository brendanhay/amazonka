{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.NumericOperator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostExplorer.Types.NumericOperator
  ( NumericOperator
    ( NumericOperator'
    , NumericOperatorEqual
    , NumericOperatorGreaterThanOrEqual
    , NumericOperatorLessThanOrEqual
    , NumericOperatorGreaterThan
    , NumericOperatorLessThan
    , NumericOperatorBetween
    , fromNumericOperator
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype NumericOperator = NumericOperator'{fromNumericOperator ::
                                           Core.Text}
                            deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                            Core.Generic)
                            deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                              Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                              Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                              Core.FromText, Core.ToByteString, Core.ToQuery,
                                              Core.ToHeader)

pattern NumericOperatorEqual :: NumericOperator
pattern NumericOperatorEqual = NumericOperator' "EQUAL"

pattern NumericOperatorGreaterThanOrEqual :: NumericOperator
pattern NumericOperatorGreaterThanOrEqual = NumericOperator' "GREATER_THAN_OR_EQUAL"

pattern NumericOperatorLessThanOrEqual :: NumericOperator
pattern NumericOperatorLessThanOrEqual = NumericOperator' "LESS_THAN_OR_EQUAL"

pattern NumericOperatorGreaterThan :: NumericOperator
pattern NumericOperatorGreaterThan = NumericOperator' "GREATER_THAN"

pattern NumericOperatorLessThan :: NumericOperator
pattern NumericOperatorLessThan = NumericOperator' "LESS_THAN"

pattern NumericOperatorBetween :: NumericOperator
pattern NumericOperatorBetween = NumericOperator' "BETWEEN"

{-# COMPLETE 
  NumericOperatorEqual,

  NumericOperatorGreaterThanOrEqual,

  NumericOperatorLessThanOrEqual,

  NumericOperatorGreaterThan,

  NumericOperatorLessThan,

  NumericOperatorBetween,
  NumericOperator'
  #-}
