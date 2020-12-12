{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.NumericOperator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.NumericOperator
  ( NumericOperator
      ( NumericOperator',
        Between,
        Equal,
        GreaterThan,
        GreaterThanOrEqual,
        LessThan,
        LessThanOrEqual
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype NumericOperator = NumericOperator' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern Between :: NumericOperator
pattern Between = NumericOperator' "BETWEEN"

pattern Equal :: NumericOperator
pattern Equal = NumericOperator' "EQUAL"

pattern GreaterThan :: NumericOperator
pattern GreaterThan = NumericOperator' "GREATER_THAN"

pattern GreaterThanOrEqual :: NumericOperator
pattern GreaterThanOrEqual = NumericOperator' "GREATER_THAN_OR_EQUAL"

pattern LessThan :: NumericOperator
pattern LessThan = NumericOperator' "LESS_THAN"

pattern LessThanOrEqual :: NumericOperator
pattern LessThanOrEqual = NumericOperator' "LESS_THAN_OR_EQUAL"

{-# COMPLETE
  Between,
  Equal,
  GreaterThan,
  GreaterThanOrEqual,
  LessThan,
  LessThanOrEqual,
  NumericOperator'
  #-}
