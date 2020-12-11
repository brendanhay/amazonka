-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.ComparisonOperator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.ComparisonOperator
  ( ComparisonOperator
      ( ComparisonOperator',
        EqualTo,
        GreaterThan,
        LessThan
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | The comparison operator of a notification. Currently the service supports the following operators:
--
-- @GREATER_THAN@ , @LESS_THAN@ , @EQUAL_TO@
newtype ComparisonOperator = ComparisonOperator' Lude.Text
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

pattern EqualTo :: ComparisonOperator
pattern EqualTo = ComparisonOperator' "EQUAL_TO"

pattern GreaterThan :: ComparisonOperator
pattern GreaterThan = ComparisonOperator' "GREATER_THAN"

pattern LessThan :: ComparisonOperator
pattern LessThan = ComparisonOperator' "LESS_THAN"

{-# COMPLETE
  EqualTo,
  GreaterThan,
  LessThan,
  ComparisonOperator'
  #-}
