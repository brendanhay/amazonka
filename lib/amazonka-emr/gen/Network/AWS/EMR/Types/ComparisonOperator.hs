{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.ComparisonOperator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.ComparisonOperator
  ( ComparisonOperator
      ( ComparisonOperator',
        GreaterThan,
        GreaterThanOrEqual,
        LessThan,
        LessThanOrEqual
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

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

pattern GreaterThan :: ComparisonOperator
pattern GreaterThan = ComparisonOperator' "GREATER_THAN"

pattern GreaterThanOrEqual :: ComparisonOperator
pattern GreaterThanOrEqual = ComparisonOperator' "GREATER_THAN_OR_EQUAL"

pattern LessThan :: ComparisonOperator
pattern LessThan = ComparisonOperator' "LESS_THAN"

pattern LessThanOrEqual :: ComparisonOperator
pattern LessThanOrEqual = ComparisonOperator' "LESS_THAN_OR_EQUAL"

{-# COMPLETE
  GreaterThan,
  GreaterThanOrEqual,
  LessThan,
  LessThanOrEqual,
  ComparisonOperator'
  #-}
