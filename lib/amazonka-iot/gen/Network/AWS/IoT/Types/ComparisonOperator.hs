{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ComparisonOperator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ComparisonOperator
  ( ComparisonOperator
      ( ComparisonOperator',
        LessThan,
        LessThanEquals,
        GreaterThan,
        GreaterThanEquals,
        InCidrSet,
        NotInCidrSet,
        InPortSet,
        NotInPortSet
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

pattern LessThan :: ComparisonOperator
pattern LessThan = ComparisonOperator' "less-than"

pattern LessThanEquals :: ComparisonOperator
pattern LessThanEquals = ComparisonOperator' "less-than-equals"

pattern GreaterThan :: ComparisonOperator
pattern GreaterThan = ComparisonOperator' "greater-than"

pattern GreaterThanEquals :: ComparisonOperator
pattern GreaterThanEquals = ComparisonOperator' "greater-than-equals"

pattern InCidrSet :: ComparisonOperator
pattern InCidrSet = ComparisonOperator' "in-cidr-set"

pattern NotInCidrSet :: ComparisonOperator
pattern NotInCidrSet = ComparisonOperator' "not-in-cidr-set"

pattern InPortSet :: ComparisonOperator
pattern InPortSet = ComparisonOperator' "in-port-set"

pattern NotInPortSet :: ComparisonOperator
pattern NotInPortSet = ComparisonOperator' "not-in-port-set"

{-# COMPLETE
  LessThan,
  LessThanEquals,
  GreaterThan,
  GreaterThanEquals,
  InCidrSet,
  NotInCidrSet,
  InPortSet,
  NotInPortSet,
  ComparisonOperator'
  #-}
