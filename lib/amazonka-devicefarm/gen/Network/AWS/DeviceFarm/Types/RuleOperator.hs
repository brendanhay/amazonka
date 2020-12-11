-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.RuleOperator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.RuleOperator
  ( RuleOperator
      ( RuleOperator',
        Contains,
        Equals,
        GreaterThan,
        GreaterThanOrEquals,
        IN,
        LessThan,
        LessThanOrEquals,
        NotIn
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype RuleOperator = RuleOperator' Lude.Text
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

pattern Contains :: RuleOperator
pattern Contains = RuleOperator' "CONTAINS"

pattern Equals :: RuleOperator
pattern Equals = RuleOperator' "EQUALS"

pattern GreaterThan :: RuleOperator
pattern GreaterThan = RuleOperator' "GREATER_THAN"

pattern GreaterThanOrEquals :: RuleOperator
pattern GreaterThanOrEquals = RuleOperator' "GREATER_THAN_OR_EQUALS"

pattern IN :: RuleOperator
pattern IN = RuleOperator' "IN"

pattern LessThan :: RuleOperator
pattern LessThan = RuleOperator' "LESS_THAN"

pattern LessThanOrEquals :: RuleOperator
pattern LessThanOrEquals = RuleOperator' "LESS_THAN_OR_EQUALS"

pattern NotIn :: RuleOperator
pattern NotIn = RuleOperator' "NOT_IN"

{-# COMPLETE
  Contains,
  Equals,
  GreaterThan,
  GreaterThanOrEquals,
  IN,
  LessThan,
  LessThanOrEquals,
  NotIn,
  RuleOperator'
  #-}
