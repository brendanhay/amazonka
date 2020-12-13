{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        Equals,
        LessThan,
        LessThanOrEquals,
        GreaterThan,
        GreaterThanOrEquals,
        IN,
        NotIn,
        Contains
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

pattern Equals :: RuleOperator
pattern Equals = RuleOperator' "EQUALS"

pattern LessThan :: RuleOperator
pattern LessThan = RuleOperator' "LESS_THAN"

pattern LessThanOrEquals :: RuleOperator
pattern LessThanOrEquals = RuleOperator' "LESS_THAN_OR_EQUALS"

pattern GreaterThan :: RuleOperator
pattern GreaterThan = RuleOperator' "GREATER_THAN"

pattern GreaterThanOrEquals :: RuleOperator
pattern GreaterThanOrEquals = RuleOperator' "GREATER_THAN_OR_EQUALS"

pattern IN :: RuleOperator
pattern IN = RuleOperator' "IN"

pattern NotIn :: RuleOperator
pattern NotIn = RuleOperator' "NOT_IN"

pattern Contains :: RuleOperator
pattern Contains = RuleOperator' "CONTAINS"

{-# COMPLETE
  Equals,
  LessThan,
  LessThanOrEquals,
  GreaterThan,
  GreaterThanOrEquals,
  IN,
  NotIn,
  Contains,
  RuleOperator'
  #-}
