{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.Operator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.Operator
  ( Operator
      ( Operator',
        Equals,
        NotEquals,
        GreaterThan,
        GreaterThanOrEqualTo,
        LessThan,
        LessThanOrEqualTo,
        Contains,
        Exists,
        NotExists,
        IN
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype Operator = Operator' Lude.Text
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

pattern Equals :: Operator
pattern Equals = Operator' "Equals"

pattern NotEquals :: Operator
pattern NotEquals = Operator' "NotEquals"

pattern GreaterThan :: Operator
pattern GreaterThan = Operator' "GreaterThan"

pattern GreaterThanOrEqualTo :: Operator
pattern GreaterThanOrEqualTo = Operator' "GreaterThanOrEqualTo"

pattern LessThan :: Operator
pattern LessThan = Operator' "LessThan"

pattern LessThanOrEqualTo :: Operator
pattern LessThanOrEqualTo = Operator' "LessThanOrEqualTo"

pattern Contains :: Operator
pattern Contains = Operator' "Contains"

pattern Exists :: Operator
pattern Exists = Operator' "Exists"

pattern NotExists :: Operator
pattern NotExists = Operator' "NotExists"

pattern IN :: Operator
pattern IN = Operator' "In"

{-# COMPLETE
  Equals,
  NotEquals,
  GreaterThan,
  GreaterThanOrEqualTo,
  LessThan,
  LessThanOrEqualTo,
  Contains,
  Exists,
  NotExists,
  IN,
  Operator'
  #-}
