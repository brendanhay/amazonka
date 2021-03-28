{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.Operator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.Operator
  ( Operator
    ( Operator'
    , OperatorEquals
    , OperatorNotEquals
    , OperatorGreaterThan
    , OperatorGreaterThanOrEqualTo
    , OperatorLessThan
    , OperatorLessThanOrEqualTo
    , OperatorContains
    , OperatorExists
    , OperatorNotExists
    , OperatorIN
    , fromOperator
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype Operator = Operator'{fromOperator :: Core.Text}
                     deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                     Core.Generic)
                     deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                       Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON, Core.FromJSON,
                                       Core.ToXML, Core.FromXML, Core.ToText, Core.FromText,
                                       Core.ToByteString, Core.ToQuery, Core.ToHeader)

pattern OperatorEquals :: Operator
pattern OperatorEquals = Operator' "Equals"

pattern OperatorNotEquals :: Operator
pattern OperatorNotEquals = Operator' "NotEquals"

pattern OperatorGreaterThan :: Operator
pattern OperatorGreaterThan = Operator' "GreaterThan"

pattern OperatorGreaterThanOrEqualTo :: Operator
pattern OperatorGreaterThanOrEqualTo = Operator' "GreaterThanOrEqualTo"

pattern OperatorLessThan :: Operator
pattern OperatorLessThan = Operator' "LessThan"

pattern OperatorLessThanOrEqualTo :: Operator
pattern OperatorLessThanOrEqualTo = Operator' "LessThanOrEqualTo"

pattern OperatorContains :: Operator
pattern OperatorContains = Operator' "Contains"

pattern OperatorExists :: Operator
pattern OperatorExists = Operator' "Exists"

pattern OperatorNotExists :: Operator
pattern OperatorNotExists = Operator' "NotExists"

pattern OperatorIN :: Operator
pattern OperatorIN = Operator' "In"

{-# COMPLETE 
  OperatorEquals,

  OperatorNotEquals,

  OperatorGreaterThan,

  OperatorGreaterThanOrEqualTo,

  OperatorLessThan,

  OperatorLessThanOrEqualTo,

  OperatorContains,

  OperatorExists,

  OperatorNotExists,

  OperatorIN,
  Operator'
  #-}
