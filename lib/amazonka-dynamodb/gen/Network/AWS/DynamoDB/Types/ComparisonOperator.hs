{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ComparisonOperator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ComparisonOperator
  ( ComparisonOperator
      ( ComparisonOperator',
        ComparisonOperatorEQ,
        ComparisonOperatorNE,
        ComparisonOperatorIN,
        ComparisonOperatorLE,
        ComparisonOperatorLT,
        ComparisonOperatorGE,
        ComparisonOperatorGT,
        ComparisonOperatorBetween,
        ComparisonOperatorNotNull,
        ComparisonOperatorNull,
        ComparisonOperatorContains,
        ComparisonOperatorNotContains,
        ComparisonOperatorBeginsWith,
        fromComparisonOperator
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ComparisonOperator = ComparisonOperator'
  { fromComparisonOperator ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern ComparisonOperatorEQ :: ComparisonOperator
pattern ComparisonOperatorEQ = ComparisonOperator' "EQ"

pattern ComparisonOperatorNE :: ComparisonOperator
pattern ComparisonOperatorNE = ComparisonOperator' "NE"

pattern ComparisonOperatorIN :: ComparisonOperator
pattern ComparisonOperatorIN = ComparisonOperator' "IN"

pattern ComparisonOperatorLE :: ComparisonOperator
pattern ComparisonOperatorLE = ComparisonOperator' "LE"

pattern ComparisonOperatorLT :: ComparisonOperator
pattern ComparisonOperatorLT = ComparisonOperator' "LT"

pattern ComparisonOperatorGE :: ComparisonOperator
pattern ComparisonOperatorGE = ComparisonOperator' "GE"

pattern ComparisonOperatorGT :: ComparisonOperator
pattern ComparisonOperatorGT = ComparisonOperator' "GT"

pattern ComparisonOperatorBetween :: ComparisonOperator
pattern ComparisonOperatorBetween = ComparisonOperator' "BETWEEN"

pattern ComparisonOperatorNotNull :: ComparisonOperator
pattern ComparisonOperatorNotNull = ComparisonOperator' "NOT_NULL"

pattern ComparisonOperatorNull :: ComparisonOperator
pattern ComparisonOperatorNull = ComparisonOperator' "NULL"

pattern ComparisonOperatorContains :: ComparisonOperator
pattern ComparisonOperatorContains = ComparisonOperator' "CONTAINS"

pattern ComparisonOperatorNotContains :: ComparisonOperator
pattern ComparisonOperatorNotContains = ComparisonOperator' "NOT_CONTAINS"

pattern ComparisonOperatorBeginsWith :: ComparisonOperator
pattern ComparisonOperatorBeginsWith = ComparisonOperator' "BEGINS_WITH"

{-# COMPLETE
  ComparisonOperatorEQ,
  ComparisonOperatorNE,
  ComparisonOperatorIN,
  ComparisonOperatorLE,
  ComparisonOperatorLT,
  ComparisonOperatorGE,
  ComparisonOperatorGT,
  ComparisonOperatorBetween,
  ComparisonOperatorNotNull,
  ComparisonOperatorNull,
  ComparisonOperatorContains,
  ComparisonOperatorNotContains,
  ComparisonOperatorBeginsWith,
  ComparisonOperator'
  #-}
