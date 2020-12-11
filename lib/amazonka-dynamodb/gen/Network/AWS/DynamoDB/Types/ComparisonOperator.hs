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
        BeginsWith,
        Between,
        Contains,
        EQ,
        GE,
        GT,
        IN,
        LE,
        LT,
        NE,
        NotContains,
        NotNull,
        Null
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

pattern BeginsWith :: ComparisonOperator
pattern BeginsWith = ComparisonOperator' "BEGINS_WITH"

pattern Between :: ComparisonOperator
pattern Between = ComparisonOperator' "BETWEEN"

pattern Contains :: ComparisonOperator
pattern Contains = ComparisonOperator' "CONTAINS"

pattern EQ :: ComparisonOperator
pattern EQ = ComparisonOperator' "EQ"

pattern GE :: ComparisonOperator
pattern GE = ComparisonOperator' "GE"

pattern GT :: ComparisonOperator
pattern GT = ComparisonOperator' "GT"

pattern IN :: ComparisonOperator
pattern IN = ComparisonOperator' "IN"

pattern LE :: ComparisonOperator
pattern LE = ComparisonOperator' "LE"

pattern LT :: ComparisonOperator
pattern LT = ComparisonOperator' "LT"

pattern NE :: ComparisonOperator
pattern NE = ComparisonOperator' "NE"

pattern NotContains :: ComparisonOperator
pattern NotContains = ComparisonOperator' "NOT_CONTAINS"

pattern NotNull :: ComparisonOperator
pattern NotNull = ComparisonOperator' "NOT_NULL"

pattern Null :: ComparisonOperator
pattern Null = ComparisonOperator' "NULL"

{-# COMPLETE
  BeginsWith,
  Between,
  Contains,
  EQ,
  GE,
  GT,
  IN,
  LE,
  LT,
  NE,
  NotContains,
  NotNull,
  Null,
  ComparisonOperator'
  #-}
