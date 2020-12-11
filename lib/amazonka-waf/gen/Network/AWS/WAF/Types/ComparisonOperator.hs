-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.ComparisonOperator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.ComparisonOperator
  ( ComparisonOperator
      ( ComparisonOperator',
        EQ,
        GE,
        GT,
        LE,
        LT,
        NE
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

pattern EQ :: ComparisonOperator
pattern EQ = ComparisonOperator' "EQ"

pattern GE :: ComparisonOperator
pattern GE = ComparisonOperator' "GE"

pattern GT :: ComparisonOperator
pattern GT = ComparisonOperator' "GT"

pattern LE :: ComparisonOperator
pattern LE = ComparisonOperator' "LE"

pattern LT :: ComparisonOperator
pattern LT = ComparisonOperator' "LT"

pattern NE :: ComparisonOperator
pattern NE = ComparisonOperator' "NE"

{-# COMPLETE
  EQ,
  GE,
  GT,
  LE,
  LT,
  NE,
  ComparisonOperator'
  #-}
