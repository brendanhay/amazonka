{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.ComparisonOperatorType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.ComparisonOperatorType
  ( ComparisonOperatorType
      ( ComparisonOperatorType',
        GreaterThanOrEqualToThreshold,
        GreaterThanThreshold,
        LessThanOrEqualToThreshold,
        LessThanThreshold
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ComparisonOperatorType = ComparisonOperatorType' Lude.Text
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

pattern GreaterThanOrEqualToThreshold :: ComparisonOperatorType
pattern GreaterThanOrEqualToThreshold = ComparisonOperatorType' "GreaterThanOrEqualToThreshold"

pattern GreaterThanThreshold :: ComparisonOperatorType
pattern GreaterThanThreshold = ComparisonOperatorType' "GreaterThanThreshold"

pattern LessThanOrEqualToThreshold :: ComparisonOperatorType
pattern LessThanOrEqualToThreshold = ComparisonOperatorType' "LessThanOrEqualToThreshold"

pattern LessThanThreshold :: ComparisonOperatorType
pattern LessThanThreshold = ComparisonOperatorType' "LessThanThreshold"

{-# COMPLETE
  GreaterThanOrEqualToThreshold,
  GreaterThanThreshold,
  LessThanOrEqualToThreshold,
  LessThanThreshold,
  ComparisonOperatorType'
  #-}
