-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.AdjustmentType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.AdjustmentType
  ( AdjustmentType
      ( AdjustmentType',
        ChangeInCapacity,
        ExactCapacity,
        PercentChangeInCapacity
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype AdjustmentType = AdjustmentType' Lude.Text
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

pattern ChangeInCapacity :: AdjustmentType
pattern ChangeInCapacity = AdjustmentType' "CHANGE_IN_CAPACITY"

pattern ExactCapacity :: AdjustmentType
pattern ExactCapacity = AdjustmentType' "EXACT_CAPACITY"

pattern PercentChangeInCapacity :: AdjustmentType
pattern PercentChangeInCapacity = AdjustmentType' "PERCENT_CHANGE_IN_CAPACITY"

{-# COMPLETE
  ChangeInCapacity,
  ExactCapacity,
  PercentChangeInCapacity,
  AdjustmentType'
  #-}
