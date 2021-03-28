{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.AdjustmentType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.AdjustmentType
  ( AdjustmentType
    ( AdjustmentType'
    , AdjustmentTypeChangeInCapacity
    , AdjustmentTypePercentChangeInCapacity
    , AdjustmentTypeExactCapacity
    , fromAdjustmentType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype AdjustmentType = AdjustmentType'{fromAdjustmentType ::
                                         Core.Text}
                           deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                           Core.Generic)
                           deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                             Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                             Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                             Core.FromText, Core.ToByteString, Core.ToQuery,
                                             Core.ToHeader)

pattern AdjustmentTypeChangeInCapacity :: AdjustmentType
pattern AdjustmentTypeChangeInCapacity = AdjustmentType' "CHANGE_IN_CAPACITY"

pattern AdjustmentTypePercentChangeInCapacity :: AdjustmentType
pattern AdjustmentTypePercentChangeInCapacity = AdjustmentType' "PERCENT_CHANGE_IN_CAPACITY"

pattern AdjustmentTypeExactCapacity :: AdjustmentType
pattern AdjustmentTypeExactCapacity = AdjustmentType' "EXACT_CAPACITY"

{-# COMPLETE 
  AdjustmentTypeChangeInCapacity,

  AdjustmentTypePercentChangeInCapacity,

  AdjustmentTypeExactCapacity,
  AdjustmentType'
  #-}
