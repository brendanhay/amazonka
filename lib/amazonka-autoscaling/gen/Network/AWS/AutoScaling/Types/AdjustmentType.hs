{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.AdjustmentType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.AdjustmentType where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a policy adjustment type.
--
--
--
-- /See:/ 'adjustmentType' smart constructor.
newtype AdjustmentType = AdjustmentType'
  { _atAdjustmentType ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AdjustmentType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atAdjustmentType' - The policy adjustment type. The valid values are @ChangeInCapacity@ , @ExactCapacity@ , and @PercentChangeInCapacity@ .
adjustmentType ::
  AdjustmentType
adjustmentType = AdjustmentType' {_atAdjustmentType = Nothing}

-- | The policy adjustment type. The valid values are @ChangeInCapacity@ , @ExactCapacity@ , and @PercentChangeInCapacity@ .
atAdjustmentType :: Lens' AdjustmentType (Maybe Text)
atAdjustmentType = lens _atAdjustmentType (\s a -> s {_atAdjustmentType = a})

instance FromXML AdjustmentType where
  parseXML x = AdjustmentType' <$> (x .@? "AdjustmentType")

instance Hashable AdjustmentType

instance NFData AdjustmentType
