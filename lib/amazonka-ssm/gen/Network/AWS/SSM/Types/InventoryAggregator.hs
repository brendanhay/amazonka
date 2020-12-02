{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InventoryAggregator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InventoryAggregator where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.InventoryGroup

-- | Specifies the inventory type and attribute for the aggregation execution.
--
--
--
-- /See:/ 'inventoryAggregator' smart constructor.
data InventoryAggregator = InventoryAggregator'
  { _iaGroups ::
      !(Maybe (List1 InventoryGroup)),
    _iaAggregators ::
      !(Maybe (List1 InventoryAggregator)),
    _iaExpression :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InventoryAggregator' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iaGroups' - A user-defined set of one or more filters on which to aggregate inventory data. Groups return a count of resources that match and don't match the specified criteria.
--
-- * 'iaAggregators' - Nested aggregators to further refine aggregation for an inventory type.
--
-- * 'iaExpression' - The inventory type and attribute name for aggregation.
inventoryAggregator ::
  InventoryAggregator
inventoryAggregator =
  InventoryAggregator'
    { _iaGroups = Nothing,
      _iaAggregators = Nothing,
      _iaExpression = Nothing
    }

-- | A user-defined set of one or more filters on which to aggregate inventory data. Groups return a count of resources that match and don't match the specified criteria.
iaGroups :: Lens' InventoryAggregator (Maybe (NonEmpty InventoryGroup))
iaGroups = lens _iaGroups (\s a -> s {_iaGroups = a}) . mapping _List1

-- | Nested aggregators to further refine aggregation for an inventory type.
iaAggregators :: Lens' InventoryAggregator (Maybe (NonEmpty InventoryAggregator))
iaAggregators = lens _iaAggregators (\s a -> s {_iaAggregators = a}) . mapping _List1

-- | The inventory type and attribute name for aggregation.
iaExpression :: Lens' InventoryAggregator (Maybe Text)
iaExpression = lens _iaExpression (\s a -> s {_iaExpression = a})

instance Hashable InventoryAggregator

instance NFData InventoryAggregator

instance ToJSON InventoryAggregator where
  toJSON InventoryAggregator' {..} =
    object
      ( catMaybes
          [ ("Groups" .=) <$> _iaGroups,
            ("Aggregators" .=) <$> _iaAggregators,
            ("Expression" .=) <$> _iaExpression
          ]
      )
