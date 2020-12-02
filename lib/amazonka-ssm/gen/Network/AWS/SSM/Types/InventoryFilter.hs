{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InventoryFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InventoryFilter where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.InventoryQueryOperatorType

-- | One or more filters. Use a filter to return a more specific list of results.
--
--
--
-- /See:/ 'inventoryFilter' smart constructor.
data InventoryFilter = InventoryFilter'
  { _ifType ::
      !(Maybe InventoryQueryOperatorType),
    _ifKey :: !Text,
    _ifValues :: !(List1 Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InventoryFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ifType' - The type of filter.
--
-- * 'ifKey' - The name of the filter key.
--
-- * 'ifValues' - Inventory filter values. Example: inventory filter where instance IDs are specified as values Key=AWS:InstanceInformation.InstanceId,Values= i-a12b3c4d5e6g, i-1a2b3c4d5e6,Type=Equal
inventoryFilter ::
  -- | 'ifKey'
  Text ->
  -- | 'ifValues'
  NonEmpty Text ->
  InventoryFilter
inventoryFilter pKey_ pValues_ =
  InventoryFilter'
    { _ifType = Nothing,
      _ifKey = pKey_,
      _ifValues = _List1 # pValues_
    }

-- | The type of filter.
ifType :: Lens' InventoryFilter (Maybe InventoryQueryOperatorType)
ifType = lens _ifType (\s a -> s {_ifType = a})

-- | The name of the filter key.
ifKey :: Lens' InventoryFilter Text
ifKey = lens _ifKey (\s a -> s {_ifKey = a})

-- | Inventory filter values. Example: inventory filter where instance IDs are specified as values Key=AWS:InstanceInformation.InstanceId,Values= i-a12b3c4d5e6g, i-1a2b3c4d5e6,Type=Equal
ifValues :: Lens' InventoryFilter (NonEmpty Text)
ifValues = lens _ifValues (\s a -> s {_ifValues = a}) . _List1

instance Hashable InventoryFilter

instance NFData InventoryFilter

instance ToJSON InventoryFilter where
  toJSON InventoryFilter' {..} =
    object
      ( catMaybes
          [ ("Type" .=) <$> _ifType,
            Just ("Key" .= _ifKey),
            Just ("Values" .= _ifValues)
          ]
      )
