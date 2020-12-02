{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.InventoryFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.InventoryFilter where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal

-- | Specifies an inventory filter. The inventory only includes objects that meet the filter's criteria.
--
--
--
-- /See:/ 'inventoryFilter' smart constructor.
newtype InventoryFilter = InventoryFilter' {_ifPrefix :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InventoryFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ifPrefix' - The prefix that an object must have to be included in the inventory results.
inventoryFilter ::
  -- | 'ifPrefix'
  Text ->
  InventoryFilter
inventoryFilter pPrefix_ = InventoryFilter' {_ifPrefix = pPrefix_}

-- | The prefix that an object must have to be included in the inventory results.
ifPrefix :: Lens' InventoryFilter Text
ifPrefix = lens _ifPrefix (\s a -> s {_ifPrefix = a})

instance FromXML InventoryFilter where
  parseXML x = InventoryFilter' <$> (x .@ "Prefix")

instance Hashable InventoryFilter

instance NFData InventoryFilter

instance ToXML InventoryFilter where
  toXML InventoryFilter' {..} = mconcat ["Prefix" @= _ifPrefix]
