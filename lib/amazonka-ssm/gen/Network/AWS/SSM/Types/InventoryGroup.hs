{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InventoryGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InventoryGroup where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.InventoryFilter

-- | A user-defined set of one or more filters on which to aggregate inventory data. Groups return a count of resources that match and don't match the specified criteria.
--
--
--
-- /See:/ 'inventoryGroup' smart constructor.
data InventoryGroup = InventoryGroup'
  { _igName :: !Text,
    _igFilters :: !(List1 InventoryFilter)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InventoryGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'igName' - The name of the group.
--
-- * 'igFilters' - Filters define the criteria for the group. The @matchingCount@ field displays the number of resources that match the criteria. The @notMatchingCount@ field displays the number of resources that don't match the criteria.
inventoryGroup ::
  -- | 'igName'
  Text ->
  -- | 'igFilters'
  NonEmpty InventoryFilter ->
  InventoryGroup
inventoryGroup pName_ pFilters_ =
  InventoryGroup'
    { _igName = pName_,
      _igFilters = _List1 # pFilters_
    }

-- | The name of the group.
igName :: Lens' InventoryGroup Text
igName = lens _igName (\s a -> s {_igName = a})

-- | Filters define the criteria for the group. The @matchingCount@ field displays the number of resources that match the criteria. The @notMatchingCount@ field displays the number of resources that don't match the criteria.
igFilters :: Lens' InventoryGroup (NonEmpty InventoryFilter)
igFilters = lens _igFilters (\s a -> s {_igFilters = a}) . _List1

instance Hashable InventoryGroup

instance NFData InventoryGroup

instance ToJSON InventoryGroup where
  toJSON InventoryGroup' {..} =
    object
      ( catMaybes
          [Just ("Name" .= _igName), Just ("Filters" .= _igFilters)]
      )
