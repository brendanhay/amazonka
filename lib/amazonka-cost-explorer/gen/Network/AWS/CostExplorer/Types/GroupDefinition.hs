{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.GroupDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.GroupDefinition where

import Network.AWS.CostExplorer.Types.GroupDefinitionType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a group when you specify a group by criteria or in the response to a query with a specific grouping.
--
--
--
-- /See:/ 'groupDefinition' smart constructor.
data GroupDefinition = GroupDefinition'
  { _gdKey :: !(Maybe Text),
    _gdType :: !(Maybe GroupDefinitionType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GroupDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdKey' - The string that represents a key for a specified group.
--
-- * 'gdType' - The string that represents the type of group.
groupDefinition ::
  GroupDefinition
groupDefinition =
  GroupDefinition' {_gdKey = Nothing, _gdType = Nothing}

-- | The string that represents a key for a specified group.
gdKey :: Lens' GroupDefinition (Maybe Text)
gdKey = lens _gdKey (\s a -> s {_gdKey = a})

-- | The string that represents the type of group.
gdType :: Lens' GroupDefinition (Maybe GroupDefinitionType)
gdType = lens _gdType (\s a -> s {_gdType = a})

instance FromJSON GroupDefinition where
  parseJSON =
    withObject
      "GroupDefinition"
      (\x -> GroupDefinition' <$> (x .:? "Key") <*> (x .:? "Type"))

instance Hashable GroupDefinition

instance NFData GroupDefinition

instance ToJSON GroupDefinition where
  toJSON GroupDefinition' {..} =
    object
      (catMaybes [("Key" .=) <$> _gdKey, ("Type" .=) <$> _gdType])
