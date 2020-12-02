{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.HierarchyLevelUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.HierarchyLevelUpdate where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the hierarchy level to update.
--
--
--
-- /See:/ 'hierarchyLevelUpdate' smart constructor.
newtype HierarchyLevelUpdate = HierarchyLevelUpdate'
  { _hluName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HierarchyLevelUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hluName' - The name of the user hierarchy level. Must not be more than 50 characters.
hierarchyLevelUpdate ::
  -- | 'hluName'
  Text ->
  HierarchyLevelUpdate
hierarchyLevelUpdate pName_ =
  HierarchyLevelUpdate' {_hluName = pName_}

-- | The name of the user hierarchy level. Must not be more than 50 characters.
hluName :: Lens' HierarchyLevelUpdate Text
hluName = lens _hluName (\s a -> s {_hluName = a})

instance Hashable HierarchyLevelUpdate

instance NFData HierarchyLevelUpdate

instance ToJSON HierarchyLevelUpdate where
  toJSON HierarchyLevelUpdate' {..} =
    object (catMaybes [Just ("Name" .= _hluName)])
