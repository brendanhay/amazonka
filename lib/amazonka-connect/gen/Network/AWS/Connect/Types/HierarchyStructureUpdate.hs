{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.HierarchyStructureUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.HierarchyStructureUpdate where

import Network.AWS.Connect.Types.HierarchyLevelUpdate
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the level hierarchy to update.
--
--
--
-- /See:/ 'hierarchyStructureUpdate' smart constructor.
data HierarchyStructureUpdate = HierarchyStructureUpdate'
  { _hsuLevelFive ::
      !(Maybe HierarchyLevelUpdate),
    _hsuLevelThree ::
      !(Maybe HierarchyLevelUpdate),
    _hsuLevelFour ::
      !(Maybe HierarchyLevelUpdate),
    _hsuLevelTwo ::
      !(Maybe HierarchyLevelUpdate),
    _hsuLevelOne ::
      !(Maybe HierarchyLevelUpdate)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HierarchyStructureUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hsuLevelFive' - The update for level five.
--
-- * 'hsuLevelThree' - The update for level three.
--
-- * 'hsuLevelFour' - The update for level four.
--
-- * 'hsuLevelTwo' - The update for level two.
--
-- * 'hsuLevelOne' - The update for level one.
hierarchyStructureUpdate ::
  HierarchyStructureUpdate
hierarchyStructureUpdate =
  HierarchyStructureUpdate'
    { _hsuLevelFive = Nothing,
      _hsuLevelThree = Nothing,
      _hsuLevelFour = Nothing,
      _hsuLevelTwo = Nothing,
      _hsuLevelOne = Nothing
    }

-- | The update for level five.
hsuLevelFive :: Lens' HierarchyStructureUpdate (Maybe HierarchyLevelUpdate)
hsuLevelFive = lens _hsuLevelFive (\s a -> s {_hsuLevelFive = a})

-- | The update for level three.
hsuLevelThree :: Lens' HierarchyStructureUpdate (Maybe HierarchyLevelUpdate)
hsuLevelThree = lens _hsuLevelThree (\s a -> s {_hsuLevelThree = a})

-- | The update for level four.
hsuLevelFour :: Lens' HierarchyStructureUpdate (Maybe HierarchyLevelUpdate)
hsuLevelFour = lens _hsuLevelFour (\s a -> s {_hsuLevelFour = a})

-- | The update for level two.
hsuLevelTwo :: Lens' HierarchyStructureUpdate (Maybe HierarchyLevelUpdate)
hsuLevelTwo = lens _hsuLevelTwo (\s a -> s {_hsuLevelTwo = a})

-- | The update for level one.
hsuLevelOne :: Lens' HierarchyStructureUpdate (Maybe HierarchyLevelUpdate)
hsuLevelOne = lens _hsuLevelOne (\s a -> s {_hsuLevelOne = a})

instance Hashable HierarchyStructureUpdate

instance NFData HierarchyStructureUpdate

instance ToJSON HierarchyStructureUpdate where
  toJSON HierarchyStructureUpdate' {..} =
    object
      ( catMaybes
          [ ("LevelFive" .=) <$> _hsuLevelFive,
            ("LevelThree" .=) <$> _hsuLevelThree,
            ("LevelFour" .=) <$> _hsuLevelFour,
            ("LevelTwo" .=) <$> _hsuLevelTwo,
            ("LevelOne" .=) <$> _hsuLevelOne
          ]
      )
