{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.HierarchyStructure
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.HierarchyStructure where

import Network.AWS.Connect.Types.HierarchyLevel
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about a hierarchy structure.
--
--
--
-- /See:/ 'hierarchyStructure' smart constructor.
data HierarchyStructure = HierarchyStructure'
  { _hsLevelFive ::
      !(Maybe HierarchyLevel),
    _hsLevelThree :: !(Maybe HierarchyLevel),
    _hsLevelFour :: !(Maybe HierarchyLevel),
    _hsLevelTwo :: !(Maybe HierarchyLevel),
    _hsLevelOne :: !(Maybe HierarchyLevel)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HierarchyStructure' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hsLevelFive' - Information about level five.
--
-- * 'hsLevelThree' - Information about level three.
--
-- * 'hsLevelFour' - Information about level four.
--
-- * 'hsLevelTwo' - Information about level two.
--
-- * 'hsLevelOne' - Information about level one.
hierarchyStructure ::
  HierarchyStructure
hierarchyStructure =
  HierarchyStructure'
    { _hsLevelFive = Nothing,
      _hsLevelThree = Nothing,
      _hsLevelFour = Nothing,
      _hsLevelTwo = Nothing,
      _hsLevelOne = Nothing
    }

-- | Information about level five.
hsLevelFive :: Lens' HierarchyStructure (Maybe HierarchyLevel)
hsLevelFive = lens _hsLevelFive (\s a -> s {_hsLevelFive = a})

-- | Information about level three.
hsLevelThree :: Lens' HierarchyStructure (Maybe HierarchyLevel)
hsLevelThree = lens _hsLevelThree (\s a -> s {_hsLevelThree = a})

-- | Information about level four.
hsLevelFour :: Lens' HierarchyStructure (Maybe HierarchyLevel)
hsLevelFour = lens _hsLevelFour (\s a -> s {_hsLevelFour = a})

-- | Information about level two.
hsLevelTwo :: Lens' HierarchyStructure (Maybe HierarchyLevel)
hsLevelTwo = lens _hsLevelTwo (\s a -> s {_hsLevelTwo = a})

-- | Information about level one.
hsLevelOne :: Lens' HierarchyStructure (Maybe HierarchyLevel)
hsLevelOne = lens _hsLevelOne (\s a -> s {_hsLevelOne = a})

instance FromJSON HierarchyStructure where
  parseJSON =
    withObject
      "HierarchyStructure"
      ( \x ->
          HierarchyStructure'
            <$> (x .:? "LevelFive")
            <*> (x .:? "LevelThree")
            <*> (x .:? "LevelFour")
            <*> (x .:? "LevelTwo")
            <*> (x .:? "LevelOne")
      )

instance Hashable HierarchyStructure

instance NFData HierarchyStructure
