{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.HierarchyPath
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.HierarchyPath where

import Network.AWS.Connect.Types.HierarchyGroupSummary
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the levels of a hierarchy group.
--
--
--
-- /See:/ 'hierarchyPath' smart constructor.
data HierarchyPath = HierarchyPath'
  { _hpLevelFive ::
      !(Maybe HierarchyGroupSummary),
    _hpLevelThree :: !(Maybe HierarchyGroupSummary),
    _hpLevelFour :: !(Maybe HierarchyGroupSummary),
    _hpLevelTwo :: !(Maybe HierarchyGroupSummary),
    _hpLevelOne :: !(Maybe HierarchyGroupSummary)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HierarchyPath' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hpLevelFive' - Information about level five.
--
-- * 'hpLevelThree' - Information about level three.
--
-- * 'hpLevelFour' - Information about level four.
--
-- * 'hpLevelTwo' - Information about level two.
--
-- * 'hpLevelOne' - Information about level one.
hierarchyPath ::
  HierarchyPath
hierarchyPath =
  HierarchyPath'
    { _hpLevelFive = Nothing,
      _hpLevelThree = Nothing,
      _hpLevelFour = Nothing,
      _hpLevelTwo = Nothing,
      _hpLevelOne = Nothing
    }

-- | Information about level five.
hpLevelFive :: Lens' HierarchyPath (Maybe HierarchyGroupSummary)
hpLevelFive = lens _hpLevelFive (\s a -> s {_hpLevelFive = a})

-- | Information about level three.
hpLevelThree :: Lens' HierarchyPath (Maybe HierarchyGroupSummary)
hpLevelThree = lens _hpLevelThree (\s a -> s {_hpLevelThree = a})

-- | Information about level four.
hpLevelFour :: Lens' HierarchyPath (Maybe HierarchyGroupSummary)
hpLevelFour = lens _hpLevelFour (\s a -> s {_hpLevelFour = a})

-- | Information about level two.
hpLevelTwo :: Lens' HierarchyPath (Maybe HierarchyGroupSummary)
hpLevelTwo = lens _hpLevelTwo (\s a -> s {_hpLevelTwo = a})

-- | Information about level one.
hpLevelOne :: Lens' HierarchyPath (Maybe HierarchyGroupSummary)
hpLevelOne = lens _hpLevelOne (\s a -> s {_hpLevelOne = a})

instance FromJSON HierarchyPath where
  parseJSON =
    withObject
      "HierarchyPath"
      ( \x ->
          HierarchyPath'
            <$> (x .:? "LevelFive")
            <*> (x .:? "LevelThree")
            <*> (x .:? "LevelFour")
            <*> (x .:? "LevelTwo")
            <*> (x .:? "LevelOne")
      )

instance Hashable HierarchyPath

instance NFData HierarchyPath
