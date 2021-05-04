{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.HierarchyPath
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.HierarchyPath where

import Network.AWS.Connect.Types.HierarchyGroupSummary
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about the levels of a hierarchy group.
--
-- /See:/ 'newHierarchyPath' smart constructor.
data HierarchyPath = HierarchyPath'
  { -- | Information about level three.
    levelThree :: Prelude.Maybe HierarchyGroupSummary,
    -- | Information about level four.
    levelFour :: Prelude.Maybe HierarchyGroupSummary,
    -- | Information about level two.
    levelTwo :: Prelude.Maybe HierarchyGroupSummary,
    -- | Information about level one.
    levelOne :: Prelude.Maybe HierarchyGroupSummary,
    -- | Information about level five.
    levelFive :: Prelude.Maybe HierarchyGroupSummary
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'HierarchyPath' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'levelThree', 'hierarchyPath_levelThree' - Information about level three.
--
-- 'levelFour', 'hierarchyPath_levelFour' - Information about level four.
--
-- 'levelTwo', 'hierarchyPath_levelTwo' - Information about level two.
--
-- 'levelOne', 'hierarchyPath_levelOne' - Information about level one.
--
-- 'levelFive', 'hierarchyPath_levelFive' - Information about level five.
newHierarchyPath ::
  HierarchyPath
newHierarchyPath =
  HierarchyPath'
    { levelThree = Prelude.Nothing,
      levelFour = Prelude.Nothing,
      levelTwo = Prelude.Nothing,
      levelOne = Prelude.Nothing,
      levelFive = Prelude.Nothing
    }

-- | Information about level three.
hierarchyPath_levelThree :: Lens.Lens' HierarchyPath (Prelude.Maybe HierarchyGroupSummary)
hierarchyPath_levelThree = Lens.lens (\HierarchyPath' {levelThree} -> levelThree) (\s@HierarchyPath' {} a -> s {levelThree = a} :: HierarchyPath)

-- | Information about level four.
hierarchyPath_levelFour :: Lens.Lens' HierarchyPath (Prelude.Maybe HierarchyGroupSummary)
hierarchyPath_levelFour = Lens.lens (\HierarchyPath' {levelFour} -> levelFour) (\s@HierarchyPath' {} a -> s {levelFour = a} :: HierarchyPath)

-- | Information about level two.
hierarchyPath_levelTwo :: Lens.Lens' HierarchyPath (Prelude.Maybe HierarchyGroupSummary)
hierarchyPath_levelTwo = Lens.lens (\HierarchyPath' {levelTwo} -> levelTwo) (\s@HierarchyPath' {} a -> s {levelTwo = a} :: HierarchyPath)

-- | Information about level one.
hierarchyPath_levelOne :: Lens.Lens' HierarchyPath (Prelude.Maybe HierarchyGroupSummary)
hierarchyPath_levelOne = Lens.lens (\HierarchyPath' {levelOne} -> levelOne) (\s@HierarchyPath' {} a -> s {levelOne = a} :: HierarchyPath)

-- | Information about level five.
hierarchyPath_levelFive :: Lens.Lens' HierarchyPath (Prelude.Maybe HierarchyGroupSummary)
hierarchyPath_levelFive = Lens.lens (\HierarchyPath' {levelFive} -> levelFive) (\s@HierarchyPath' {} a -> s {levelFive = a} :: HierarchyPath)

instance Prelude.FromJSON HierarchyPath where
  parseJSON =
    Prelude.withObject
      "HierarchyPath"
      ( \x ->
          HierarchyPath'
            Prelude.<$> (x Prelude..:? "LevelThree")
            Prelude.<*> (x Prelude..:? "LevelFour")
            Prelude.<*> (x Prelude..:? "LevelTwo")
            Prelude.<*> (x Prelude..:? "LevelOne")
            Prelude.<*> (x Prelude..:? "LevelFive")
      )

instance Prelude.Hashable HierarchyPath

instance Prelude.NFData HierarchyPath
