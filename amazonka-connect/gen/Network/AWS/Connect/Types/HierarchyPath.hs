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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains information about the levels of a hierarchy group.
--
-- /See:/ 'newHierarchyPath' smart constructor.
data HierarchyPath = HierarchyPath'
  { -- | Information about level three.
    levelThree :: Core.Maybe HierarchyGroupSummary,
    -- | Information about level four.
    levelFour :: Core.Maybe HierarchyGroupSummary,
    -- | Information about level two.
    levelTwo :: Core.Maybe HierarchyGroupSummary,
    -- | Information about level one.
    levelOne :: Core.Maybe HierarchyGroupSummary,
    -- | Information about level five.
    levelFive :: Core.Maybe HierarchyGroupSummary
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { levelThree = Core.Nothing,
      levelFour = Core.Nothing,
      levelTwo = Core.Nothing,
      levelOne = Core.Nothing,
      levelFive = Core.Nothing
    }

-- | Information about level three.
hierarchyPath_levelThree :: Lens.Lens' HierarchyPath (Core.Maybe HierarchyGroupSummary)
hierarchyPath_levelThree = Lens.lens (\HierarchyPath' {levelThree} -> levelThree) (\s@HierarchyPath' {} a -> s {levelThree = a} :: HierarchyPath)

-- | Information about level four.
hierarchyPath_levelFour :: Lens.Lens' HierarchyPath (Core.Maybe HierarchyGroupSummary)
hierarchyPath_levelFour = Lens.lens (\HierarchyPath' {levelFour} -> levelFour) (\s@HierarchyPath' {} a -> s {levelFour = a} :: HierarchyPath)

-- | Information about level two.
hierarchyPath_levelTwo :: Lens.Lens' HierarchyPath (Core.Maybe HierarchyGroupSummary)
hierarchyPath_levelTwo = Lens.lens (\HierarchyPath' {levelTwo} -> levelTwo) (\s@HierarchyPath' {} a -> s {levelTwo = a} :: HierarchyPath)

-- | Information about level one.
hierarchyPath_levelOne :: Lens.Lens' HierarchyPath (Core.Maybe HierarchyGroupSummary)
hierarchyPath_levelOne = Lens.lens (\HierarchyPath' {levelOne} -> levelOne) (\s@HierarchyPath' {} a -> s {levelOne = a} :: HierarchyPath)

-- | Information about level five.
hierarchyPath_levelFive :: Lens.Lens' HierarchyPath (Core.Maybe HierarchyGroupSummary)
hierarchyPath_levelFive = Lens.lens (\HierarchyPath' {levelFive} -> levelFive) (\s@HierarchyPath' {} a -> s {levelFive = a} :: HierarchyPath)

instance Core.FromJSON HierarchyPath where
  parseJSON =
    Core.withObject
      "HierarchyPath"
      ( \x ->
          HierarchyPath'
            Core.<$> (x Core..:? "LevelThree")
            Core.<*> (x Core..:? "LevelFour")
            Core.<*> (x Core..:? "LevelTwo")
            Core.<*> (x Core..:? "LevelOne")
            Core.<*> (x Core..:? "LevelFive")
      )

instance Core.Hashable HierarchyPath

instance Core.NFData HierarchyPath
