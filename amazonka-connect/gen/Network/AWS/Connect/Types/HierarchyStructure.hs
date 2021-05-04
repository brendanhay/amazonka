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
-- Module      : Network.AWS.Connect.Types.HierarchyStructure
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.HierarchyStructure where

import Network.AWS.Connect.Types.HierarchyLevel
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about a hierarchy structure.
--
-- /See:/ 'newHierarchyStructure' smart constructor.
data HierarchyStructure = HierarchyStructure'
  { -- | Information about level three.
    levelThree :: Prelude.Maybe HierarchyLevel,
    -- | Information about level four.
    levelFour :: Prelude.Maybe HierarchyLevel,
    -- | Information about level two.
    levelTwo :: Prelude.Maybe HierarchyLevel,
    -- | Information about level one.
    levelOne :: Prelude.Maybe HierarchyLevel,
    -- | Information about level five.
    levelFive :: Prelude.Maybe HierarchyLevel
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'HierarchyStructure' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'levelThree', 'hierarchyStructure_levelThree' - Information about level three.
--
-- 'levelFour', 'hierarchyStructure_levelFour' - Information about level four.
--
-- 'levelTwo', 'hierarchyStructure_levelTwo' - Information about level two.
--
-- 'levelOne', 'hierarchyStructure_levelOne' - Information about level one.
--
-- 'levelFive', 'hierarchyStructure_levelFive' - Information about level five.
newHierarchyStructure ::
  HierarchyStructure
newHierarchyStructure =
  HierarchyStructure'
    { levelThree = Prelude.Nothing,
      levelFour = Prelude.Nothing,
      levelTwo = Prelude.Nothing,
      levelOne = Prelude.Nothing,
      levelFive = Prelude.Nothing
    }

-- | Information about level three.
hierarchyStructure_levelThree :: Lens.Lens' HierarchyStructure (Prelude.Maybe HierarchyLevel)
hierarchyStructure_levelThree = Lens.lens (\HierarchyStructure' {levelThree} -> levelThree) (\s@HierarchyStructure' {} a -> s {levelThree = a} :: HierarchyStructure)

-- | Information about level four.
hierarchyStructure_levelFour :: Lens.Lens' HierarchyStructure (Prelude.Maybe HierarchyLevel)
hierarchyStructure_levelFour = Lens.lens (\HierarchyStructure' {levelFour} -> levelFour) (\s@HierarchyStructure' {} a -> s {levelFour = a} :: HierarchyStructure)

-- | Information about level two.
hierarchyStructure_levelTwo :: Lens.Lens' HierarchyStructure (Prelude.Maybe HierarchyLevel)
hierarchyStructure_levelTwo = Lens.lens (\HierarchyStructure' {levelTwo} -> levelTwo) (\s@HierarchyStructure' {} a -> s {levelTwo = a} :: HierarchyStructure)

-- | Information about level one.
hierarchyStructure_levelOne :: Lens.Lens' HierarchyStructure (Prelude.Maybe HierarchyLevel)
hierarchyStructure_levelOne = Lens.lens (\HierarchyStructure' {levelOne} -> levelOne) (\s@HierarchyStructure' {} a -> s {levelOne = a} :: HierarchyStructure)

-- | Information about level five.
hierarchyStructure_levelFive :: Lens.Lens' HierarchyStructure (Prelude.Maybe HierarchyLevel)
hierarchyStructure_levelFive = Lens.lens (\HierarchyStructure' {levelFive} -> levelFive) (\s@HierarchyStructure' {} a -> s {levelFive = a} :: HierarchyStructure)

instance Prelude.FromJSON HierarchyStructure where
  parseJSON =
    Prelude.withObject
      "HierarchyStructure"
      ( \x ->
          HierarchyStructure'
            Prelude.<$> (x Prelude..:? "LevelThree")
            Prelude.<*> (x Prelude..:? "LevelFour")
            Prelude.<*> (x Prelude..:? "LevelTwo")
            Prelude.<*> (x Prelude..:? "LevelOne")
            Prelude.<*> (x Prelude..:? "LevelFive")
      )

instance Prelude.Hashable HierarchyStructure

instance Prelude.NFData HierarchyStructure
