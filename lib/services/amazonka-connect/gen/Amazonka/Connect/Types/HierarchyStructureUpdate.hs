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
-- Module      : Amazonka.Connect.Types.HierarchyStructureUpdate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.HierarchyStructureUpdate where

import Amazonka.Connect.Types.HierarchyLevelUpdate
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the level hierarchy to update.
--
-- /See:/ 'newHierarchyStructureUpdate' smart constructor.
data HierarchyStructureUpdate = HierarchyStructureUpdate'
  { -- | The update for level five.
    levelFive :: Prelude.Maybe HierarchyLevelUpdate,
    -- | The update for level four.
    levelFour :: Prelude.Maybe HierarchyLevelUpdate,
    -- | The update for level one.
    levelOne :: Prelude.Maybe HierarchyLevelUpdate,
    -- | The update for level three.
    levelThree :: Prelude.Maybe HierarchyLevelUpdate,
    -- | The update for level two.
    levelTwo :: Prelude.Maybe HierarchyLevelUpdate
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HierarchyStructureUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'levelFive', 'hierarchyStructureUpdate_levelFive' - The update for level five.
--
-- 'levelFour', 'hierarchyStructureUpdate_levelFour' - The update for level four.
--
-- 'levelOne', 'hierarchyStructureUpdate_levelOne' - The update for level one.
--
-- 'levelThree', 'hierarchyStructureUpdate_levelThree' - The update for level three.
--
-- 'levelTwo', 'hierarchyStructureUpdate_levelTwo' - The update for level two.
newHierarchyStructureUpdate ::
  HierarchyStructureUpdate
newHierarchyStructureUpdate =
  HierarchyStructureUpdate'
    { levelFive =
        Prelude.Nothing,
      levelFour = Prelude.Nothing,
      levelOne = Prelude.Nothing,
      levelThree = Prelude.Nothing,
      levelTwo = Prelude.Nothing
    }

-- | The update for level five.
hierarchyStructureUpdate_levelFive :: Lens.Lens' HierarchyStructureUpdate (Prelude.Maybe HierarchyLevelUpdate)
hierarchyStructureUpdate_levelFive = Lens.lens (\HierarchyStructureUpdate' {levelFive} -> levelFive) (\s@HierarchyStructureUpdate' {} a -> s {levelFive = a} :: HierarchyStructureUpdate)

-- | The update for level four.
hierarchyStructureUpdate_levelFour :: Lens.Lens' HierarchyStructureUpdate (Prelude.Maybe HierarchyLevelUpdate)
hierarchyStructureUpdate_levelFour = Lens.lens (\HierarchyStructureUpdate' {levelFour} -> levelFour) (\s@HierarchyStructureUpdate' {} a -> s {levelFour = a} :: HierarchyStructureUpdate)

-- | The update for level one.
hierarchyStructureUpdate_levelOne :: Lens.Lens' HierarchyStructureUpdate (Prelude.Maybe HierarchyLevelUpdate)
hierarchyStructureUpdate_levelOne = Lens.lens (\HierarchyStructureUpdate' {levelOne} -> levelOne) (\s@HierarchyStructureUpdate' {} a -> s {levelOne = a} :: HierarchyStructureUpdate)

-- | The update for level three.
hierarchyStructureUpdate_levelThree :: Lens.Lens' HierarchyStructureUpdate (Prelude.Maybe HierarchyLevelUpdate)
hierarchyStructureUpdate_levelThree = Lens.lens (\HierarchyStructureUpdate' {levelThree} -> levelThree) (\s@HierarchyStructureUpdate' {} a -> s {levelThree = a} :: HierarchyStructureUpdate)

-- | The update for level two.
hierarchyStructureUpdate_levelTwo :: Lens.Lens' HierarchyStructureUpdate (Prelude.Maybe HierarchyLevelUpdate)
hierarchyStructureUpdate_levelTwo = Lens.lens (\HierarchyStructureUpdate' {levelTwo} -> levelTwo) (\s@HierarchyStructureUpdate' {} a -> s {levelTwo = a} :: HierarchyStructureUpdate)

instance Prelude.Hashable HierarchyStructureUpdate where
  hashWithSalt _salt HierarchyStructureUpdate' {..} =
    _salt `Prelude.hashWithSalt` levelFive
      `Prelude.hashWithSalt` levelFour
      `Prelude.hashWithSalt` levelOne
      `Prelude.hashWithSalt` levelThree
      `Prelude.hashWithSalt` levelTwo

instance Prelude.NFData HierarchyStructureUpdate where
  rnf HierarchyStructureUpdate' {..} =
    Prelude.rnf levelFive
      `Prelude.seq` Prelude.rnf levelFour
      `Prelude.seq` Prelude.rnf levelOne
      `Prelude.seq` Prelude.rnf levelThree
      `Prelude.seq` Prelude.rnf levelTwo

instance Data.ToJSON HierarchyStructureUpdate where
  toJSON HierarchyStructureUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LevelFive" Data..=) Prelude.<$> levelFive,
            ("LevelFour" Data..=) Prelude.<$> levelFour,
            ("LevelOne" Data..=) Prelude.<$> levelOne,
            ("LevelThree" Data..=) Prelude.<$> levelThree,
            ("LevelTwo" Data..=) Prelude.<$> levelTwo
          ]
      )
