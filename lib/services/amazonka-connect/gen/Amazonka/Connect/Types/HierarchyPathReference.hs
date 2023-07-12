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
-- Module      : Amazonka.Connect.Types.HierarchyPathReference
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.HierarchyPathReference where

import Amazonka.Connect.Types.HierarchyGroupSummaryReference
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the levels in the hierarchy group.
--
-- /See:/ 'newHierarchyPathReference' smart constructor.
data HierarchyPathReference = HierarchyPathReference'
  { -- | Information about level five.
    levelFive :: Prelude.Maybe HierarchyGroupSummaryReference,
    -- | Information about level four.
    levelFour :: Prelude.Maybe HierarchyGroupSummaryReference,
    -- | Information about level one.
    levelOne :: Prelude.Maybe HierarchyGroupSummaryReference,
    -- | Information about level three.
    levelThree :: Prelude.Maybe HierarchyGroupSummaryReference,
    -- | Information about level two.
    levelTwo :: Prelude.Maybe HierarchyGroupSummaryReference
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HierarchyPathReference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'levelFive', 'hierarchyPathReference_levelFive' - Information about level five.
--
-- 'levelFour', 'hierarchyPathReference_levelFour' - Information about level four.
--
-- 'levelOne', 'hierarchyPathReference_levelOne' - Information about level one.
--
-- 'levelThree', 'hierarchyPathReference_levelThree' - Information about level three.
--
-- 'levelTwo', 'hierarchyPathReference_levelTwo' - Information about level two.
newHierarchyPathReference ::
  HierarchyPathReference
newHierarchyPathReference =
  HierarchyPathReference'
    { levelFive =
        Prelude.Nothing,
      levelFour = Prelude.Nothing,
      levelOne = Prelude.Nothing,
      levelThree = Prelude.Nothing,
      levelTwo = Prelude.Nothing
    }

-- | Information about level five.
hierarchyPathReference_levelFive :: Lens.Lens' HierarchyPathReference (Prelude.Maybe HierarchyGroupSummaryReference)
hierarchyPathReference_levelFive = Lens.lens (\HierarchyPathReference' {levelFive} -> levelFive) (\s@HierarchyPathReference' {} a -> s {levelFive = a} :: HierarchyPathReference)

-- | Information about level four.
hierarchyPathReference_levelFour :: Lens.Lens' HierarchyPathReference (Prelude.Maybe HierarchyGroupSummaryReference)
hierarchyPathReference_levelFour = Lens.lens (\HierarchyPathReference' {levelFour} -> levelFour) (\s@HierarchyPathReference' {} a -> s {levelFour = a} :: HierarchyPathReference)

-- | Information about level one.
hierarchyPathReference_levelOne :: Lens.Lens' HierarchyPathReference (Prelude.Maybe HierarchyGroupSummaryReference)
hierarchyPathReference_levelOne = Lens.lens (\HierarchyPathReference' {levelOne} -> levelOne) (\s@HierarchyPathReference' {} a -> s {levelOne = a} :: HierarchyPathReference)

-- | Information about level three.
hierarchyPathReference_levelThree :: Lens.Lens' HierarchyPathReference (Prelude.Maybe HierarchyGroupSummaryReference)
hierarchyPathReference_levelThree = Lens.lens (\HierarchyPathReference' {levelThree} -> levelThree) (\s@HierarchyPathReference' {} a -> s {levelThree = a} :: HierarchyPathReference)

-- | Information about level two.
hierarchyPathReference_levelTwo :: Lens.Lens' HierarchyPathReference (Prelude.Maybe HierarchyGroupSummaryReference)
hierarchyPathReference_levelTwo = Lens.lens (\HierarchyPathReference' {levelTwo} -> levelTwo) (\s@HierarchyPathReference' {} a -> s {levelTwo = a} :: HierarchyPathReference)

instance Data.FromJSON HierarchyPathReference where
  parseJSON =
    Data.withObject
      "HierarchyPathReference"
      ( \x ->
          HierarchyPathReference'
            Prelude.<$> (x Data..:? "LevelFive")
            Prelude.<*> (x Data..:? "LevelFour")
            Prelude.<*> (x Data..:? "LevelOne")
            Prelude.<*> (x Data..:? "LevelThree")
            Prelude.<*> (x Data..:? "LevelTwo")
      )

instance Prelude.Hashable HierarchyPathReference where
  hashWithSalt _salt HierarchyPathReference' {..} =
    _salt
      `Prelude.hashWithSalt` levelFive
      `Prelude.hashWithSalt` levelFour
      `Prelude.hashWithSalt` levelOne
      `Prelude.hashWithSalt` levelThree
      `Prelude.hashWithSalt` levelTwo

instance Prelude.NFData HierarchyPathReference where
  rnf HierarchyPathReference' {..} =
    Prelude.rnf levelFive
      `Prelude.seq` Prelude.rnf levelFour
      `Prelude.seq` Prelude.rnf levelOne
      `Prelude.seq` Prelude.rnf levelThree
      `Prelude.seq` Prelude.rnf levelTwo
