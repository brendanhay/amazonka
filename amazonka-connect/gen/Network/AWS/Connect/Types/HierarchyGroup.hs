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
-- Module      : Network.AWS.Connect.Types.HierarchyGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.HierarchyGroup where

import Network.AWS.Connect.Types.HierarchyPath
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about a hierarchy group.
--
-- /See:/ 'newHierarchyGroup' smart constructor.
data HierarchyGroup = HierarchyGroup'
  { -- | The identifier of the level in the hierarchy group.
    levelId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the hierarchy group.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the hierarchy group.
    id :: Prelude.Maybe Prelude.Text,
    -- | Information about the levels in the hierarchy group.
    hierarchyPath :: Prelude.Maybe HierarchyPath,
    -- | The name of the hierarchy group.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'HierarchyGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'levelId', 'hierarchyGroup_levelId' - The identifier of the level in the hierarchy group.
--
-- 'arn', 'hierarchyGroup_arn' - The Amazon Resource Name (ARN) of the hierarchy group.
--
-- 'id', 'hierarchyGroup_id' - The identifier of the hierarchy group.
--
-- 'hierarchyPath', 'hierarchyGroup_hierarchyPath' - Information about the levels in the hierarchy group.
--
-- 'name', 'hierarchyGroup_name' - The name of the hierarchy group.
newHierarchyGroup ::
  HierarchyGroup
newHierarchyGroup =
  HierarchyGroup'
    { levelId = Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing,
      hierarchyPath = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The identifier of the level in the hierarchy group.
hierarchyGroup_levelId :: Lens.Lens' HierarchyGroup (Prelude.Maybe Prelude.Text)
hierarchyGroup_levelId = Lens.lens (\HierarchyGroup' {levelId} -> levelId) (\s@HierarchyGroup' {} a -> s {levelId = a} :: HierarchyGroup)

-- | The Amazon Resource Name (ARN) of the hierarchy group.
hierarchyGroup_arn :: Lens.Lens' HierarchyGroup (Prelude.Maybe Prelude.Text)
hierarchyGroup_arn = Lens.lens (\HierarchyGroup' {arn} -> arn) (\s@HierarchyGroup' {} a -> s {arn = a} :: HierarchyGroup)

-- | The identifier of the hierarchy group.
hierarchyGroup_id :: Lens.Lens' HierarchyGroup (Prelude.Maybe Prelude.Text)
hierarchyGroup_id = Lens.lens (\HierarchyGroup' {id} -> id) (\s@HierarchyGroup' {} a -> s {id = a} :: HierarchyGroup)

-- | Information about the levels in the hierarchy group.
hierarchyGroup_hierarchyPath :: Lens.Lens' HierarchyGroup (Prelude.Maybe HierarchyPath)
hierarchyGroup_hierarchyPath = Lens.lens (\HierarchyGroup' {hierarchyPath} -> hierarchyPath) (\s@HierarchyGroup' {} a -> s {hierarchyPath = a} :: HierarchyGroup)

-- | The name of the hierarchy group.
hierarchyGroup_name :: Lens.Lens' HierarchyGroup (Prelude.Maybe Prelude.Text)
hierarchyGroup_name = Lens.lens (\HierarchyGroup' {name} -> name) (\s@HierarchyGroup' {} a -> s {name = a} :: HierarchyGroup)

instance Prelude.FromJSON HierarchyGroup where
  parseJSON =
    Prelude.withObject
      "HierarchyGroup"
      ( \x ->
          HierarchyGroup'
            Prelude.<$> (x Prelude..:? "LevelId")
            Prelude.<*> (x Prelude..:? "Arn")
            Prelude.<*> (x Prelude..:? "Id")
            Prelude.<*> (x Prelude..:? "HierarchyPath")
            Prelude.<*> (x Prelude..:? "Name")
      )

instance Prelude.Hashable HierarchyGroup

instance Prelude.NFData HierarchyGroup
