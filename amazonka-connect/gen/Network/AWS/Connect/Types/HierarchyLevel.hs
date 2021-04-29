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
-- Module      : Network.AWS.Connect.Types.HierarchyLevel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.HierarchyLevel where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about a hierarchy level.
--
-- /See:/ 'newHierarchyLevel' smart constructor.
data HierarchyLevel = HierarchyLevel'
  { -- | The Amazon Resource Name (ARN) of the hierarchy level.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the hierarchy level.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the hierarchy level.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'HierarchyLevel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'hierarchyLevel_arn' - The Amazon Resource Name (ARN) of the hierarchy level.
--
-- 'id', 'hierarchyLevel_id' - The identifier of the hierarchy level.
--
-- 'name', 'hierarchyLevel_name' - The name of the hierarchy level.
newHierarchyLevel ::
  HierarchyLevel
newHierarchyLevel =
  HierarchyLevel'
    { arn = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the hierarchy level.
hierarchyLevel_arn :: Lens.Lens' HierarchyLevel (Prelude.Maybe Prelude.Text)
hierarchyLevel_arn = Lens.lens (\HierarchyLevel' {arn} -> arn) (\s@HierarchyLevel' {} a -> s {arn = a} :: HierarchyLevel)

-- | The identifier of the hierarchy level.
hierarchyLevel_id :: Lens.Lens' HierarchyLevel (Prelude.Maybe Prelude.Text)
hierarchyLevel_id = Lens.lens (\HierarchyLevel' {id} -> id) (\s@HierarchyLevel' {} a -> s {id = a} :: HierarchyLevel)

-- | The name of the hierarchy level.
hierarchyLevel_name :: Lens.Lens' HierarchyLevel (Prelude.Maybe Prelude.Text)
hierarchyLevel_name = Lens.lens (\HierarchyLevel' {name} -> name) (\s@HierarchyLevel' {} a -> s {name = a} :: HierarchyLevel)

instance Prelude.FromJSON HierarchyLevel where
  parseJSON =
    Prelude.withObject
      "HierarchyLevel"
      ( \x ->
          HierarchyLevel'
            Prelude.<$> (x Prelude..:? "Arn")
            Prelude.<*> (x Prelude..:? "Id")
            Prelude.<*> (x Prelude..:? "Name")
      )

instance Prelude.Hashable HierarchyLevel

instance Prelude.NFData HierarchyLevel
