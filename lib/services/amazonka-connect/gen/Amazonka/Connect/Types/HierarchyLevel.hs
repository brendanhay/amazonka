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
-- Module      : Amazonka.Connect.Types.HierarchyLevel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.HierarchyLevel where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON HierarchyLevel where
  parseJSON =
    Data.withObject
      "HierarchyLevel"
      ( \x ->
          HierarchyLevel'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Name")
      )

instance Prelude.Hashable HierarchyLevel where
  hashWithSalt _salt HierarchyLevel' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name

instance Prelude.NFData HierarchyLevel where
  rnf HierarchyLevel' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
