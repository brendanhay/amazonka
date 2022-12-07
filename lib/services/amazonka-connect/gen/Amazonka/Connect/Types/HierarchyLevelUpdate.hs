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
-- Module      : Amazonka.Connect.Types.HierarchyLevelUpdate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.HierarchyLevelUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the hierarchy level to update.
--
-- /See:/ 'newHierarchyLevelUpdate' smart constructor.
data HierarchyLevelUpdate = HierarchyLevelUpdate'
  { -- | The name of the user hierarchy level. Must not be more than 50
    -- characters.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HierarchyLevelUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'hierarchyLevelUpdate_name' - The name of the user hierarchy level. Must not be more than 50
-- characters.
newHierarchyLevelUpdate ::
  -- | 'name'
  Prelude.Text ->
  HierarchyLevelUpdate
newHierarchyLevelUpdate pName_ =
  HierarchyLevelUpdate' {name = pName_}

-- | The name of the user hierarchy level. Must not be more than 50
-- characters.
hierarchyLevelUpdate_name :: Lens.Lens' HierarchyLevelUpdate Prelude.Text
hierarchyLevelUpdate_name = Lens.lens (\HierarchyLevelUpdate' {name} -> name) (\s@HierarchyLevelUpdate' {} a -> s {name = a} :: HierarchyLevelUpdate)

instance Prelude.Hashable HierarchyLevelUpdate where
  hashWithSalt _salt HierarchyLevelUpdate' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData HierarchyLevelUpdate where
  rnf HierarchyLevelUpdate' {..} = Prelude.rnf name

instance Data.ToJSON HierarchyLevelUpdate where
  toJSON HierarchyLevelUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Data..= name)]
      )
