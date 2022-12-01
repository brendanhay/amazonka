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
-- Module      : Amazonka.EC2.Types.LoadPermissionModifications
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.LoadPermissionModifications where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.LoadPermissionRequest
import qualified Amazonka.Prelude as Prelude

-- | Describes modifications to the load permissions of an Amazon FPGA image
-- (AFI).
--
-- /See:/ 'newLoadPermissionModifications' smart constructor.
data LoadPermissionModifications = LoadPermissionModifications'
  { -- | The load permissions to remove.
    remove :: Prelude.Maybe [LoadPermissionRequest],
    -- | The load permissions to add.
    add :: Prelude.Maybe [LoadPermissionRequest]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LoadPermissionModifications' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'remove', 'loadPermissionModifications_remove' - The load permissions to remove.
--
-- 'add', 'loadPermissionModifications_add' - The load permissions to add.
newLoadPermissionModifications ::
  LoadPermissionModifications
newLoadPermissionModifications =
  LoadPermissionModifications'
    { remove =
        Prelude.Nothing,
      add = Prelude.Nothing
    }

-- | The load permissions to remove.
loadPermissionModifications_remove :: Lens.Lens' LoadPermissionModifications (Prelude.Maybe [LoadPermissionRequest])
loadPermissionModifications_remove = Lens.lens (\LoadPermissionModifications' {remove} -> remove) (\s@LoadPermissionModifications' {} a -> s {remove = a} :: LoadPermissionModifications) Prelude.. Lens.mapping Lens.coerced

-- | The load permissions to add.
loadPermissionModifications_add :: Lens.Lens' LoadPermissionModifications (Prelude.Maybe [LoadPermissionRequest])
loadPermissionModifications_add = Lens.lens (\LoadPermissionModifications' {add} -> add) (\s@LoadPermissionModifications' {} a -> s {add = a} :: LoadPermissionModifications) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable LoadPermissionModifications where
  hashWithSalt _salt LoadPermissionModifications' {..} =
    _salt `Prelude.hashWithSalt` remove
      `Prelude.hashWithSalt` add

instance Prelude.NFData LoadPermissionModifications where
  rnf LoadPermissionModifications' {..} =
    Prelude.rnf remove `Prelude.seq` Prelude.rnf add

instance Core.ToQuery LoadPermissionModifications where
  toQuery LoadPermissionModifications' {..} =
    Prelude.mconcat
      [ Core.toQuery
          (Core.toQueryList "Remove" Prelude.<$> remove),
        Core.toQuery
          (Core.toQueryList "Add" Prelude.<$> add)
      ]
