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
-- Module      : Network.AWS.EC2.Types.LoadPermissionModifications
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LoadPermissionModifications where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.LoadPermissionRequest
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes modifications to the load permissions of an Amazon FPGA image
-- (AFI).
--
-- /See:/ 'newLoadPermissionModifications' smart constructor.
data LoadPermissionModifications = LoadPermissionModifications'
  { -- | The load permissions to add.
    add :: Prelude.Maybe [LoadPermissionRequest],
    -- | The load permissions to remove.
    remove :: Prelude.Maybe [LoadPermissionRequest]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LoadPermissionModifications' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'add', 'loadPermissionModifications_add' - The load permissions to add.
--
-- 'remove', 'loadPermissionModifications_remove' - The load permissions to remove.
newLoadPermissionModifications ::
  LoadPermissionModifications
newLoadPermissionModifications =
  LoadPermissionModifications'
    { add = Prelude.Nothing,
      remove = Prelude.Nothing
    }

-- | The load permissions to add.
loadPermissionModifications_add :: Lens.Lens' LoadPermissionModifications (Prelude.Maybe [LoadPermissionRequest])
loadPermissionModifications_add = Lens.lens (\LoadPermissionModifications' {add} -> add) (\s@LoadPermissionModifications' {} a -> s {add = a} :: LoadPermissionModifications) Prelude.. Lens.mapping Prelude._Coerce

-- | The load permissions to remove.
loadPermissionModifications_remove :: Lens.Lens' LoadPermissionModifications (Prelude.Maybe [LoadPermissionRequest])
loadPermissionModifications_remove = Lens.lens (\LoadPermissionModifications' {remove} -> remove) (\s@LoadPermissionModifications' {} a -> s {remove = a} :: LoadPermissionModifications) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.Hashable LoadPermissionModifications

instance Prelude.NFData LoadPermissionModifications

instance Prelude.ToQuery LoadPermissionModifications where
  toQuery LoadPermissionModifications' {..} =
    Prelude.mconcat
      [ Prelude.toQuery
          (Prelude.toQueryList "Add" Prelude.<$> add),
        Prelude.toQuery
          (Prelude.toQueryList "Remove" Prelude.<$> remove)
      ]
