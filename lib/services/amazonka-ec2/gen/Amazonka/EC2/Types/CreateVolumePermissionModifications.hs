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
-- Module      : Amazonka.EC2.Types.CreateVolumePermissionModifications
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.CreateVolumePermissionModifications where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.CreateVolumePermission
import qualified Amazonka.Prelude as Prelude

-- | Describes modifications to the list of create volume permissions for a
-- volume.
--
-- /See:/ 'newCreateVolumePermissionModifications' smart constructor.
data CreateVolumePermissionModifications = CreateVolumePermissionModifications'
  { -- | Adds the specified Amazon Web Services account ID or group to the list.
    add :: Prelude.Maybe [CreateVolumePermission],
    -- | Removes the specified Amazon Web Services account ID or group from the
    -- list.
    remove :: Prelude.Maybe [CreateVolumePermission]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVolumePermissionModifications' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'add', 'createVolumePermissionModifications_add' - Adds the specified Amazon Web Services account ID or group to the list.
--
-- 'remove', 'createVolumePermissionModifications_remove' - Removes the specified Amazon Web Services account ID or group from the
-- list.
newCreateVolumePermissionModifications ::
  CreateVolumePermissionModifications
newCreateVolumePermissionModifications =
  CreateVolumePermissionModifications'
    { add =
        Prelude.Nothing,
      remove = Prelude.Nothing
    }

-- | Adds the specified Amazon Web Services account ID or group to the list.
createVolumePermissionModifications_add :: Lens.Lens' CreateVolumePermissionModifications (Prelude.Maybe [CreateVolumePermission])
createVolumePermissionModifications_add = Lens.lens (\CreateVolumePermissionModifications' {add} -> add) (\s@CreateVolumePermissionModifications' {} a -> s {add = a} :: CreateVolumePermissionModifications) Prelude.. Lens.mapping Lens.coerced

-- | Removes the specified Amazon Web Services account ID or group from the
-- list.
createVolumePermissionModifications_remove :: Lens.Lens' CreateVolumePermissionModifications (Prelude.Maybe [CreateVolumePermission])
createVolumePermissionModifications_remove = Lens.lens (\CreateVolumePermissionModifications' {remove} -> remove) (\s@CreateVolumePermissionModifications' {} a -> s {remove = a} :: CreateVolumePermissionModifications) Prelude.. Lens.mapping Lens.coerced

instance
  Prelude.Hashable
    CreateVolumePermissionModifications
  where
  hashWithSalt
    _salt
    CreateVolumePermissionModifications' {..} =
      _salt
        `Prelude.hashWithSalt` add
        `Prelude.hashWithSalt` remove

instance
  Prelude.NFData
    CreateVolumePermissionModifications
  where
  rnf CreateVolumePermissionModifications' {..} =
    Prelude.rnf add `Prelude.seq` Prelude.rnf remove

instance
  Data.ToQuery
    CreateVolumePermissionModifications
  where
  toQuery CreateVolumePermissionModifications' {..} =
    Prelude.mconcat
      [ Data.toQuery
          (Data.toQueryList "Add" Prelude.<$> add),
        Data.toQuery
          (Data.toQueryList "Remove" Prelude.<$> remove)
      ]
