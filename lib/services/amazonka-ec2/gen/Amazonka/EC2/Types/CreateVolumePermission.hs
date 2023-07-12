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
-- Module      : Amazonka.EC2.Types.CreateVolumePermission
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.CreateVolumePermission where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.PermissionGroup
import qualified Amazonka.Prelude as Prelude

-- | Describes the user or group to be added or removed from the list of
-- create volume permissions for a volume.
--
-- /See:/ 'newCreateVolumePermission' smart constructor.
data CreateVolumePermission = CreateVolumePermission'
  { -- | The group to be added or removed. The possible value is @all@.
    group' :: Prelude.Maybe PermissionGroup,
    -- | The ID of the Amazon Web Services account to be added or removed.
    userId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVolumePermission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'group'', 'createVolumePermission_group' - The group to be added or removed. The possible value is @all@.
--
-- 'userId', 'createVolumePermission_userId' - The ID of the Amazon Web Services account to be added or removed.
newCreateVolumePermission ::
  CreateVolumePermission
newCreateVolumePermission =
  CreateVolumePermission'
    { group' = Prelude.Nothing,
      userId = Prelude.Nothing
    }

-- | The group to be added or removed. The possible value is @all@.
createVolumePermission_group :: Lens.Lens' CreateVolumePermission (Prelude.Maybe PermissionGroup)
createVolumePermission_group = Lens.lens (\CreateVolumePermission' {group'} -> group') (\s@CreateVolumePermission' {} a -> s {group' = a} :: CreateVolumePermission)

-- | The ID of the Amazon Web Services account to be added or removed.
createVolumePermission_userId :: Lens.Lens' CreateVolumePermission (Prelude.Maybe Prelude.Text)
createVolumePermission_userId = Lens.lens (\CreateVolumePermission' {userId} -> userId) (\s@CreateVolumePermission' {} a -> s {userId = a} :: CreateVolumePermission)

instance Data.FromXML CreateVolumePermission where
  parseXML x =
    CreateVolumePermission'
      Prelude.<$> (x Data..@? "group")
      Prelude.<*> (x Data..@? "userId")

instance Prelude.Hashable CreateVolumePermission where
  hashWithSalt _salt CreateVolumePermission' {..} =
    _salt
      `Prelude.hashWithSalt` group'
      `Prelude.hashWithSalt` userId

instance Prelude.NFData CreateVolumePermission where
  rnf CreateVolumePermission' {..} =
    Prelude.rnf group' `Prelude.seq` Prelude.rnf userId

instance Data.ToQuery CreateVolumePermission where
  toQuery CreateVolumePermission' {..} =
    Prelude.mconcat
      ["Group" Data.=: group', "UserId" Data.=: userId]
