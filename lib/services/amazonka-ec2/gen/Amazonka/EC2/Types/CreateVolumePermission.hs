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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.CreateVolumePermission where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.PermissionGroup
import qualified Amazonka.Prelude as Prelude

-- | Describes the user or group to be added or removed from the list of
-- create volume permissions for a volume.
--
-- /See:/ 'newCreateVolumePermission' smart constructor.
data CreateVolumePermission = CreateVolumePermission'
  { -- | The ID of the Amazon Web Services account to be added or removed.
    userId :: Prelude.Maybe Prelude.Text,
    -- | The group to be added or removed. The possible value is @all@.
    group' :: Prelude.Maybe PermissionGroup
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
-- 'userId', 'createVolumePermission_userId' - The ID of the Amazon Web Services account to be added or removed.
--
-- 'group'', 'createVolumePermission_group' - The group to be added or removed. The possible value is @all@.
newCreateVolumePermission ::
  CreateVolumePermission
newCreateVolumePermission =
  CreateVolumePermission'
    { userId = Prelude.Nothing,
      group' = Prelude.Nothing
    }

-- | The ID of the Amazon Web Services account to be added or removed.
createVolumePermission_userId :: Lens.Lens' CreateVolumePermission (Prelude.Maybe Prelude.Text)
createVolumePermission_userId = Lens.lens (\CreateVolumePermission' {userId} -> userId) (\s@CreateVolumePermission' {} a -> s {userId = a} :: CreateVolumePermission)

-- | The group to be added or removed. The possible value is @all@.
createVolumePermission_group :: Lens.Lens' CreateVolumePermission (Prelude.Maybe PermissionGroup)
createVolumePermission_group = Lens.lens (\CreateVolumePermission' {group'} -> group') (\s@CreateVolumePermission' {} a -> s {group' = a} :: CreateVolumePermission)

instance Core.FromXML CreateVolumePermission where
  parseXML x =
    CreateVolumePermission'
      Prelude.<$> (x Core..@? "userId")
      Prelude.<*> (x Core..@? "group")

instance Prelude.Hashable CreateVolumePermission where
  hashWithSalt _salt CreateVolumePermission' {..} =
    _salt `Prelude.hashWithSalt` userId
      `Prelude.hashWithSalt` group'

instance Prelude.NFData CreateVolumePermission where
  rnf CreateVolumePermission' {..} =
    Prelude.rnf userId `Prelude.seq` Prelude.rnf group'

instance Core.ToQuery CreateVolumePermission where
  toQuery CreateVolumePermission' {..} =
    Prelude.mconcat
      ["UserId" Core.=: userId, "Group" Core.=: group']
