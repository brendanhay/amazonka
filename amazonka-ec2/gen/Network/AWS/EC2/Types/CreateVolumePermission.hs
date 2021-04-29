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
-- Module      : Network.AWS.EC2.Types.CreateVolumePermission
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CreateVolumePermission where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.PermissionGroup
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the user or group to be added or removed from the list of
-- create volume permissions for a volume.
--
-- /See:/ 'newCreateVolumePermission' smart constructor.
data CreateVolumePermission = CreateVolumePermission'
  { -- | The group to be added or removed. The possible value is @all@.
    group' :: Prelude.Maybe PermissionGroup,
    -- | The AWS account ID to be added or removed.
    userId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'userId', 'createVolumePermission_userId' - The AWS account ID to be added or removed.
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

-- | The AWS account ID to be added or removed.
createVolumePermission_userId :: Lens.Lens' CreateVolumePermission (Prelude.Maybe Prelude.Text)
createVolumePermission_userId = Lens.lens (\CreateVolumePermission' {userId} -> userId) (\s@CreateVolumePermission' {} a -> s {userId = a} :: CreateVolumePermission)

instance Prelude.FromXML CreateVolumePermission where
  parseXML x =
    CreateVolumePermission'
      Prelude.<$> (x Prelude..@? "group")
      Prelude.<*> (x Prelude..@? "userId")

instance Prelude.Hashable CreateVolumePermission

instance Prelude.NFData CreateVolumePermission

instance Prelude.ToQuery CreateVolumePermission where
  toQuery CreateVolumePermission' {..} =
    Prelude.mconcat
      [ "Group" Prelude.=: group',
        "UserId" Prelude.=: userId
      ]
