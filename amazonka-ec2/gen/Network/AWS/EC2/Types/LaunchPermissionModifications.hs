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
-- Module      : Network.AWS.EC2.Types.LaunchPermissionModifications
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchPermissionModifications where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.LaunchPermission
import qualified Network.AWS.Lens as Lens

-- | Describes a launch permission modification.
--
-- /See:/ 'newLaunchPermissionModifications' smart constructor.
data LaunchPermissionModifications = LaunchPermissionModifications'
  { -- | The AWS account ID to add to the list of launch permissions for the AMI.
    add :: Core.Maybe [LaunchPermission],
    -- | The AWS account ID to remove from the list of launch permissions for the
    -- AMI.
    remove :: Core.Maybe [LaunchPermission]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LaunchPermissionModifications' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'add', 'launchPermissionModifications_add' - The AWS account ID to add to the list of launch permissions for the AMI.
--
-- 'remove', 'launchPermissionModifications_remove' - The AWS account ID to remove from the list of launch permissions for the
-- AMI.
newLaunchPermissionModifications ::
  LaunchPermissionModifications
newLaunchPermissionModifications =
  LaunchPermissionModifications'
    { add = Core.Nothing,
      remove = Core.Nothing
    }

-- | The AWS account ID to add to the list of launch permissions for the AMI.
launchPermissionModifications_add :: Lens.Lens' LaunchPermissionModifications (Core.Maybe [LaunchPermission])
launchPermissionModifications_add = Lens.lens (\LaunchPermissionModifications' {add} -> add) (\s@LaunchPermissionModifications' {} a -> s {add = a} :: LaunchPermissionModifications) Core.. Lens.mapping Lens._Coerce

-- | The AWS account ID to remove from the list of launch permissions for the
-- AMI.
launchPermissionModifications_remove :: Lens.Lens' LaunchPermissionModifications (Core.Maybe [LaunchPermission])
launchPermissionModifications_remove = Lens.lens (\LaunchPermissionModifications' {remove} -> remove) (\s@LaunchPermissionModifications' {} a -> s {remove = a} :: LaunchPermissionModifications) Core.. Lens.mapping Lens._Coerce

instance Core.Hashable LaunchPermissionModifications

instance Core.NFData LaunchPermissionModifications

instance Core.ToQuery LaunchPermissionModifications where
  toQuery LaunchPermissionModifications' {..} =
    Core.mconcat
      [ Core.toQuery (Core.toQueryList "Add" Core.<$> add),
        Core.toQuery
          (Core.toQueryList "Remove" Core.<$> remove)
      ]
