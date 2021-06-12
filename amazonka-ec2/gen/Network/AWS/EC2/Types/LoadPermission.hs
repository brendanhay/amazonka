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
-- Module      : Network.AWS.EC2.Types.LoadPermission
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LoadPermission where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.PermissionGroup
import qualified Network.AWS.Lens as Lens

-- | Describes a load permission.
--
-- /See:/ 'newLoadPermission' smart constructor.
data LoadPermission = LoadPermission'
  { -- | The name of the group.
    group' :: Core.Maybe PermissionGroup,
    -- | The AWS account ID.
    userId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LoadPermission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'group'', 'loadPermission_group' - The name of the group.
--
-- 'userId', 'loadPermission_userId' - The AWS account ID.
newLoadPermission ::
  LoadPermission
newLoadPermission =
  LoadPermission'
    { group' = Core.Nothing,
      userId = Core.Nothing
    }

-- | The name of the group.
loadPermission_group :: Lens.Lens' LoadPermission (Core.Maybe PermissionGroup)
loadPermission_group = Lens.lens (\LoadPermission' {group'} -> group') (\s@LoadPermission' {} a -> s {group' = a} :: LoadPermission)

-- | The AWS account ID.
loadPermission_userId :: Lens.Lens' LoadPermission (Core.Maybe Core.Text)
loadPermission_userId = Lens.lens (\LoadPermission' {userId} -> userId) (\s@LoadPermission' {} a -> s {userId = a} :: LoadPermission)

instance Core.FromXML LoadPermission where
  parseXML x =
    LoadPermission'
      Core.<$> (x Core..@? "group") Core.<*> (x Core..@? "userId")

instance Core.Hashable LoadPermission

instance Core.NFData LoadPermission
