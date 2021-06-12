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
-- Module      : Network.AWS.WorkSpaces.Types.WorkspacesIpGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.WorkspacesIpGroup where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.WorkSpaces.Types.IpRuleItem

-- | Describes an IP access control group.
--
-- /See:/ 'newWorkspacesIpGroup' smart constructor.
data WorkspacesIpGroup = WorkspacesIpGroup'
  { -- | The rules.
    userRules :: Core.Maybe [IpRuleItem],
    -- | The description of the group.
    groupDesc :: Core.Maybe Core.Text,
    -- | The name of the group.
    groupName :: Core.Maybe Core.Text,
    -- | The identifier of the group.
    groupId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'WorkspacesIpGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userRules', 'workspacesIpGroup_userRules' - The rules.
--
-- 'groupDesc', 'workspacesIpGroup_groupDesc' - The description of the group.
--
-- 'groupName', 'workspacesIpGroup_groupName' - The name of the group.
--
-- 'groupId', 'workspacesIpGroup_groupId' - The identifier of the group.
newWorkspacesIpGroup ::
  WorkspacesIpGroup
newWorkspacesIpGroup =
  WorkspacesIpGroup'
    { userRules = Core.Nothing,
      groupDesc = Core.Nothing,
      groupName = Core.Nothing,
      groupId = Core.Nothing
    }

-- | The rules.
workspacesIpGroup_userRules :: Lens.Lens' WorkspacesIpGroup (Core.Maybe [IpRuleItem])
workspacesIpGroup_userRules = Lens.lens (\WorkspacesIpGroup' {userRules} -> userRules) (\s@WorkspacesIpGroup' {} a -> s {userRules = a} :: WorkspacesIpGroup) Core.. Lens.mapping Lens._Coerce

-- | The description of the group.
workspacesIpGroup_groupDesc :: Lens.Lens' WorkspacesIpGroup (Core.Maybe Core.Text)
workspacesIpGroup_groupDesc = Lens.lens (\WorkspacesIpGroup' {groupDesc} -> groupDesc) (\s@WorkspacesIpGroup' {} a -> s {groupDesc = a} :: WorkspacesIpGroup)

-- | The name of the group.
workspacesIpGroup_groupName :: Lens.Lens' WorkspacesIpGroup (Core.Maybe Core.Text)
workspacesIpGroup_groupName = Lens.lens (\WorkspacesIpGroup' {groupName} -> groupName) (\s@WorkspacesIpGroup' {} a -> s {groupName = a} :: WorkspacesIpGroup)

-- | The identifier of the group.
workspacesIpGroup_groupId :: Lens.Lens' WorkspacesIpGroup (Core.Maybe Core.Text)
workspacesIpGroup_groupId = Lens.lens (\WorkspacesIpGroup' {groupId} -> groupId) (\s@WorkspacesIpGroup' {} a -> s {groupId = a} :: WorkspacesIpGroup)

instance Core.FromJSON WorkspacesIpGroup where
  parseJSON =
    Core.withObject
      "WorkspacesIpGroup"
      ( \x ->
          WorkspacesIpGroup'
            Core.<$> (x Core..:? "userRules" Core..!= Core.mempty)
            Core.<*> (x Core..:? "groupDesc")
            Core.<*> (x Core..:? "groupName")
            Core.<*> (x Core..:? "groupId")
      )

instance Core.Hashable WorkspacesIpGroup

instance Core.NFData WorkspacesIpGroup
