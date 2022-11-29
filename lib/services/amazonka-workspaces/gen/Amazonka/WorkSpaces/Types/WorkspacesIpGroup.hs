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
-- Module      : Amazonka.WorkSpaces.Types.WorkspacesIpGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpaces.Types.WorkspacesIpGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkSpaces.Types.IpRuleItem

-- | Describes an IP access control group.
--
-- /See:/ 'newWorkspacesIpGroup' smart constructor.
data WorkspacesIpGroup = WorkspacesIpGroup'
  { -- | The rules.
    userRules :: Prelude.Maybe [IpRuleItem],
    -- | The name of the group.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | The description of the group.
    groupDesc :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the group.
    groupId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'groupName', 'workspacesIpGroup_groupName' - The name of the group.
--
-- 'groupDesc', 'workspacesIpGroup_groupDesc' - The description of the group.
--
-- 'groupId', 'workspacesIpGroup_groupId' - The identifier of the group.
newWorkspacesIpGroup ::
  WorkspacesIpGroup
newWorkspacesIpGroup =
  WorkspacesIpGroup'
    { userRules = Prelude.Nothing,
      groupName = Prelude.Nothing,
      groupDesc = Prelude.Nothing,
      groupId = Prelude.Nothing
    }

-- | The rules.
workspacesIpGroup_userRules :: Lens.Lens' WorkspacesIpGroup (Prelude.Maybe [IpRuleItem])
workspacesIpGroup_userRules = Lens.lens (\WorkspacesIpGroup' {userRules} -> userRules) (\s@WorkspacesIpGroup' {} a -> s {userRules = a} :: WorkspacesIpGroup) Prelude.. Lens.mapping Lens.coerced

-- | The name of the group.
workspacesIpGroup_groupName :: Lens.Lens' WorkspacesIpGroup (Prelude.Maybe Prelude.Text)
workspacesIpGroup_groupName = Lens.lens (\WorkspacesIpGroup' {groupName} -> groupName) (\s@WorkspacesIpGroup' {} a -> s {groupName = a} :: WorkspacesIpGroup)

-- | The description of the group.
workspacesIpGroup_groupDesc :: Lens.Lens' WorkspacesIpGroup (Prelude.Maybe Prelude.Text)
workspacesIpGroup_groupDesc = Lens.lens (\WorkspacesIpGroup' {groupDesc} -> groupDesc) (\s@WorkspacesIpGroup' {} a -> s {groupDesc = a} :: WorkspacesIpGroup)

-- | The identifier of the group.
workspacesIpGroup_groupId :: Lens.Lens' WorkspacesIpGroup (Prelude.Maybe Prelude.Text)
workspacesIpGroup_groupId = Lens.lens (\WorkspacesIpGroup' {groupId} -> groupId) (\s@WorkspacesIpGroup' {} a -> s {groupId = a} :: WorkspacesIpGroup)

instance Core.FromJSON WorkspacesIpGroup where
  parseJSON =
    Core.withObject
      "WorkspacesIpGroup"
      ( \x ->
          WorkspacesIpGroup'
            Prelude.<$> (x Core..:? "userRules" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "groupName")
            Prelude.<*> (x Core..:? "groupDesc")
            Prelude.<*> (x Core..:? "groupId")
      )

instance Prelude.Hashable WorkspacesIpGroup where
  hashWithSalt _salt WorkspacesIpGroup' {..} =
    _salt `Prelude.hashWithSalt` userRules
      `Prelude.hashWithSalt` groupName
      `Prelude.hashWithSalt` groupDesc
      `Prelude.hashWithSalt` groupId

instance Prelude.NFData WorkspacesIpGroup where
  rnf WorkspacesIpGroup' {..} =
    Prelude.rnf userRules
      `Prelude.seq` Prelude.rnf groupName
      `Prelude.seq` Prelude.rnf groupDesc
      `Prelude.seq` Prelude.rnf groupId
