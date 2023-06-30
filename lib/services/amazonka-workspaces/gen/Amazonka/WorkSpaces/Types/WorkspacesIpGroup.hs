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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpaces.Types.WorkspacesIpGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkSpaces.Types.IpRuleItem

-- | Describes an IP access control group.
--
-- /See:/ 'newWorkspacesIpGroup' smart constructor.
data WorkspacesIpGroup = WorkspacesIpGroup'
  { -- | The description of the group.
    groupDesc :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the group.
    groupId :: Prelude.Maybe Prelude.Text,
    -- | The name of the group.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | The rules.
    userRules :: Prelude.Maybe [IpRuleItem]
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
-- 'groupDesc', 'workspacesIpGroup_groupDesc' - The description of the group.
--
-- 'groupId', 'workspacesIpGroup_groupId' - The identifier of the group.
--
-- 'groupName', 'workspacesIpGroup_groupName' - The name of the group.
--
-- 'userRules', 'workspacesIpGroup_userRules' - The rules.
newWorkspacesIpGroup ::
  WorkspacesIpGroup
newWorkspacesIpGroup =
  WorkspacesIpGroup'
    { groupDesc = Prelude.Nothing,
      groupId = Prelude.Nothing,
      groupName = Prelude.Nothing,
      userRules = Prelude.Nothing
    }

-- | The description of the group.
workspacesIpGroup_groupDesc :: Lens.Lens' WorkspacesIpGroup (Prelude.Maybe Prelude.Text)
workspacesIpGroup_groupDesc = Lens.lens (\WorkspacesIpGroup' {groupDesc} -> groupDesc) (\s@WorkspacesIpGroup' {} a -> s {groupDesc = a} :: WorkspacesIpGroup)

-- | The identifier of the group.
workspacesIpGroup_groupId :: Lens.Lens' WorkspacesIpGroup (Prelude.Maybe Prelude.Text)
workspacesIpGroup_groupId = Lens.lens (\WorkspacesIpGroup' {groupId} -> groupId) (\s@WorkspacesIpGroup' {} a -> s {groupId = a} :: WorkspacesIpGroup)

-- | The name of the group.
workspacesIpGroup_groupName :: Lens.Lens' WorkspacesIpGroup (Prelude.Maybe Prelude.Text)
workspacesIpGroup_groupName = Lens.lens (\WorkspacesIpGroup' {groupName} -> groupName) (\s@WorkspacesIpGroup' {} a -> s {groupName = a} :: WorkspacesIpGroup)

-- | The rules.
workspacesIpGroup_userRules :: Lens.Lens' WorkspacesIpGroup (Prelude.Maybe [IpRuleItem])
workspacesIpGroup_userRules = Lens.lens (\WorkspacesIpGroup' {userRules} -> userRules) (\s@WorkspacesIpGroup' {} a -> s {userRules = a} :: WorkspacesIpGroup) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON WorkspacesIpGroup where
  parseJSON =
    Data.withObject
      "WorkspacesIpGroup"
      ( \x ->
          WorkspacesIpGroup'
            Prelude.<$> (x Data..:? "groupDesc")
            Prelude.<*> (x Data..:? "groupId")
            Prelude.<*> (x Data..:? "groupName")
            Prelude.<*> (x Data..:? "userRules" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable WorkspacesIpGroup where
  hashWithSalt _salt WorkspacesIpGroup' {..} =
    _salt
      `Prelude.hashWithSalt` groupDesc
      `Prelude.hashWithSalt` groupId
      `Prelude.hashWithSalt` groupName
      `Prelude.hashWithSalt` userRules

instance Prelude.NFData WorkspacesIpGroup where
  rnf WorkspacesIpGroup' {..} =
    Prelude.rnf groupDesc
      `Prelude.seq` Prelude.rnf groupId
      `Prelude.seq` Prelude.rnf groupName
      `Prelude.seq` Prelude.rnf userRules
