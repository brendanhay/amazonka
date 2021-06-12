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
-- Module      : Network.AWS.EC2.Types.GroupIdentifier
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.GroupIdentifier where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Describes a security group.
--
-- /See:/ 'newGroupIdentifier' smart constructor.
data GroupIdentifier = GroupIdentifier'
  { -- | The name of the security group.
    groupName :: Core.Maybe Core.Text,
    -- | The ID of the security group.
    groupId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GroupIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupName', 'groupIdentifier_groupName' - The name of the security group.
--
-- 'groupId', 'groupIdentifier_groupId' - The ID of the security group.
newGroupIdentifier ::
  GroupIdentifier
newGroupIdentifier =
  GroupIdentifier'
    { groupName = Core.Nothing,
      groupId = Core.Nothing
    }

-- | The name of the security group.
groupIdentifier_groupName :: Lens.Lens' GroupIdentifier (Core.Maybe Core.Text)
groupIdentifier_groupName = Lens.lens (\GroupIdentifier' {groupName} -> groupName) (\s@GroupIdentifier' {} a -> s {groupName = a} :: GroupIdentifier)

-- | The ID of the security group.
groupIdentifier_groupId :: Lens.Lens' GroupIdentifier (Core.Maybe Core.Text)
groupIdentifier_groupId = Lens.lens (\GroupIdentifier' {groupId} -> groupId) (\s@GroupIdentifier' {} a -> s {groupId = a} :: GroupIdentifier)

instance Core.FromXML GroupIdentifier where
  parseXML x =
    GroupIdentifier'
      Core.<$> (x Core..@? "groupName")
      Core.<*> (x Core..@? "groupId")

instance Core.Hashable GroupIdentifier

instance Core.NFData GroupIdentifier

instance Core.ToQuery GroupIdentifier where
  toQuery GroupIdentifier' {..} =
    Core.mconcat
      [ "GroupName" Core.=: groupName,
        "GroupId" Core.=: groupId
      ]
