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
-- Module      : Network.AWS.Inspector.Types.SecurityGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.SecurityGroup where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains information about a security group associated with a network
-- interface. This data type is used as one of the elements of the
-- NetworkInterface data type.
--
-- /See:/ 'newSecurityGroup' smart constructor.
data SecurityGroup = SecurityGroup'
  { -- | The name of the security group.
    groupName :: Core.Maybe Core.Text,
    -- | The ID of the security group.
    groupId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SecurityGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupName', 'securityGroup_groupName' - The name of the security group.
--
-- 'groupId', 'securityGroup_groupId' - The ID of the security group.
newSecurityGroup ::
  SecurityGroup
newSecurityGroup =
  SecurityGroup'
    { groupName = Core.Nothing,
      groupId = Core.Nothing
    }

-- | The name of the security group.
securityGroup_groupName :: Lens.Lens' SecurityGroup (Core.Maybe Core.Text)
securityGroup_groupName = Lens.lens (\SecurityGroup' {groupName} -> groupName) (\s@SecurityGroup' {} a -> s {groupName = a} :: SecurityGroup)

-- | The ID of the security group.
securityGroup_groupId :: Lens.Lens' SecurityGroup (Core.Maybe Core.Text)
securityGroup_groupId = Lens.lens (\SecurityGroup' {groupId} -> groupId) (\s@SecurityGroup' {} a -> s {groupId = a} :: SecurityGroup)

instance Core.FromJSON SecurityGroup where
  parseJSON =
    Core.withObject
      "SecurityGroup"
      ( \x ->
          SecurityGroup'
            Core.<$> (x Core..:? "groupName")
            Core.<*> (x Core..:? "groupId")
      )

instance Core.Hashable SecurityGroup

instance Core.NFData SecurityGroup
