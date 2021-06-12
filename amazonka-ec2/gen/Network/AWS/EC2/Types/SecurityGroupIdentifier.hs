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
-- Module      : Network.AWS.EC2.Types.SecurityGroupIdentifier
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SecurityGroupIdentifier where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Describes a security group.
--
-- /See:/ 'newSecurityGroupIdentifier' smart constructor.
data SecurityGroupIdentifier = SecurityGroupIdentifier'
  { -- | The name of the security group.
    groupName :: Core.Maybe Core.Text,
    -- | The ID of the security group.
    groupId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SecurityGroupIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupName', 'securityGroupIdentifier_groupName' - The name of the security group.
--
-- 'groupId', 'securityGroupIdentifier_groupId' - The ID of the security group.
newSecurityGroupIdentifier ::
  SecurityGroupIdentifier
newSecurityGroupIdentifier =
  SecurityGroupIdentifier'
    { groupName = Core.Nothing,
      groupId = Core.Nothing
    }

-- | The name of the security group.
securityGroupIdentifier_groupName :: Lens.Lens' SecurityGroupIdentifier (Core.Maybe Core.Text)
securityGroupIdentifier_groupName = Lens.lens (\SecurityGroupIdentifier' {groupName} -> groupName) (\s@SecurityGroupIdentifier' {} a -> s {groupName = a} :: SecurityGroupIdentifier)

-- | The ID of the security group.
securityGroupIdentifier_groupId :: Lens.Lens' SecurityGroupIdentifier (Core.Maybe Core.Text)
securityGroupIdentifier_groupId = Lens.lens (\SecurityGroupIdentifier' {groupId} -> groupId) (\s@SecurityGroupIdentifier' {} a -> s {groupId = a} :: SecurityGroupIdentifier)

instance Core.FromXML SecurityGroupIdentifier where
  parseXML x =
    SecurityGroupIdentifier'
      Core.<$> (x Core..@? "groupName")
      Core.<*> (x Core..@? "groupId")

instance Core.Hashable SecurityGroupIdentifier

instance Core.NFData SecurityGroupIdentifier
