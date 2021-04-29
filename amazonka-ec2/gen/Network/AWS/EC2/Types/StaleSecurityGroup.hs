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
-- Module      : Network.AWS.EC2.Types.StaleSecurityGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.StaleSecurityGroup where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.StaleIpPermission
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a stale security group (a security group that contains stale
-- rules).
--
-- /See:/ 'newStaleSecurityGroup' smart constructor.
data StaleSecurityGroup = StaleSecurityGroup'
  { -- | Information about the stale inbound rules in the security group.
    staleIpPermissions :: Prelude.Maybe [StaleIpPermission],
    -- | The name of the security group.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the security group.
    groupId :: Prelude.Maybe Prelude.Text,
    -- | The description of the security group.
    description :: Prelude.Maybe Prelude.Text,
    -- | Information about the stale outbound rules in the security group.
    staleIpPermissionsEgress :: Prelude.Maybe [StaleIpPermission],
    -- | The ID of the VPC for the security group.
    vpcId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StaleSecurityGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'staleIpPermissions', 'staleSecurityGroup_staleIpPermissions' - Information about the stale inbound rules in the security group.
--
-- 'groupName', 'staleSecurityGroup_groupName' - The name of the security group.
--
-- 'groupId', 'staleSecurityGroup_groupId' - The ID of the security group.
--
-- 'description', 'staleSecurityGroup_description' - The description of the security group.
--
-- 'staleIpPermissionsEgress', 'staleSecurityGroup_staleIpPermissionsEgress' - Information about the stale outbound rules in the security group.
--
-- 'vpcId', 'staleSecurityGroup_vpcId' - The ID of the VPC for the security group.
newStaleSecurityGroup ::
  StaleSecurityGroup
newStaleSecurityGroup =
  StaleSecurityGroup'
    { staleIpPermissions =
        Prelude.Nothing,
      groupName = Prelude.Nothing,
      groupId = Prelude.Nothing,
      description = Prelude.Nothing,
      staleIpPermissionsEgress = Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | Information about the stale inbound rules in the security group.
staleSecurityGroup_staleIpPermissions :: Lens.Lens' StaleSecurityGroup (Prelude.Maybe [StaleIpPermission])
staleSecurityGroup_staleIpPermissions = Lens.lens (\StaleSecurityGroup' {staleIpPermissions} -> staleIpPermissions) (\s@StaleSecurityGroup' {} a -> s {staleIpPermissions = a} :: StaleSecurityGroup) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the security group.
staleSecurityGroup_groupName :: Lens.Lens' StaleSecurityGroup (Prelude.Maybe Prelude.Text)
staleSecurityGroup_groupName = Lens.lens (\StaleSecurityGroup' {groupName} -> groupName) (\s@StaleSecurityGroup' {} a -> s {groupName = a} :: StaleSecurityGroup)

-- | The ID of the security group.
staleSecurityGroup_groupId :: Lens.Lens' StaleSecurityGroup (Prelude.Maybe Prelude.Text)
staleSecurityGroup_groupId = Lens.lens (\StaleSecurityGroup' {groupId} -> groupId) (\s@StaleSecurityGroup' {} a -> s {groupId = a} :: StaleSecurityGroup)

-- | The description of the security group.
staleSecurityGroup_description :: Lens.Lens' StaleSecurityGroup (Prelude.Maybe Prelude.Text)
staleSecurityGroup_description = Lens.lens (\StaleSecurityGroup' {description} -> description) (\s@StaleSecurityGroup' {} a -> s {description = a} :: StaleSecurityGroup)

-- | Information about the stale outbound rules in the security group.
staleSecurityGroup_staleIpPermissionsEgress :: Lens.Lens' StaleSecurityGroup (Prelude.Maybe [StaleIpPermission])
staleSecurityGroup_staleIpPermissionsEgress = Lens.lens (\StaleSecurityGroup' {staleIpPermissionsEgress} -> staleIpPermissionsEgress) (\s@StaleSecurityGroup' {} a -> s {staleIpPermissionsEgress = a} :: StaleSecurityGroup) Prelude.. Lens.mapping Prelude._Coerce

-- | The ID of the VPC for the security group.
staleSecurityGroup_vpcId :: Lens.Lens' StaleSecurityGroup (Prelude.Maybe Prelude.Text)
staleSecurityGroup_vpcId = Lens.lens (\StaleSecurityGroup' {vpcId} -> vpcId) (\s@StaleSecurityGroup' {} a -> s {vpcId = a} :: StaleSecurityGroup)

instance Prelude.FromXML StaleSecurityGroup where
  parseXML x =
    StaleSecurityGroup'
      Prelude.<$> ( x Prelude..@? "staleIpPermissions"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "groupName")
      Prelude.<*> (x Prelude..@? "groupId")
      Prelude.<*> (x Prelude..@? "description")
      Prelude.<*> ( x Prelude..@? "staleIpPermissionsEgress"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "vpcId")

instance Prelude.Hashable StaleSecurityGroup

instance Prelude.NFData StaleSecurityGroup
