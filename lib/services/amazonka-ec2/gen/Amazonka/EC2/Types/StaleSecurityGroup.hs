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
-- Module      : Amazonka.EC2.Types.StaleSecurityGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.StaleSecurityGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.StaleIpPermission
import qualified Amazonka.Prelude as Prelude

-- | Describes a stale security group (a security group that contains stale
-- rules).
--
-- /See:/ 'newStaleSecurityGroup' smart constructor.
data StaleSecurityGroup = StaleSecurityGroup'
  { -- | Information about the stale inbound rules in the security group.
    staleIpPermissions :: Prelude.Maybe [StaleIpPermission],
    -- | The name of the security group.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | The description of the security group.
    description :: Prelude.Maybe Prelude.Text,
    -- | Information about the stale outbound rules in the security group.
    staleIpPermissionsEgress :: Prelude.Maybe [StaleIpPermission],
    -- | The ID of the VPC for the security group.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the security group.
    groupId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'description', 'staleSecurityGroup_description' - The description of the security group.
--
-- 'staleIpPermissionsEgress', 'staleSecurityGroup_staleIpPermissionsEgress' - Information about the stale outbound rules in the security group.
--
-- 'vpcId', 'staleSecurityGroup_vpcId' - The ID of the VPC for the security group.
--
-- 'groupId', 'staleSecurityGroup_groupId' - The ID of the security group.
newStaleSecurityGroup ::
  StaleSecurityGroup
newStaleSecurityGroup =
  StaleSecurityGroup'
    { staleIpPermissions =
        Prelude.Nothing,
      groupName = Prelude.Nothing,
      description = Prelude.Nothing,
      staleIpPermissionsEgress = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      groupId = Prelude.Nothing
    }

-- | Information about the stale inbound rules in the security group.
staleSecurityGroup_staleIpPermissions :: Lens.Lens' StaleSecurityGroup (Prelude.Maybe [StaleIpPermission])
staleSecurityGroup_staleIpPermissions = Lens.lens (\StaleSecurityGroup' {staleIpPermissions} -> staleIpPermissions) (\s@StaleSecurityGroup' {} a -> s {staleIpPermissions = a} :: StaleSecurityGroup) Prelude.. Lens.mapping Lens.coerced

-- | The name of the security group.
staleSecurityGroup_groupName :: Lens.Lens' StaleSecurityGroup (Prelude.Maybe Prelude.Text)
staleSecurityGroup_groupName = Lens.lens (\StaleSecurityGroup' {groupName} -> groupName) (\s@StaleSecurityGroup' {} a -> s {groupName = a} :: StaleSecurityGroup)

-- | The description of the security group.
staleSecurityGroup_description :: Lens.Lens' StaleSecurityGroup (Prelude.Maybe Prelude.Text)
staleSecurityGroup_description = Lens.lens (\StaleSecurityGroup' {description} -> description) (\s@StaleSecurityGroup' {} a -> s {description = a} :: StaleSecurityGroup)

-- | Information about the stale outbound rules in the security group.
staleSecurityGroup_staleIpPermissionsEgress :: Lens.Lens' StaleSecurityGroup (Prelude.Maybe [StaleIpPermission])
staleSecurityGroup_staleIpPermissionsEgress = Lens.lens (\StaleSecurityGroup' {staleIpPermissionsEgress} -> staleIpPermissionsEgress) (\s@StaleSecurityGroup' {} a -> s {staleIpPermissionsEgress = a} :: StaleSecurityGroup) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the VPC for the security group.
staleSecurityGroup_vpcId :: Lens.Lens' StaleSecurityGroup (Prelude.Maybe Prelude.Text)
staleSecurityGroup_vpcId = Lens.lens (\StaleSecurityGroup' {vpcId} -> vpcId) (\s@StaleSecurityGroup' {} a -> s {vpcId = a} :: StaleSecurityGroup)

-- | The ID of the security group.
staleSecurityGroup_groupId :: Lens.Lens' StaleSecurityGroup (Prelude.Maybe Prelude.Text)
staleSecurityGroup_groupId = Lens.lens (\StaleSecurityGroup' {groupId} -> groupId) (\s@StaleSecurityGroup' {} a -> s {groupId = a} :: StaleSecurityGroup)

instance Core.FromXML StaleSecurityGroup where
  parseXML x =
    StaleSecurityGroup'
      Prelude.<$> ( x Core..@? "staleIpPermissions"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "groupName")
      Prelude.<*> (x Core..@? "description")
      Prelude.<*> ( x Core..@? "staleIpPermissionsEgress"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "vpcId")
      Prelude.<*> (x Core..@? "groupId")

instance Prelude.Hashable StaleSecurityGroup where
  hashWithSalt _salt StaleSecurityGroup' {..} =
    _salt `Prelude.hashWithSalt` staleIpPermissions
      `Prelude.hashWithSalt` groupName
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` staleIpPermissionsEgress
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` groupId

instance Prelude.NFData StaleSecurityGroup where
  rnf StaleSecurityGroup' {..} =
    Prelude.rnf staleIpPermissions
      `Prelude.seq` Prelude.rnf groupName
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf staleIpPermissionsEgress
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf groupId
