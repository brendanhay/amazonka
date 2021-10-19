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
-- Module      : Network.AWS.EC2.Types.ReferencedSecurityGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ReferencedSecurityGroup where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the security group that is referenced in the security group
-- rule.
--
-- /See:/ 'newReferencedSecurityGroup' smart constructor.
data ReferencedSecurityGroup = ReferencedSecurityGroup'
  { -- | The ID of the VPC peering connection.
    vpcPeeringConnectionId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the VPC.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID.
    userId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the security group.
    groupId :: Prelude.Maybe Prelude.Text,
    -- | The status of a VPC peering connection, if applicable.
    peeringStatus :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReferencedSecurityGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcPeeringConnectionId', 'referencedSecurityGroup_vpcPeeringConnectionId' - The ID of the VPC peering connection.
--
-- 'vpcId', 'referencedSecurityGroup_vpcId' - The ID of the VPC.
--
-- 'userId', 'referencedSecurityGroup_userId' - The Amazon Web Services account ID.
--
-- 'groupId', 'referencedSecurityGroup_groupId' - The ID of the security group.
--
-- 'peeringStatus', 'referencedSecurityGroup_peeringStatus' - The status of a VPC peering connection, if applicable.
newReferencedSecurityGroup ::
  ReferencedSecurityGroup
newReferencedSecurityGroup =
  ReferencedSecurityGroup'
    { vpcPeeringConnectionId =
        Prelude.Nothing,
      vpcId = Prelude.Nothing,
      userId = Prelude.Nothing,
      groupId = Prelude.Nothing,
      peeringStatus = Prelude.Nothing
    }

-- | The ID of the VPC peering connection.
referencedSecurityGroup_vpcPeeringConnectionId :: Lens.Lens' ReferencedSecurityGroup (Prelude.Maybe Prelude.Text)
referencedSecurityGroup_vpcPeeringConnectionId = Lens.lens (\ReferencedSecurityGroup' {vpcPeeringConnectionId} -> vpcPeeringConnectionId) (\s@ReferencedSecurityGroup' {} a -> s {vpcPeeringConnectionId = a} :: ReferencedSecurityGroup)

-- | The ID of the VPC.
referencedSecurityGroup_vpcId :: Lens.Lens' ReferencedSecurityGroup (Prelude.Maybe Prelude.Text)
referencedSecurityGroup_vpcId = Lens.lens (\ReferencedSecurityGroup' {vpcId} -> vpcId) (\s@ReferencedSecurityGroup' {} a -> s {vpcId = a} :: ReferencedSecurityGroup)

-- | The Amazon Web Services account ID.
referencedSecurityGroup_userId :: Lens.Lens' ReferencedSecurityGroup (Prelude.Maybe Prelude.Text)
referencedSecurityGroup_userId = Lens.lens (\ReferencedSecurityGroup' {userId} -> userId) (\s@ReferencedSecurityGroup' {} a -> s {userId = a} :: ReferencedSecurityGroup)

-- | The ID of the security group.
referencedSecurityGroup_groupId :: Lens.Lens' ReferencedSecurityGroup (Prelude.Maybe Prelude.Text)
referencedSecurityGroup_groupId = Lens.lens (\ReferencedSecurityGroup' {groupId} -> groupId) (\s@ReferencedSecurityGroup' {} a -> s {groupId = a} :: ReferencedSecurityGroup)

-- | The status of a VPC peering connection, if applicable.
referencedSecurityGroup_peeringStatus :: Lens.Lens' ReferencedSecurityGroup (Prelude.Maybe Prelude.Text)
referencedSecurityGroup_peeringStatus = Lens.lens (\ReferencedSecurityGroup' {peeringStatus} -> peeringStatus) (\s@ReferencedSecurityGroup' {} a -> s {peeringStatus = a} :: ReferencedSecurityGroup)

instance Core.FromXML ReferencedSecurityGroup where
  parseXML x =
    ReferencedSecurityGroup'
      Prelude.<$> (x Core..@? "vpcPeeringConnectionId")
      Prelude.<*> (x Core..@? "vpcId")
      Prelude.<*> (x Core..@? "userId")
      Prelude.<*> (x Core..@? "groupId")
      Prelude.<*> (x Core..@? "peeringStatus")

instance Prelude.Hashable ReferencedSecurityGroup

instance Prelude.NFData ReferencedSecurityGroup
