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
-- Module      : Amazonka.EC2.Types.ReferencedSecurityGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ReferencedSecurityGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes the security group that is referenced in the security group
-- rule.
--
-- /See:/ 'newReferencedSecurityGroup' smart constructor.
data ReferencedSecurityGroup = ReferencedSecurityGroup'
  { -- | The ID of the VPC peering connection.
    vpcPeeringConnectionId :: Prelude.Maybe Prelude.Text,
    -- | The status of a VPC peering connection, if applicable.
    peeringStatus :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID.
    userId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the VPC.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the security group.
    groupId :: Prelude.Maybe Prelude.Text
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
-- 'peeringStatus', 'referencedSecurityGroup_peeringStatus' - The status of a VPC peering connection, if applicable.
--
-- 'userId', 'referencedSecurityGroup_userId' - The Amazon Web Services account ID.
--
-- 'vpcId', 'referencedSecurityGroup_vpcId' - The ID of the VPC.
--
-- 'groupId', 'referencedSecurityGroup_groupId' - The ID of the security group.
newReferencedSecurityGroup ::
  ReferencedSecurityGroup
newReferencedSecurityGroup =
  ReferencedSecurityGroup'
    { vpcPeeringConnectionId =
        Prelude.Nothing,
      peeringStatus = Prelude.Nothing,
      userId = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      groupId = Prelude.Nothing
    }

-- | The ID of the VPC peering connection.
referencedSecurityGroup_vpcPeeringConnectionId :: Lens.Lens' ReferencedSecurityGroup (Prelude.Maybe Prelude.Text)
referencedSecurityGroup_vpcPeeringConnectionId = Lens.lens (\ReferencedSecurityGroup' {vpcPeeringConnectionId} -> vpcPeeringConnectionId) (\s@ReferencedSecurityGroup' {} a -> s {vpcPeeringConnectionId = a} :: ReferencedSecurityGroup)

-- | The status of a VPC peering connection, if applicable.
referencedSecurityGroup_peeringStatus :: Lens.Lens' ReferencedSecurityGroup (Prelude.Maybe Prelude.Text)
referencedSecurityGroup_peeringStatus = Lens.lens (\ReferencedSecurityGroup' {peeringStatus} -> peeringStatus) (\s@ReferencedSecurityGroup' {} a -> s {peeringStatus = a} :: ReferencedSecurityGroup)

-- | The Amazon Web Services account ID.
referencedSecurityGroup_userId :: Lens.Lens' ReferencedSecurityGroup (Prelude.Maybe Prelude.Text)
referencedSecurityGroup_userId = Lens.lens (\ReferencedSecurityGroup' {userId} -> userId) (\s@ReferencedSecurityGroup' {} a -> s {userId = a} :: ReferencedSecurityGroup)

-- | The ID of the VPC.
referencedSecurityGroup_vpcId :: Lens.Lens' ReferencedSecurityGroup (Prelude.Maybe Prelude.Text)
referencedSecurityGroup_vpcId = Lens.lens (\ReferencedSecurityGroup' {vpcId} -> vpcId) (\s@ReferencedSecurityGroup' {} a -> s {vpcId = a} :: ReferencedSecurityGroup)

-- | The ID of the security group.
referencedSecurityGroup_groupId :: Lens.Lens' ReferencedSecurityGroup (Prelude.Maybe Prelude.Text)
referencedSecurityGroup_groupId = Lens.lens (\ReferencedSecurityGroup' {groupId} -> groupId) (\s@ReferencedSecurityGroup' {} a -> s {groupId = a} :: ReferencedSecurityGroup)

instance Core.FromXML ReferencedSecurityGroup where
  parseXML x =
    ReferencedSecurityGroup'
      Prelude.<$> (x Core..@? "vpcPeeringConnectionId")
      Prelude.<*> (x Core..@? "peeringStatus")
      Prelude.<*> (x Core..@? "userId")
      Prelude.<*> (x Core..@? "vpcId")
      Prelude.<*> (x Core..@? "groupId")

instance Prelude.Hashable ReferencedSecurityGroup where
  hashWithSalt _salt ReferencedSecurityGroup' {..} =
    _salt `Prelude.hashWithSalt` vpcPeeringConnectionId
      `Prelude.hashWithSalt` peeringStatus
      `Prelude.hashWithSalt` userId
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` groupId

instance Prelude.NFData ReferencedSecurityGroup where
  rnf ReferencedSecurityGroup' {..} =
    Prelude.rnf vpcPeeringConnectionId
      `Prelude.seq` Prelude.rnf peeringStatus
      `Prelude.seq` Prelude.rnf userId
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf groupId
