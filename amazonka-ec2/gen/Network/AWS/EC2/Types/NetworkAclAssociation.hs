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
-- Module      : Network.AWS.EC2.Types.NetworkAclAssociation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NetworkAclAssociation where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Describes an association between a network ACL and a subnet.
--
-- /See:/ 'newNetworkAclAssociation' smart constructor.
data NetworkAclAssociation = NetworkAclAssociation'
  { -- | The ID of the association between a network ACL and a subnet.
    networkAclAssociationId :: Core.Maybe Core.Text,
    -- | The ID of the subnet.
    subnetId :: Core.Maybe Core.Text,
    -- | The ID of the network ACL.
    networkAclId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'NetworkAclAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkAclAssociationId', 'networkAclAssociation_networkAclAssociationId' - The ID of the association between a network ACL and a subnet.
--
-- 'subnetId', 'networkAclAssociation_subnetId' - The ID of the subnet.
--
-- 'networkAclId', 'networkAclAssociation_networkAclId' - The ID of the network ACL.
newNetworkAclAssociation ::
  NetworkAclAssociation
newNetworkAclAssociation =
  NetworkAclAssociation'
    { networkAclAssociationId =
        Core.Nothing,
      subnetId = Core.Nothing,
      networkAclId = Core.Nothing
    }

-- | The ID of the association between a network ACL and a subnet.
networkAclAssociation_networkAclAssociationId :: Lens.Lens' NetworkAclAssociation (Core.Maybe Core.Text)
networkAclAssociation_networkAclAssociationId = Lens.lens (\NetworkAclAssociation' {networkAclAssociationId} -> networkAclAssociationId) (\s@NetworkAclAssociation' {} a -> s {networkAclAssociationId = a} :: NetworkAclAssociation)

-- | The ID of the subnet.
networkAclAssociation_subnetId :: Lens.Lens' NetworkAclAssociation (Core.Maybe Core.Text)
networkAclAssociation_subnetId = Lens.lens (\NetworkAclAssociation' {subnetId} -> subnetId) (\s@NetworkAclAssociation' {} a -> s {subnetId = a} :: NetworkAclAssociation)

-- | The ID of the network ACL.
networkAclAssociation_networkAclId :: Lens.Lens' NetworkAclAssociation (Core.Maybe Core.Text)
networkAclAssociation_networkAclId = Lens.lens (\NetworkAclAssociation' {networkAclId} -> networkAclId) (\s@NetworkAclAssociation' {} a -> s {networkAclId = a} :: NetworkAclAssociation)

instance Core.FromXML NetworkAclAssociation where
  parseXML x =
    NetworkAclAssociation'
      Core.<$> (x Core..@? "networkAclAssociationId")
      Core.<*> (x Core..@? "subnetId")
      Core.<*> (x Core..@? "networkAclId")

instance Core.Hashable NetworkAclAssociation

instance Core.NFData NetworkAclAssociation
