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
-- Module      : Network.AWS.EC2.Types.NetworkAcl
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NetworkAcl where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.NetworkAclAssociation
import Network.AWS.EC2.Types.NetworkAclEntry
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens

-- | Describes a network ACL.
--
-- /See:/ 'newNetworkAcl' smart constructor.
data NetworkAcl = NetworkAcl'
  { -- | The ID of the AWS account that owns the network ACL.
    ownerId :: Core.Maybe Core.Text,
    -- | Indicates whether this is the default network ACL for the VPC.
    isDefault :: Core.Maybe Core.Bool,
    -- | Any tags assigned to the network ACL.
    tags :: Core.Maybe [Tag],
    -- | The ID of the VPC for the network ACL.
    vpcId :: Core.Maybe Core.Text,
    -- | The ID of the network ACL.
    networkAclId :: Core.Maybe Core.Text,
    -- | Any associations between the network ACL and one or more subnets
    associations :: Core.Maybe [NetworkAclAssociation],
    -- | One or more entries (rules) in the network ACL.
    entries :: Core.Maybe [NetworkAclEntry]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'NetworkAcl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ownerId', 'networkAcl_ownerId' - The ID of the AWS account that owns the network ACL.
--
-- 'isDefault', 'networkAcl_isDefault' - Indicates whether this is the default network ACL for the VPC.
--
-- 'tags', 'networkAcl_tags' - Any tags assigned to the network ACL.
--
-- 'vpcId', 'networkAcl_vpcId' - The ID of the VPC for the network ACL.
--
-- 'networkAclId', 'networkAcl_networkAclId' - The ID of the network ACL.
--
-- 'associations', 'networkAcl_associations' - Any associations between the network ACL and one or more subnets
--
-- 'entries', 'networkAcl_entries' - One or more entries (rules) in the network ACL.
newNetworkAcl ::
  NetworkAcl
newNetworkAcl =
  NetworkAcl'
    { ownerId = Core.Nothing,
      isDefault = Core.Nothing,
      tags = Core.Nothing,
      vpcId = Core.Nothing,
      networkAclId = Core.Nothing,
      associations = Core.Nothing,
      entries = Core.Nothing
    }

-- | The ID of the AWS account that owns the network ACL.
networkAcl_ownerId :: Lens.Lens' NetworkAcl (Core.Maybe Core.Text)
networkAcl_ownerId = Lens.lens (\NetworkAcl' {ownerId} -> ownerId) (\s@NetworkAcl' {} a -> s {ownerId = a} :: NetworkAcl)

-- | Indicates whether this is the default network ACL for the VPC.
networkAcl_isDefault :: Lens.Lens' NetworkAcl (Core.Maybe Core.Bool)
networkAcl_isDefault = Lens.lens (\NetworkAcl' {isDefault} -> isDefault) (\s@NetworkAcl' {} a -> s {isDefault = a} :: NetworkAcl)

-- | Any tags assigned to the network ACL.
networkAcl_tags :: Lens.Lens' NetworkAcl (Core.Maybe [Tag])
networkAcl_tags = Lens.lens (\NetworkAcl' {tags} -> tags) (\s@NetworkAcl' {} a -> s {tags = a} :: NetworkAcl) Core.. Lens.mapping Lens._Coerce

-- | The ID of the VPC for the network ACL.
networkAcl_vpcId :: Lens.Lens' NetworkAcl (Core.Maybe Core.Text)
networkAcl_vpcId = Lens.lens (\NetworkAcl' {vpcId} -> vpcId) (\s@NetworkAcl' {} a -> s {vpcId = a} :: NetworkAcl)

-- | The ID of the network ACL.
networkAcl_networkAclId :: Lens.Lens' NetworkAcl (Core.Maybe Core.Text)
networkAcl_networkAclId = Lens.lens (\NetworkAcl' {networkAclId} -> networkAclId) (\s@NetworkAcl' {} a -> s {networkAclId = a} :: NetworkAcl)

-- | Any associations between the network ACL and one or more subnets
networkAcl_associations :: Lens.Lens' NetworkAcl (Core.Maybe [NetworkAclAssociation])
networkAcl_associations = Lens.lens (\NetworkAcl' {associations} -> associations) (\s@NetworkAcl' {} a -> s {associations = a} :: NetworkAcl) Core.. Lens.mapping Lens._Coerce

-- | One or more entries (rules) in the network ACL.
networkAcl_entries :: Lens.Lens' NetworkAcl (Core.Maybe [NetworkAclEntry])
networkAcl_entries = Lens.lens (\NetworkAcl' {entries} -> entries) (\s@NetworkAcl' {} a -> s {entries = a} :: NetworkAcl) Core.. Lens.mapping Lens._Coerce

instance Core.FromXML NetworkAcl where
  parseXML x =
    NetworkAcl'
      Core.<$> (x Core..@? "ownerId")
      Core.<*> (x Core..@? "default")
      Core.<*> ( x Core..@? "tagSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "vpcId")
      Core.<*> (x Core..@? "networkAclId")
      Core.<*> ( x Core..@? "associationSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> ( x Core..@? "entrySet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )

instance Core.Hashable NetworkAcl

instance Core.NFData NetworkAcl
