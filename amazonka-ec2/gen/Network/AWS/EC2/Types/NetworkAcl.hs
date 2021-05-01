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
-- Module      : Network.AWS.EC2.Types.NetworkAcl
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NetworkAcl where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.NetworkAclAssociation
import Network.AWS.EC2.Types.NetworkAclEntry
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a network ACL.
--
-- /See:/ 'newNetworkAcl' smart constructor.
data NetworkAcl = NetworkAcl'
  { -- | The ID of the AWS account that owns the network ACL.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether this is the default network ACL for the VPC.
    isDefault :: Prelude.Maybe Prelude.Bool,
    -- | Any tags assigned to the network ACL.
    tags :: Prelude.Maybe [Tag],
    -- | The ID of the VPC for the network ACL.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the network ACL.
    networkAclId :: Prelude.Maybe Prelude.Text,
    -- | Any associations between the network ACL and one or more subnets
    associations :: Prelude.Maybe [NetworkAclAssociation],
    -- | One or more entries (rules) in the network ACL.
    entries :: Prelude.Maybe [NetworkAclEntry]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { ownerId = Prelude.Nothing,
      isDefault = Prelude.Nothing,
      tags = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      networkAclId = Prelude.Nothing,
      associations = Prelude.Nothing,
      entries = Prelude.Nothing
    }

-- | The ID of the AWS account that owns the network ACL.
networkAcl_ownerId :: Lens.Lens' NetworkAcl (Prelude.Maybe Prelude.Text)
networkAcl_ownerId = Lens.lens (\NetworkAcl' {ownerId} -> ownerId) (\s@NetworkAcl' {} a -> s {ownerId = a} :: NetworkAcl)

-- | Indicates whether this is the default network ACL for the VPC.
networkAcl_isDefault :: Lens.Lens' NetworkAcl (Prelude.Maybe Prelude.Bool)
networkAcl_isDefault = Lens.lens (\NetworkAcl' {isDefault} -> isDefault) (\s@NetworkAcl' {} a -> s {isDefault = a} :: NetworkAcl)

-- | Any tags assigned to the network ACL.
networkAcl_tags :: Lens.Lens' NetworkAcl (Prelude.Maybe [Tag])
networkAcl_tags = Lens.lens (\NetworkAcl' {tags} -> tags) (\s@NetworkAcl' {} a -> s {tags = a} :: NetworkAcl) Prelude.. Lens.mapping Prelude._Coerce

-- | The ID of the VPC for the network ACL.
networkAcl_vpcId :: Lens.Lens' NetworkAcl (Prelude.Maybe Prelude.Text)
networkAcl_vpcId = Lens.lens (\NetworkAcl' {vpcId} -> vpcId) (\s@NetworkAcl' {} a -> s {vpcId = a} :: NetworkAcl)

-- | The ID of the network ACL.
networkAcl_networkAclId :: Lens.Lens' NetworkAcl (Prelude.Maybe Prelude.Text)
networkAcl_networkAclId = Lens.lens (\NetworkAcl' {networkAclId} -> networkAclId) (\s@NetworkAcl' {} a -> s {networkAclId = a} :: NetworkAcl)

-- | Any associations between the network ACL and one or more subnets
networkAcl_associations :: Lens.Lens' NetworkAcl (Prelude.Maybe [NetworkAclAssociation])
networkAcl_associations = Lens.lens (\NetworkAcl' {associations} -> associations) (\s@NetworkAcl' {} a -> s {associations = a} :: NetworkAcl) Prelude.. Lens.mapping Prelude._Coerce

-- | One or more entries (rules) in the network ACL.
networkAcl_entries :: Lens.Lens' NetworkAcl (Prelude.Maybe [NetworkAclEntry])
networkAcl_entries = Lens.lens (\NetworkAcl' {entries} -> entries) (\s@NetworkAcl' {} a -> s {entries = a} :: NetworkAcl) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromXML NetworkAcl where
  parseXML x =
    NetworkAcl'
      Prelude.<$> (x Prelude..@? "ownerId")
      Prelude.<*> (x Prelude..@? "default")
      Prelude.<*> ( x Prelude..@? "tagSet" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "vpcId")
      Prelude.<*> (x Prelude..@? "networkAclId")
      Prelude.<*> ( x Prelude..@? "associationSet"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> ( x Prelude..@? "entrySet" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )

instance Prelude.Hashable NetworkAcl

instance Prelude.NFData NetworkAcl
