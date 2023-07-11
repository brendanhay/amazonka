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
-- Module      : Amazonka.EC2.Types.NetworkAcl
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.NetworkAcl where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.NetworkAclAssociation
import Amazonka.EC2.Types.NetworkAclEntry
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes a network ACL.
--
-- /See:/ 'newNetworkAcl' smart constructor.
data NetworkAcl = NetworkAcl'
  { -- | Any associations between the network ACL and one or more subnets
    associations :: Prelude.Maybe [NetworkAclAssociation],
    -- | One or more entries (rules) in the network ACL.
    entries :: Prelude.Maybe [NetworkAclEntry],
    -- | Indicates whether this is the default network ACL for the VPC.
    isDefault :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the network ACL.
    networkAclId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services account that owns the network ACL.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | Any tags assigned to the network ACL.
    tags :: Prelude.Maybe [Tag],
    -- | The ID of the VPC for the network ACL.
    vpcId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkAcl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associations', 'networkAcl_associations' - Any associations between the network ACL and one or more subnets
--
-- 'entries', 'networkAcl_entries' - One or more entries (rules) in the network ACL.
--
-- 'isDefault', 'networkAcl_isDefault' - Indicates whether this is the default network ACL for the VPC.
--
-- 'networkAclId', 'networkAcl_networkAclId' - The ID of the network ACL.
--
-- 'ownerId', 'networkAcl_ownerId' - The ID of the Amazon Web Services account that owns the network ACL.
--
-- 'tags', 'networkAcl_tags' - Any tags assigned to the network ACL.
--
-- 'vpcId', 'networkAcl_vpcId' - The ID of the VPC for the network ACL.
newNetworkAcl ::
  NetworkAcl
newNetworkAcl =
  NetworkAcl'
    { associations = Prelude.Nothing,
      entries = Prelude.Nothing,
      isDefault = Prelude.Nothing,
      networkAclId = Prelude.Nothing,
      ownerId = Prelude.Nothing,
      tags = Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | Any associations between the network ACL and one or more subnets
networkAcl_associations :: Lens.Lens' NetworkAcl (Prelude.Maybe [NetworkAclAssociation])
networkAcl_associations = Lens.lens (\NetworkAcl' {associations} -> associations) (\s@NetworkAcl' {} a -> s {associations = a} :: NetworkAcl) Prelude.. Lens.mapping Lens.coerced

-- | One or more entries (rules) in the network ACL.
networkAcl_entries :: Lens.Lens' NetworkAcl (Prelude.Maybe [NetworkAclEntry])
networkAcl_entries = Lens.lens (\NetworkAcl' {entries} -> entries) (\s@NetworkAcl' {} a -> s {entries = a} :: NetworkAcl) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether this is the default network ACL for the VPC.
networkAcl_isDefault :: Lens.Lens' NetworkAcl (Prelude.Maybe Prelude.Bool)
networkAcl_isDefault = Lens.lens (\NetworkAcl' {isDefault} -> isDefault) (\s@NetworkAcl' {} a -> s {isDefault = a} :: NetworkAcl)

-- | The ID of the network ACL.
networkAcl_networkAclId :: Lens.Lens' NetworkAcl (Prelude.Maybe Prelude.Text)
networkAcl_networkAclId = Lens.lens (\NetworkAcl' {networkAclId} -> networkAclId) (\s@NetworkAcl' {} a -> s {networkAclId = a} :: NetworkAcl)

-- | The ID of the Amazon Web Services account that owns the network ACL.
networkAcl_ownerId :: Lens.Lens' NetworkAcl (Prelude.Maybe Prelude.Text)
networkAcl_ownerId = Lens.lens (\NetworkAcl' {ownerId} -> ownerId) (\s@NetworkAcl' {} a -> s {ownerId = a} :: NetworkAcl)

-- | Any tags assigned to the network ACL.
networkAcl_tags :: Lens.Lens' NetworkAcl (Prelude.Maybe [Tag])
networkAcl_tags = Lens.lens (\NetworkAcl' {tags} -> tags) (\s@NetworkAcl' {} a -> s {tags = a} :: NetworkAcl) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the VPC for the network ACL.
networkAcl_vpcId :: Lens.Lens' NetworkAcl (Prelude.Maybe Prelude.Text)
networkAcl_vpcId = Lens.lens (\NetworkAcl' {vpcId} -> vpcId) (\s@NetworkAcl' {} a -> s {vpcId = a} :: NetworkAcl)

instance Data.FromXML NetworkAcl where
  parseXML x =
    NetworkAcl'
      Prelude.<$> ( x
                      Data..@? "associationSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> ( x
                      Data..@? "entrySet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "default")
      Prelude.<*> (x Data..@? "networkAclId")
      Prelude.<*> (x Data..@? "ownerId")
      Prelude.<*> ( x
                      Data..@? "tagSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "vpcId")

instance Prelude.Hashable NetworkAcl where
  hashWithSalt _salt NetworkAcl' {..} =
    _salt
      `Prelude.hashWithSalt` associations
      `Prelude.hashWithSalt` entries
      `Prelude.hashWithSalt` isDefault
      `Prelude.hashWithSalt` networkAclId
      `Prelude.hashWithSalt` ownerId
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData NetworkAcl where
  rnf NetworkAcl' {..} =
    Prelude.rnf associations
      `Prelude.seq` Prelude.rnf entries
      `Prelude.seq` Prelude.rnf isDefault
      `Prelude.seq` Prelude.rnf networkAclId
      `Prelude.seq` Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf vpcId
