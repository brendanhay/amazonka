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
-- Module      : Amazonka.EC2.Types.NetworkAclAssociation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.NetworkAclAssociation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes an association between a network ACL and a subnet.
--
-- /See:/ 'newNetworkAclAssociation' smart constructor.
data NetworkAclAssociation = NetworkAclAssociation'
  { -- | The ID of the network ACL.
    networkAclId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the subnet.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the association between a network ACL and a subnet.
    networkAclAssociationId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkAclAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkAclId', 'networkAclAssociation_networkAclId' - The ID of the network ACL.
--
-- 'subnetId', 'networkAclAssociation_subnetId' - The ID of the subnet.
--
-- 'networkAclAssociationId', 'networkAclAssociation_networkAclAssociationId' - The ID of the association between a network ACL and a subnet.
newNetworkAclAssociation ::
  NetworkAclAssociation
newNetworkAclAssociation =
  NetworkAclAssociation'
    { networkAclId =
        Prelude.Nothing,
      subnetId = Prelude.Nothing,
      networkAclAssociationId = Prelude.Nothing
    }

-- | The ID of the network ACL.
networkAclAssociation_networkAclId :: Lens.Lens' NetworkAclAssociation (Prelude.Maybe Prelude.Text)
networkAclAssociation_networkAclId = Lens.lens (\NetworkAclAssociation' {networkAclId} -> networkAclId) (\s@NetworkAclAssociation' {} a -> s {networkAclId = a} :: NetworkAclAssociation)

-- | The ID of the subnet.
networkAclAssociation_subnetId :: Lens.Lens' NetworkAclAssociation (Prelude.Maybe Prelude.Text)
networkAclAssociation_subnetId = Lens.lens (\NetworkAclAssociation' {subnetId} -> subnetId) (\s@NetworkAclAssociation' {} a -> s {subnetId = a} :: NetworkAclAssociation)

-- | The ID of the association between a network ACL and a subnet.
networkAclAssociation_networkAclAssociationId :: Lens.Lens' NetworkAclAssociation (Prelude.Maybe Prelude.Text)
networkAclAssociation_networkAclAssociationId = Lens.lens (\NetworkAclAssociation' {networkAclAssociationId} -> networkAclAssociationId) (\s@NetworkAclAssociation' {} a -> s {networkAclAssociationId = a} :: NetworkAclAssociation)

instance Data.FromXML NetworkAclAssociation where
  parseXML x =
    NetworkAclAssociation'
      Prelude.<$> (x Data..@? "networkAclId")
      Prelude.<*> (x Data..@? "subnetId")
      Prelude.<*> (x Data..@? "networkAclAssociationId")

instance Prelude.Hashable NetworkAclAssociation where
  hashWithSalt _salt NetworkAclAssociation' {..} =
    _salt `Prelude.hashWithSalt` networkAclId
      `Prelude.hashWithSalt` subnetId
      `Prelude.hashWithSalt` networkAclAssociationId

instance Prelude.NFData NetworkAclAssociation where
  rnf NetworkAclAssociation' {..} =
    Prelude.rnf networkAclId
      `Prelude.seq` Prelude.rnf subnetId
      `Prelude.seq` Prelude.rnf networkAclAssociationId
