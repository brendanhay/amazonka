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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2NetworkAclAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2NetworkAclAssociation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An association between the network ACL and a subnet.
--
-- /See:/ 'newAwsEc2NetworkAclAssociation' smart constructor.
data AwsEc2NetworkAclAssociation = AwsEc2NetworkAclAssociation'
  { -- | The identifier of the association between the network ACL and the
    -- subnet.
    networkAclAssociationId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the network ACL.
    networkAclId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the subnet that is associated with the network ACL.
    subnetId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2NetworkAclAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkAclAssociationId', 'awsEc2NetworkAclAssociation_networkAclAssociationId' - The identifier of the association between the network ACL and the
-- subnet.
--
-- 'networkAclId', 'awsEc2NetworkAclAssociation_networkAclId' - The identifier of the network ACL.
--
-- 'subnetId', 'awsEc2NetworkAclAssociation_subnetId' - The identifier of the subnet that is associated with the network ACL.
newAwsEc2NetworkAclAssociation ::
  AwsEc2NetworkAclAssociation
newAwsEc2NetworkAclAssociation =
  AwsEc2NetworkAclAssociation'
    { networkAclAssociationId =
        Prelude.Nothing,
      networkAclId = Prelude.Nothing,
      subnetId = Prelude.Nothing
    }

-- | The identifier of the association between the network ACL and the
-- subnet.
awsEc2NetworkAclAssociation_networkAclAssociationId :: Lens.Lens' AwsEc2NetworkAclAssociation (Prelude.Maybe Prelude.Text)
awsEc2NetworkAclAssociation_networkAclAssociationId = Lens.lens (\AwsEc2NetworkAclAssociation' {networkAclAssociationId} -> networkAclAssociationId) (\s@AwsEc2NetworkAclAssociation' {} a -> s {networkAclAssociationId = a} :: AwsEc2NetworkAclAssociation)

-- | The identifier of the network ACL.
awsEc2NetworkAclAssociation_networkAclId :: Lens.Lens' AwsEc2NetworkAclAssociation (Prelude.Maybe Prelude.Text)
awsEc2NetworkAclAssociation_networkAclId = Lens.lens (\AwsEc2NetworkAclAssociation' {networkAclId} -> networkAclId) (\s@AwsEc2NetworkAclAssociation' {} a -> s {networkAclId = a} :: AwsEc2NetworkAclAssociation)

-- | The identifier of the subnet that is associated with the network ACL.
awsEc2NetworkAclAssociation_subnetId :: Lens.Lens' AwsEc2NetworkAclAssociation (Prelude.Maybe Prelude.Text)
awsEc2NetworkAclAssociation_subnetId = Lens.lens (\AwsEc2NetworkAclAssociation' {subnetId} -> subnetId) (\s@AwsEc2NetworkAclAssociation' {} a -> s {subnetId = a} :: AwsEc2NetworkAclAssociation)

instance Data.FromJSON AwsEc2NetworkAclAssociation where
  parseJSON =
    Data.withObject
      "AwsEc2NetworkAclAssociation"
      ( \x ->
          AwsEc2NetworkAclAssociation'
            Prelude.<$> (x Data..:? "NetworkAclAssociationId")
            Prelude.<*> (x Data..:? "NetworkAclId")
            Prelude.<*> (x Data..:? "SubnetId")
      )

instance Prelude.Hashable AwsEc2NetworkAclAssociation where
  hashWithSalt _salt AwsEc2NetworkAclAssociation' {..} =
    _salt
      `Prelude.hashWithSalt` networkAclAssociationId
      `Prelude.hashWithSalt` networkAclId
      `Prelude.hashWithSalt` subnetId

instance Prelude.NFData AwsEc2NetworkAclAssociation where
  rnf AwsEc2NetworkAclAssociation' {..} =
    Prelude.rnf networkAclAssociationId
      `Prelude.seq` Prelude.rnf networkAclId
      `Prelude.seq` Prelude.rnf subnetId

instance Data.ToJSON AwsEc2NetworkAclAssociation where
  toJSON AwsEc2NetworkAclAssociation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NetworkAclAssociationId" Data..=)
              Prelude.<$> networkAclAssociationId,
            ("NetworkAclId" Data..=) Prelude.<$> networkAclId,
            ("SubnetId" Data..=) Prelude.<$> subnetId
          ]
      )
