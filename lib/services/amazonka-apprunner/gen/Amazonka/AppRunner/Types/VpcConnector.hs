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
-- Module      : Amazonka.AppRunner.Types.VpcConnector
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppRunner.Types.VpcConnector where

import Amazonka.AppRunner.Types.VpcConnectorStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an App Runner VPC connector resource. A VPC connector
-- describes the Amazon Virtual Private Cloud (Amazon VPC) that an App
-- Runner service is associated with, and the subnets and security group
-- that are used.
--
-- Multiple revisions of a connector might have the same @Name@ and
-- different @Revision@ values.
--
-- At this time, App Runner supports only one revision per name.
--
-- /See:/ 'newVpcConnector' smart constructor.
data VpcConnector = VpcConnector'
  { -- | The time when the VPC connector was created. It\'s in Unix time stamp
    -- format.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The time when the VPC connector was deleted. It\'s in Unix time stamp
    -- format.
    deletedAt :: Prelude.Maybe Data.POSIX,
    -- | A list of IDs of security groups that App Runner uses for access to
    -- Amazon Web Services resources under the specified subnets. If not
    -- specified, App Runner uses the default security group of the Amazon VPC.
    -- The default security group allows all outbound traffic.
    securityGroups :: Prelude.Maybe [Prelude.Text],
    -- | The current state of the VPC connector. If the status of a connector
    -- revision is @INACTIVE@, it was deleted and can\'t be used. Inactive
    -- connector revisions are permanently removed some time after they are
    -- deleted.
    status :: Prelude.Maybe VpcConnectorStatus,
    -- | A list of IDs of subnets that App Runner uses for your service. All IDs
    -- are of subnets of a single Amazon VPC.
    subnets :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Resource Name (ARN) of this VPC connector.
    vpcConnectorArn :: Prelude.Maybe Prelude.Text,
    -- | The customer-provided VPC connector name.
    vpcConnectorName :: Prelude.Maybe Prelude.Text,
    -- | The revision of this VPC connector. It\'s unique among all the active
    -- connectors (@\"Status\": \"ACTIVE\"@) that share the same @Name@.
    --
    -- At this time, App Runner supports only one revision per name.
    vpcConnectorRevision :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpcConnector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'vpcConnector_createdAt' - The time when the VPC connector was created. It\'s in Unix time stamp
-- format.
--
-- 'deletedAt', 'vpcConnector_deletedAt' - The time when the VPC connector was deleted. It\'s in Unix time stamp
-- format.
--
-- 'securityGroups', 'vpcConnector_securityGroups' - A list of IDs of security groups that App Runner uses for access to
-- Amazon Web Services resources under the specified subnets. If not
-- specified, App Runner uses the default security group of the Amazon VPC.
-- The default security group allows all outbound traffic.
--
-- 'status', 'vpcConnector_status' - The current state of the VPC connector. If the status of a connector
-- revision is @INACTIVE@, it was deleted and can\'t be used. Inactive
-- connector revisions are permanently removed some time after they are
-- deleted.
--
-- 'subnets', 'vpcConnector_subnets' - A list of IDs of subnets that App Runner uses for your service. All IDs
-- are of subnets of a single Amazon VPC.
--
-- 'vpcConnectorArn', 'vpcConnector_vpcConnectorArn' - The Amazon Resource Name (ARN) of this VPC connector.
--
-- 'vpcConnectorName', 'vpcConnector_vpcConnectorName' - The customer-provided VPC connector name.
--
-- 'vpcConnectorRevision', 'vpcConnector_vpcConnectorRevision' - The revision of this VPC connector. It\'s unique among all the active
-- connectors (@\"Status\": \"ACTIVE\"@) that share the same @Name@.
--
-- At this time, App Runner supports only one revision per name.
newVpcConnector ::
  VpcConnector
newVpcConnector =
  VpcConnector'
    { createdAt = Prelude.Nothing,
      deletedAt = Prelude.Nothing,
      securityGroups = Prelude.Nothing,
      status = Prelude.Nothing,
      subnets = Prelude.Nothing,
      vpcConnectorArn = Prelude.Nothing,
      vpcConnectorName = Prelude.Nothing,
      vpcConnectorRevision = Prelude.Nothing
    }

-- | The time when the VPC connector was created. It\'s in Unix time stamp
-- format.
vpcConnector_createdAt :: Lens.Lens' VpcConnector (Prelude.Maybe Prelude.UTCTime)
vpcConnector_createdAt = Lens.lens (\VpcConnector' {createdAt} -> createdAt) (\s@VpcConnector' {} a -> s {createdAt = a} :: VpcConnector) Prelude.. Lens.mapping Data._Time

-- | The time when the VPC connector was deleted. It\'s in Unix time stamp
-- format.
vpcConnector_deletedAt :: Lens.Lens' VpcConnector (Prelude.Maybe Prelude.UTCTime)
vpcConnector_deletedAt = Lens.lens (\VpcConnector' {deletedAt} -> deletedAt) (\s@VpcConnector' {} a -> s {deletedAt = a} :: VpcConnector) Prelude.. Lens.mapping Data._Time

-- | A list of IDs of security groups that App Runner uses for access to
-- Amazon Web Services resources under the specified subnets. If not
-- specified, App Runner uses the default security group of the Amazon VPC.
-- The default security group allows all outbound traffic.
vpcConnector_securityGroups :: Lens.Lens' VpcConnector (Prelude.Maybe [Prelude.Text])
vpcConnector_securityGroups = Lens.lens (\VpcConnector' {securityGroups} -> securityGroups) (\s@VpcConnector' {} a -> s {securityGroups = a} :: VpcConnector) Prelude.. Lens.mapping Lens.coerced

-- | The current state of the VPC connector. If the status of a connector
-- revision is @INACTIVE@, it was deleted and can\'t be used. Inactive
-- connector revisions are permanently removed some time after they are
-- deleted.
vpcConnector_status :: Lens.Lens' VpcConnector (Prelude.Maybe VpcConnectorStatus)
vpcConnector_status = Lens.lens (\VpcConnector' {status} -> status) (\s@VpcConnector' {} a -> s {status = a} :: VpcConnector)

-- | A list of IDs of subnets that App Runner uses for your service. All IDs
-- are of subnets of a single Amazon VPC.
vpcConnector_subnets :: Lens.Lens' VpcConnector (Prelude.Maybe [Prelude.Text])
vpcConnector_subnets = Lens.lens (\VpcConnector' {subnets} -> subnets) (\s@VpcConnector' {} a -> s {subnets = a} :: VpcConnector) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of this VPC connector.
vpcConnector_vpcConnectorArn :: Lens.Lens' VpcConnector (Prelude.Maybe Prelude.Text)
vpcConnector_vpcConnectorArn = Lens.lens (\VpcConnector' {vpcConnectorArn} -> vpcConnectorArn) (\s@VpcConnector' {} a -> s {vpcConnectorArn = a} :: VpcConnector)

-- | The customer-provided VPC connector name.
vpcConnector_vpcConnectorName :: Lens.Lens' VpcConnector (Prelude.Maybe Prelude.Text)
vpcConnector_vpcConnectorName = Lens.lens (\VpcConnector' {vpcConnectorName} -> vpcConnectorName) (\s@VpcConnector' {} a -> s {vpcConnectorName = a} :: VpcConnector)

-- | The revision of this VPC connector. It\'s unique among all the active
-- connectors (@\"Status\": \"ACTIVE\"@) that share the same @Name@.
--
-- At this time, App Runner supports only one revision per name.
vpcConnector_vpcConnectorRevision :: Lens.Lens' VpcConnector (Prelude.Maybe Prelude.Int)
vpcConnector_vpcConnectorRevision = Lens.lens (\VpcConnector' {vpcConnectorRevision} -> vpcConnectorRevision) (\s@VpcConnector' {} a -> s {vpcConnectorRevision = a} :: VpcConnector)

instance Data.FromJSON VpcConnector where
  parseJSON =
    Data.withObject
      "VpcConnector"
      ( \x ->
          VpcConnector'
            Prelude.<$> (x Data..:? "CreatedAt")
            Prelude.<*> (x Data..:? "DeletedAt")
            Prelude.<*> (x Data..:? "SecurityGroups" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "Subnets" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "VpcConnectorArn")
            Prelude.<*> (x Data..:? "VpcConnectorName")
            Prelude.<*> (x Data..:? "VpcConnectorRevision")
      )

instance Prelude.Hashable VpcConnector where
  hashWithSalt _salt VpcConnector' {..} =
    _salt
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` deletedAt
      `Prelude.hashWithSalt` securityGroups
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` subnets
      `Prelude.hashWithSalt` vpcConnectorArn
      `Prelude.hashWithSalt` vpcConnectorName
      `Prelude.hashWithSalt` vpcConnectorRevision

instance Prelude.NFData VpcConnector where
  rnf VpcConnector' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf deletedAt
      `Prelude.seq` Prelude.rnf securityGroups
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf subnets
      `Prelude.seq` Prelude.rnf vpcConnectorArn
      `Prelude.seq` Prelude.rnf vpcConnectorName
      `Prelude.seq` Prelude.rnf vpcConnectorRevision
