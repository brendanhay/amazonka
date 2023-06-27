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
-- Module      : Amazonka.VPCLattice.Types.ServiceNetworkVpcAssociationSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VPCLattice.Types.ServiceNetworkVpcAssociationSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.VPCLattice.Types.ServiceNetworkVpcAssociationStatus

-- | Summary information about an association between a service network and a
-- VPC.
--
-- /See:/ 'newServiceNetworkVpcAssociationSummary' smart constructor.
data ServiceNetworkVpcAssociationSummary = ServiceNetworkVpcAssociationSummary'
  { -- | The Amazon Resource Name (ARN) of the association.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the association was created, specified in
    -- ISO-8601 format.
    createdAt :: Prelude.Maybe Data.ISO8601,
    -- | The account that created the association.
    createdBy :: Prelude.Maybe Prelude.Text,
    -- | The ID of the association.
    id :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the association was last updated, specified in
    -- ISO-8601 format.
    lastUpdatedAt :: Prelude.Maybe Data.ISO8601,
    -- | The Amazon Resource Name (ARN) of the service network.
    serviceNetworkArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the service network.
    serviceNetworkId :: Prelude.Maybe Prelude.Text,
    -- | The name of the service network.
    serviceNetworkName :: Prelude.Maybe Prelude.Text,
    -- | The status.
    status :: Prelude.Maybe ServiceNetworkVpcAssociationStatus,
    -- | The ID of the VPC.
    vpcId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceNetworkVpcAssociationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'serviceNetworkVpcAssociationSummary_arn' - The Amazon Resource Name (ARN) of the association.
--
-- 'createdAt', 'serviceNetworkVpcAssociationSummary_createdAt' - The date and time that the association was created, specified in
-- ISO-8601 format.
--
-- 'createdBy', 'serviceNetworkVpcAssociationSummary_createdBy' - The account that created the association.
--
-- 'id', 'serviceNetworkVpcAssociationSummary_id' - The ID of the association.
--
-- 'lastUpdatedAt', 'serviceNetworkVpcAssociationSummary_lastUpdatedAt' - The date and time that the association was last updated, specified in
-- ISO-8601 format.
--
-- 'serviceNetworkArn', 'serviceNetworkVpcAssociationSummary_serviceNetworkArn' - The Amazon Resource Name (ARN) of the service network.
--
-- 'serviceNetworkId', 'serviceNetworkVpcAssociationSummary_serviceNetworkId' - The ID of the service network.
--
-- 'serviceNetworkName', 'serviceNetworkVpcAssociationSummary_serviceNetworkName' - The name of the service network.
--
-- 'status', 'serviceNetworkVpcAssociationSummary_status' - The status.
--
-- 'vpcId', 'serviceNetworkVpcAssociationSummary_vpcId' - The ID of the VPC.
newServiceNetworkVpcAssociationSummary ::
  ServiceNetworkVpcAssociationSummary
newServiceNetworkVpcAssociationSummary =
  ServiceNetworkVpcAssociationSummary'
    { arn =
        Prelude.Nothing,
      createdAt = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      id = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      serviceNetworkArn = Prelude.Nothing,
      serviceNetworkId = Prelude.Nothing,
      serviceNetworkName = Prelude.Nothing,
      status = Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the association.
serviceNetworkVpcAssociationSummary_arn :: Lens.Lens' ServiceNetworkVpcAssociationSummary (Prelude.Maybe Prelude.Text)
serviceNetworkVpcAssociationSummary_arn = Lens.lens (\ServiceNetworkVpcAssociationSummary' {arn} -> arn) (\s@ServiceNetworkVpcAssociationSummary' {} a -> s {arn = a} :: ServiceNetworkVpcAssociationSummary)

-- | The date and time that the association was created, specified in
-- ISO-8601 format.
serviceNetworkVpcAssociationSummary_createdAt :: Lens.Lens' ServiceNetworkVpcAssociationSummary (Prelude.Maybe Prelude.UTCTime)
serviceNetworkVpcAssociationSummary_createdAt = Lens.lens (\ServiceNetworkVpcAssociationSummary' {createdAt} -> createdAt) (\s@ServiceNetworkVpcAssociationSummary' {} a -> s {createdAt = a} :: ServiceNetworkVpcAssociationSummary) Prelude.. Lens.mapping Data._Time

-- | The account that created the association.
serviceNetworkVpcAssociationSummary_createdBy :: Lens.Lens' ServiceNetworkVpcAssociationSummary (Prelude.Maybe Prelude.Text)
serviceNetworkVpcAssociationSummary_createdBy = Lens.lens (\ServiceNetworkVpcAssociationSummary' {createdBy} -> createdBy) (\s@ServiceNetworkVpcAssociationSummary' {} a -> s {createdBy = a} :: ServiceNetworkVpcAssociationSummary)

-- | The ID of the association.
serviceNetworkVpcAssociationSummary_id :: Lens.Lens' ServiceNetworkVpcAssociationSummary (Prelude.Maybe Prelude.Text)
serviceNetworkVpcAssociationSummary_id = Lens.lens (\ServiceNetworkVpcAssociationSummary' {id} -> id) (\s@ServiceNetworkVpcAssociationSummary' {} a -> s {id = a} :: ServiceNetworkVpcAssociationSummary)

-- | The date and time that the association was last updated, specified in
-- ISO-8601 format.
serviceNetworkVpcAssociationSummary_lastUpdatedAt :: Lens.Lens' ServiceNetworkVpcAssociationSummary (Prelude.Maybe Prelude.UTCTime)
serviceNetworkVpcAssociationSummary_lastUpdatedAt = Lens.lens (\ServiceNetworkVpcAssociationSummary' {lastUpdatedAt} -> lastUpdatedAt) (\s@ServiceNetworkVpcAssociationSummary' {} a -> s {lastUpdatedAt = a} :: ServiceNetworkVpcAssociationSummary) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the service network.
serviceNetworkVpcAssociationSummary_serviceNetworkArn :: Lens.Lens' ServiceNetworkVpcAssociationSummary (Prelude.Maybe Prelude.Text)
serviceNetworkVpcAssociationSummary_serviceNetworkArn = Lens.lens (\ServiceNetworkVpcAssociationSummary' {serviceNetworkArn} -> serviceNetworkArn) (\s@ServiceNetworkVpcAssociationSummary' {} a -> s {serviceNetworkArn = a} :: ServiceNetworkVpcAssociationSummary)

-- | The ID of the service network.
serviceNetworkVpcAssociationSummary_serviceNetworkId :: Lens.Lens' ServiceNetworkVpcAssociationSummary (Prelude.Maybe Prelude.Text)
serviceNetworkVpcAssociationSummary_serviceNetworkId = Lens.lens (\ServiceNetworkVpcAssociationSummary' {serviceNetworkId} -> serviceNetworkId) (\s@ServiceNetworkVpcAssociationSummary' {} a -> s {serviceNetworkId = a} :: ServiceNetworkVpcAssociationSummary)

-- | The name of the service network.
serviceNetworkVpcAssociationSummary_serviceNetworkName :: Lens.Lens' ServiceNetworkVpcAssociationSummary (Prelude.Maybe Prelude.Text)
serviceNetworkVpcAssociationSummary_serviceNetworkName = Lens.lens (\ServiceNetworkVpcAssociationSummary' {serviceNetworkName} -> serviceNetworkName) (\s@ServiceNetworkVpcAssociationSummary' {} a -> s {serviceNetworkName = a} :: ServiceNetworkVpcAssociationSummary)

-- | The status.
serviceNetworkVpcAssociationSummary_status :: Lens.Lens' ServiceNetworkVpcAssociationSummary (Prelude.Maybe ServiceNetworkVpcAssociationStatus)
serviceNetworkVpcAssociationSummary_status = Lens.lens (\ServiceNetworkVpcAssociationSummary' {status} -> status) (\s@ServiceNetworkVpcAssociationSummary' {} a -> s {status = a} :: ServiceNetworkVpcAssociationSummary)

-- | The ID of the VPC.
serviceNetworkVpcAssociationSummary_vpcId :: Lens.Lens' ServiceNetworkVpcAssociationSummary (Prelude.Maybe Prelude.Text)
serviceNetworkVpcAssociationSummary_vpcId = Lens.lens (\ServiceNetworkVpcAssociationSummary' {vpcId} -> vpcId) (\s@ServiceNetworkVpcAssociationSummary' {} a -> s {vpcId = a} :: ServiceNetworkVpcAssociationSummary)

instance
  Data.FromJSON
    ServiceNetworkVpcAssociationSummary
  where
  parseJSON =
    Data.withObject
      "ServiceNetworkVpcAssociationSummary"
      ( \x ->
          ServiceNetworkVpcAssociationSummary'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "createdBy")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "lastUpdatedAt")
            Prelude.<*> (x Data..:? "serviceNetworkArn")
            Prelude.<*> (x Data..:? "serviceNetworkId")
            Prelude.<*> (x Data..:? "serviceNetworkName")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "vpcId")
      )

instance
  Prelude.Hashable
    ServiceNetworkVpcAssociationSummary
  where
  hashWithSalt
    _salt
    ServiceNetworkVpcAssociationSummary' {..} =
      _salt
        `Prelude.hashWithSalt` arn
        `Prelude.hashWithSalt` createdAt
        `Prelude.hashWithSalt` createdBy
        `Prelude.hashWithSalt` id
        `Prelude.hashWithSalt` lastUpdatedAt
        `Prelude.hashWithSalt` serviceNetworkArn
        `Prelude.hashWithSalt` serviceNetworkId
        `Prelude.hashWithSalt` serviceNetworkName
        `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` vpcId

instance
  Prelude.NFData
    ServiceNetworkVpcAssociationSummary
  where
  rnf ServiceNetworkVpcAssociationSummary' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf serviceNetworkArn
      `Prelude.seq` Prelude.rnf serviceNetworkId
      `Prelude.seq` Prelude.rnf serviceNetworkName
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf vpcId
