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
-- Module      : Amazonka.VPCLattice.Types.ServiceNetworkServiceAssociationSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VPCLattice.Types.ServiceNetworkServiceAssociationSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.VPCLattice.Types.DnsEntry
import Amazonka.VPCLattice.Types.ServiceNetworkServiceAssociationStatus

-- | Summary information about the association between a service network and
-- a service.
--
-- /See:/ 'newServiceNetworkServiceAssociationSummary' smart constructor.
data ServiceNetworkServiceAssociationSummary = ServiceNetworkServiceAssociationSummary'
  { -- | The Amazon Resource Name (ARN) of the association.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the association was created, specified in
    -- ISO-8601 format.
    createdAt :: Prelude.Maybe Data.ISO8601,
    -- | The account that created the association.
    createdBy :: Prelude.Maybe Prelude.Text,
    -- | The custom domain name of the service.
    customDomainName :: Prelude.Maybe Prelude.Text,
    -- | DNS information about the service.
    dnsEntry :: Prelude.Maybe DnsEntry,
    -- | The ID of the association.
    id :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the service.
    serviceArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the service.
    serviceId :: Prelude.Maybe Prelude.Text,
    -- | The name of the service.
    serviceName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the service network.
    serviceNetworkArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the service network.
    serviceNetworkId :: Prelude.Maybe Prelude.Text,
    -- | The name of the service network.
    serviceNetworkName :: Prelude.Maybe Prelude.Text,
    -- | The status. If the deletion fails, try to delete again.
    status :: Prelude.Maybe ServiceNetworkServiceAssociationStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceNetworkServiceAssociationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'serviceNetworkServiceAssociationSummary_arn' - The Amazon Resource Name (ARN) of the association.
--
-- 'createdAt', 'serviceNetworkServiceAssociationSummary_createdAt' - The date and time that the association was created, specified in
-- ISO-8601 format.
--
-- 'createdBy', 'serviceNetworkServiceAssociationSummary_createdBy' - The account that created the association.
--
-- 'customDomainName', 'serviceNetworkServiceAssociationSummary_customDomainName' - The custom domain name of the service.
--
-- 'dnsEntry', 'serviceNetworkServiceAssociationSummary_dnsEntry' - DNS information about the service.
--
-- 'id', 'serviceNetworkServiceAssociationSummary_id' - The ID of the association.
--
-- 'serviceArn', 'serviceNetworkServiceAssociationSummary_serviceArn' - The Amazon Resource Name (ARN) of the service.
--
-- 'serviceId', 'serviceNetworkServiceAssociationSummary_serviceId' - The ID of the service.
--
-- 'serviceName', 'serviceNetworkServiceAssociationSummary_serviceName' - The name of the service.
--
-- 'serviceNetworkArn', 'serviceNetworkServiceAssociationSummary_serviceNetworkArn' - The Amazon Resource Name (ARN) of the service network.
--
-- 'serviceNetworkId', 'serviceNetworkServiceAssociationSummary_serviceNetworkId' - The ID of the service network.
--
-- 'serviceNetworkName', 'serviceNetworkServiceAssociationSummary_serviceNetworkName' - The name of the service network.
--
-- 'status', 'serviceNetworkServiceAssociationSummary_status' - The status. If the deletion fails, try to delete again.
newServiceNetworkServiceAssociationSummary ::
  ServiceNetworkServiceAssociationSummary
newServiceNetworkServiceAssociationSummary =
  ServiceNetworkServiceAssociationSummary'
    { arn =
        Prelude.Nothing,
      createdAt = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      customDomainName = Prelude.Nothing,
      dnsEntry = Prelude.Nothing,
      id = Prelude.Nothing,
      serviceArn = Prelude.Nothing,
      serviceId = Prelude.Nothing,
      serviceName = Prelude.Nothing,
      serviceNetworkArn =
        Prelude.Nothing,
      serviceNetworkId = Prelude.Nothing,
      serviceNetworkName =
        Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the association.
serviceNetworkServiceAssociationSummary_arn :: Lens.Lens' ServiceNetworkServiceAssociationSummary (Prelude.Maybe Prelude.Text)
serviceNetworkServiceAssociationSummary_arn = Lens.lens (\ServiceNetworkServiceAssociationSummary' {arn} -> arn) (\s@ServiceNetworkServiceAssociationSummary' {} a -> s {arn = a} :: ServiceNetworkServiceAssociationSummary)

-- | The date and time that the association was created, specified in
-- ISO-8601 format.
serviceNetworkServiceAssociationSummary_createdAt :: Lens.Lens' ServiceNetworkServiceAssociationSummary (Prelude.Maybe Prelude.UTCTime)
serviceNetworkServiceAssociationSummary_createdAt = Lens.lens (\ServiceNetworkServiceAssociationSummary' {createdAt} -> createdAt) (\s@ServiceNetworkServiceAssociationSummary' {} a -> s {createdAt = a} :: ServiceNetworkServiceAssociationSummary) Prelude.. Lens.mapping Data._Time

-- | The account that created the association.
serviceNetworkServiceAssociationSummary_createdBy :: Lens.Lens' ServiceNetworkServiceAssociationSummary (Prelude.Maybe Prelude.Text)
serviceNetworkServiceAssociationSummary_createdBy = Lens.lens (\ServiceNetworkServiceAssociationSummary' {createdBy} -> createdBy) (\s@ServiceNetworkServiceAssociationSummary' {} a -> s {createdBy = a} :: ServiceNetworkServiceAssociationSummary)

-- | The custom domain name of the service.
serviceNetworkServiceAssociationSummary_customDomainName :: Lens.Lens' ServiceNetworkServiceAssociationSummary (Prelude.Maybe Prelude.Text)
serviceNetworkServiceAssociationSummary_customDomainName = Lens.lens (\ServiceNetworkServiceAssociationSummary' {customDomainName} -> customDomainName) (\s@ServiceNetworkServiceAssociationSummary' {} a -> s {customDomainName = a} :: ServiceNetworkServiceAssociationSummary)

-- | DNS information about the service.
serviceNetworkServiceAssociationSummary_dnsEntry :: Lens.Lens' ServiceNetworkServiceAssociationSummary (Prelude.Maybe DnsEntry)
serviceNetworkServiceAssociationSummary_dnsEntry = Lens.lens (\ServiceNetworkServiceAssociationSummary' {dnsEntry} -> dnsEntry) (\s@ServiceNetworkServiceAssociationSummary' {} a -> s {dnsEntry = a} :: ServiceNetworkServiceAssociationSummary)

-- | The ID of the association.
serviceNetworkServiceAssociationSummary_id :: Lens.Lens' ServiceNetworkServiceAssociationSummary (Prelude.Maybe Prelude.Text)
serviceNetworkServiceAssociationSummary_id = Lens.lens (\ServiceNetworkServiceAssociationSummary' {id} -> id) (\s@ServiceNetworkServiceAssociationSummary' {} a -> s {id = a} :: ServiceNetworkServiceAssociationSummary)

-- | The Amazon Resource Name (ARN) of the service.
serviceNetworkServiceAssociationSummary_serviceArn :: Lens.Lens' ServiceNetworkServiceAssociationSummary (Prelude.Maybe Prelude.Text)
serviceNetworkServiceAssociationSummary_serviceArn = Lens.lens (\ServiceNetworkServiceAssociationSummary' {serviceArn} -> serviceArn) (\s@ServiceNetworkServiceAssociationSummary' {} a -> s {serviceArn = a} :: ServiceNetworkServiceAssociationSummary)

-- | The ID of the service.
serviceNetworkServiceAssociationSummary_serviceId :: Lens.Lens' ServiceNetworkServiceAssociationSummary (Prelude.Maybe Prelude.Text)
serviceNetworkServiceAssociationSummary_serviceId = Lens.lens (\ServiceNetworkServiceAssociationSummary' {serviceId} -> serviceId) (\s@ServiceNetworkServiceAssociationSummary' {} a -> s {serviceId = a} :: ServiceNetworkServiceAssociationSummary)

-- | The name of the service.
serviceNetworkServiceAssociationSummary_serviceName :: Lens.Lens' ServiceNetworkServiceAssociationSummary (Prelude.Maybe Prelude.Text)
serviceNetworkServiceAssociationSummary_serviceName = Lens.lens (\ServiceNetworkServiceAssociationSummary' {serviceName} -> serviceName) (\s@ServiceNetworkServiceAssociationSummary' {} a -> s {serviceName = a} :: ServiceNetworkServiceAssociationSummary)

-- | The Amazon Resource Name (ARN) of the service network.
serviceNetworkServiceAssociationSummary_serviceNetworkArn :: Lens.Lens' ServiceNetworkServiceAssociationSummary (Prelude.Maybe Prelude.Text)
serviceNetworkServiceAssociationSummary_serviceNetworkArn = Lens.lens (\ServiceNetworkServiceAssociationSummary' {serviceNetworkArn} -> serviceNetworkArn) (\s@ServiceNetworkServiceAssociationSummary' {} a -> s {serviceNetworkArn = a} :: ServiceNetworkServiceAssociationSummary)

-- | The ID of the service network.
serviceNetworkServiceAssociationSummary_serviceNetworkId :: Lens.Lens' ServiceNetworkServiceAssociationSummary (Prelude.Maybe Prelude.Text)
serviceNetworkServiceAssociationSummary_serviceNetworkId = Lens.lens (\ServiceNetworkServiceAssociationSummary' {serviceNetworkId} -> serviceNetworkId) (\s@ServiceNetworkServiceAssociationSummary' {} a -> s {serviceNetworkId = a} :: ServiceNetworkServiceAssociationSummary)

-- | The name of the service network.
serviceNetworkServiceAssociationSummary_serviceNetworkName :: Lens.Lens' ServiceNetworkServiceAssociationSummary (Prelude.Maybe Prelude.Text)
serviceNetworkServiceAssociationSummary_serviceNetworkName = Lens.lens (\ServiceNetworkServiceAssociationSummary' {serviceNetworkName} -> serviceNetworkName) (\s@ServiceNetworkServiceAssociationSummary' {} a -> s {serviceNetworkName = a} :: ServiceNetworkServiceAssociationSummary)

-- | The status. If the deletion fails, try to delete again.
serviceNetworkServiceAssociationSummary_status :: Lens.Lens' ServiceNetworkServiceAssociationSummary (Prelude.Maybe ServiceNetworkServiceAssociationStatus)
serviceNetworkServiceAssociationSummary_status = Lens.lens (\ServiceNetworkServiceAssociationSummary' {status} -> status) (\s@ServiceNetworkServiceAssociationSummary' {} a -> s {status = a} :: ServiceNetworkServiceAssociationSummary)

instance
  Data.FromJSON
    ServiceNetworkServiceAssociationSummary
  where
  parseJSON =
    Data.withObject
      "ServiceNetworkServiceAssociationSummary"
      ( \x ->
          ServiceNetworkServiceAssociationSummary'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "createdBy")
            Prelude.<*> (x Data..:? "customDomainName")
            Prelude.<*> (x Data..:? "dnsEntry")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "serviceArn")
            Prelude.<*> (x Data..:? "serviceId")
            Prelude.<*> (x Data..:? "serviceName")
            Prelude.<*> (x Data..:? "serviceNetworkArn")
            Prelude.<*> (x Data..:? "serviceNetworkId")
            Prelude.<*> (x Data..:? "serviceNetworkName")
            Prelude.<*> (x Data..:? "status")
      )

instance
  Prelude.Hashable
    ServiceNetworkServiceAssociationSummary
  where
  hashWithSalt
    _salt
    ServiceNetworkServiceAssociationSummary' {..} =
      _salt
        `Prelude.hashWithSalt` arn
        `Prelude.hashWithSalt` createdAt
        `Prelude.hashWithSalt` createdBy
        `Prelude.hashWithSalt` customDomainName
        `Prelude.hashWithSalt` dnsEntry
        `Prelude.hashWithSalt` id
        `Prelude.hashWithSalt` serviceArn
        `Prelude.hashWithSalt` serviceId
        `Prelude.hashWithSalt` serviceName
        `Prelude.hashWithSalt` serviceNetworkArn
        `Prelude.hashWithSalt` serviceNetworkId
        `Prelude.hashWithSalt` serviceNetworkName
        `Prelude.hashWithSalt` status

instance
  Prelude.NFData
    ServiceNetworkServiceAssociationSummary
  where
  rnf ServiceNetworkServiceAssociationSummary' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf customDomainName
      `Prelude.seq` Prelude.rnf dnsEntry
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf serviceArn
      `Prelude.seq` Prelude.rnf serviceId
      `Prelude.seq` Prelude.rnf serviceName
      `Prelude.seq` Prelude.rnf serviceNetworkArn
      `Prelude.seq` Prelude.rnf serviceNetworkId
      `Prelude.seq` Prelude.rnf serviceNetworkName
      `Prelude.seq` Prelude.rnf status
