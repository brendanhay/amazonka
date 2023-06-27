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
-- Module      : Amazonka.VPCLattice.Types.ServiceSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VPCLattice.Types.ServiceSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.VPCLattice.Types.DnsEntry
import Amazonka.VPCLattice.Types.ServiceStatus

-- | Summary information about a service.
--
-- /See:/ 'newServiceSummary' smart constructor.
data ServiceSummary = ServiceSummary'
  { -- | The Amazon Resource Name (ARN) of the service.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the service was created, specified in ISO-8601
    -- format.
    createdAt :: Prelude.Maybe Data.ISO8601,
    -- | The custom domain name of the service.
    customDomainName :: Prelude.Maybe Prelude.Text,
    -- | DNS information about the service.
    dnsEntry :: Prelude.Maybe DnsEntry,
    -- | The ID of the service.
    id :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the service was last updated. The format is
    -- ISO-8601.
    lastUpdatedAt :: Prelude.Maybe Data.ISO8601,
    -- | The name of the service.
    name :: Prelude.Maybe Prelude.Text,
    -- | The status.
    status :: Prelude.Maybe ServiceStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'serviceSummary_arn' - The Amazon Resource Name (ARN) of the service.
--
-- 'createdAt', 'serviceSummary_createdAt' - The date and time that the service was created, specified in ISO-8601
-- format.
--
-- 'customDomainName', 'serviceSummary_customDomainName' - The custom domain name of the service.
--
-- 'dnsEntry', 'serviceSummary_dnsEntry' - DNS information about the service.
--
-- 'id', 'serviceSummary_id' - The ID of the service.
--
-- 'lastUpdatedAt', 'serviceSummary_lastUpdatedAt' - The date and time that the service was last updated. The format is
-- ISO-8601.
--
-- 'name', 'serviceSummary_name' - The name of the service.
--
-- 'status', 'serviceSummary_status' - The status.
newServiceSummary ::
  ServiceSummary
newServiceSummary =
  ServiceSummary'
    { arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      customDomainName = Prelude.Nothing,
      dnsEntry = Prelude.Nothing,
      id = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the service.
serviceSummary_arn :: Lens.Lens' ServiceSummary (Prelude.Maybe Prelude.Text)
serviceSummary_arn = Lens.lens (\ServiceSummary' {arn} -> arn) (\s@ServiceSummary' {} a -> s {arn = a} :: ServiceSummary)

-- | The date and time that the service was created, specified in ISO-8601
-- format.
serviceSummary_createdAt :: Lens.Lens' ServiceSummary (Prelude.Maybe Prelude.UTCTime)
serviceSummary_createdAt = Lens.lens (\ServiceSummary' {createdAt} -> createdAt) (\s@ServiceSummary' {} a -> s {createdAt = a} :: ServiceSummary) Prelude.. Lens.mapping Data._Time

-- | The custom domain name of the service.
serviceSummary_customDomainName :: Lens.Lens' ServiceSummary (Prelude.Maybe Prelude.Text)
serviceSummary_customDomainName = Lens.lens (\ServiceSummary' {customDomainName} -> customDomainName) (\s@ServiceSummary' {} a -> s {customDomainName = a} :: ServiceSummary)

-- | DNS information about the service.
serviceSummary_dnsEntry :: Lens.Lens' ServiceSummary (Prelude.Maybe DnsEntry)
serviceSummary_dnsEntry = Lens.lens (\ServiceSummary' {dnsEntry} -> dnsEntry) (\s@ServiceSummary' {} a -> s {dnsEntry = a} :: ServiceSummary)

-- | The ID of the service.
serviceSummary_id :: Lens.Lens' ServiceSummary (Prelude.Maybe Prelude.Text)
serviceSummary_id = Lens.lens (\ServiceSummary' {id} -> id) (\s@ServiceSummary' {} a -> s {id = a} :: ServiceSummary)

-- | The date and time that the service was last updated. The format is
-- ISO-8601.
serviceSummary_lastUpdatedAt :: Lens.Lens' ServiceSummary (Prelude.Maybe Prelude.UTCTime)
serviceSummary_lastUpdatedAt = Lens.lens (\ServiceSummary' {lastUpdatedAt} -> lastUpdatedAt) (\s@ServiceSummary' {} a -> s {lastUpdatedAt = a} :: ServiceSummary) Prelude.. Lens.mapping Data._Time

-- | The name of the service.
serviceSummary_name :: Lens.Lens' ServiceSummary (Prelude.Maybe Prelude.Text)
serviceSummary_name = Lens.lens (\ServiceSummary' {name} -> name) (\s@ServiceSummary' {} a -> s {name = a} :: ServiceSummary)

-- | The status.
serviceSummary_status :: Lens.Lens' ServiceSummary (Prelude.Maybe ServiceStatus)
serviceSummary_status = Lens.lens (\ServiceSummary' {status} -> status) (\s@ServiceSummary' {} a -> s {status = a} :: ServiceSummary)

instance Data.FromJSON ServiceSummary where
  parseJSON =
    Data.withObject
      "ServiceSummary"
      ( \x ->
          ServiceSummary'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "customDomainName")
            Prelude.<*> (x Data..:? "dnsEntry")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "lastUpdatedAt")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "status")
      )

instance Prelude.Hashable ServiceSummary where
  hashWithSalt _salt ServiceSummary' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` customDomainName
      `Prelude.hashWithSalt` dnsEntry
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` lastUpdatedAt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status

instance Prelude.NFData ServiceSummary where
  rnf ServiceSummary' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf customDomainName
      `Prelude.seq` Prelude.rnf dnsEntry
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
