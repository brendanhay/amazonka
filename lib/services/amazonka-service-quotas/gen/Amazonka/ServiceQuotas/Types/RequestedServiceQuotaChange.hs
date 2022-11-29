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
-- Module      : Amazonka.ServiceQuotas.Types.RequestedServiceQuotaChange
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceQuotas.Types.RequestedServiceQuotaChange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.ServiceQuotas.Types.RequestStatus

-- | Information about a quota increase request.
--
-- /See:/ 'newRequestedServiceQuotaChange' smart constructor.
data RequestedServiceQuotaChange = RequestedServiceQuotaChange'
  { -- | The Amazon Resource Name (ARN) of the quota.
    quotaArn :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the quota is global.
    globalQuota :: Prelude.Maybe Prelude.Bool,
    -- | The quota identifier.
    quotaCode :: Prelude.Maybe Prelude.Text,
    -- | The case ID.
    caseId :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the quota increase request was received and the
    -- case ID was created.
    created :: Prelude.Maybe Core.POSIX,
    -- | The service identifier.
    serviceCode :: Prelude.Maybe Prelude.Text,
    -- | The state of the quota increase request.
    status :: Prelude.Maybe RequestStatus,
    -- | The unique identifier.
    id :: Prelude.Maybe Prelude.Text,
    -- | The date and time of the most recent change.
    lastUpdated :: Prelude.Maybe Core.POSIX,
    -- | The quota name.
    quotaName :: Prelude.Maybe Prelude.Text,
    -- | The service name.
    serviceName :: Prelude.Maybe Prelude.Text,
    -- | The IAM identity of the requester.
    requester :: Prelude.Maybe Prelude.Text,
    -- | The unit of measurement.
    unit :: Prelude.Maybe Prelude.Text,
    -- | The new, increased value for the quota.
    desiredValue :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RequestedServiceQuotaChange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'quotaArn', 'requestedServiceQuotaChange_quotaArn' - The Amazon Resource Name (ARN) of the quota.
--
-- 'globalQuota', 'requestedServiceQuotaChange_globalQuota' - Indicates whether the quota is global.
--
-- 'quotaCode', 'requestedServiceQuotaChange_quotaCode' - The quota identifier.
--
-- 'caseId', 'requestedServiceQuotaChange_caseId' - The case ID.
--
-- 'created', 'requestedServiceQuotaChange_created' - The date and time when the quota increase request was received and the
-- case ID was created.
--
-- 'serviceCode', 'requestedServiceQuotaChange_serviceCode' - The service identifier.
--
-- 'status', 'requestedServiceQuotaChange_status' - The state of the quota increase request.
--
-- 'id', 'requestedServiceQuotaChange_id' - The unique identifier.
--
-- 'lastUpdated', 'requestedServiceQuotaChange_lastUpdated' - The date and time of the most recent change.
--
-- 'quotaName', 'requestedServiceQuotaChange_quotaName' - The quota name.
--
-- 'serviceName', 'requestedServiceQuotaChange_serviceName' - The service name.
--
-- 'requester', 'requestedServiceQuotaChange_requester' - The IAM identity of the requester.
--
-- 'unit', 'requestedServiceQuotaChange_unit' - The unit of measurement.
--
-- 'desiredValue', 'requestedServiceQuotaChange_desiredValue' - The new, increased value for the quota.
newRequestedServiceQuotaChange ::
  RequestedServiceQuotaChange
newRequestedServiceQuotaChange =
  RequestedServiceQuotaChange'
    { quotaArn =
        Prelude.Nothing,
      globalQuota = Prelude.Nothing,
      quotaCode = Prelude.Nothing,
      caseId = Prelude.Nothing,
      created = Prelude.Nothing,
      serviceCode = Prelude.Nothing,
      status = Prelude.Nothing,
      id = Prelude.Nothing,
      lastUpdated = Prelude.Nothing,
      quotaName = Prelude.Nothing,
      serviceName = Prelude.Nothing,
      requester = Prelude.Nothing,
      unit = Prelude.Nothing,
      desiredValue = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the quota.
requestedServiceQuotaChange_quotaArn :: Lens.Lens' RequestedServiceQuotaChange (Prelude.Maybe Prelude.Text)
requestedServiceQuotaChange_quotaArn = Lens.lens (\RequestedServiceQuotaChange' {quotaArn} -> quotaArn) (\s@RequestedServiceQuotaChange' {} a -> s {quotaArn = a} :: RequestedServiceQuotaChange)

-- | Indicates whether the quota is global.
requestedServiceQuotaChange_globalQuota :: Lens.Lens' RequestedServiceQuotaChange (Prelude.Maybe Prelude.Bool)
requestedServiceQuotaChange_globalQuota = Lens.lens (\RequestedServiceQuotaChange' {globalQuota} -> globalQuota) (\s@RequestedServiceQuotaChange' {} a -> s {globalQuota = a} :: RequestedServiceQuotaChange)

-- | The quota identifier.
requestedServiceQuotaChange_quotaCode :: Lens.Lens' RequestedServiceQuotaChange (Prelude.Maybe Prelude.Text)
requestedServiceQuotaChange_quotaCode = Lens.lens (\RequestedServiceQuotaChange' {quotaCode} -> quotaCode) (\s@RequestedServiceQuotaChange' {} a -> s {quotaCode = a} :: RequestedServiceQuotaChange)

-- | The case ID.
requestedServiceQuotaChange_caseId :: Lens.Lens' RequestedServiceQuotaChange (Prelude.Maybe Prelude.Text)
requestedServiceQuotaChange_caseId = Lens.lens (\RequestedServiceQuotaChange' {caseId} -> caseId) (\s@RequestedServiceQuotaChange' {} a -> s {caseId = a} :: RequestedServiceQuotaChange)

-- | The date and time when the quota increase request was received and the
-- case ID was created.
requestedServiceQuotaChange_created :: Lens.Lens' RequestedServiceQuotaChange (Prelude.Maybe Prelude.UTCTime)
requestedServiceQuotaChange_created = Lens.lens (\RequestedServiceQuotaChange' {created} -> created) (\s@RequestedServiceQuotaChange' {} a -> s {created = a} :: RequestedServiceQuotaChange) Prelude.. Lens.mapping Core._Time

-- | The service identifier.
requestedServiceQuotaChange_serviceCode :: Lens.Lens' RequestedServiceQuotaChange (Prelude.Maybe Prelude.Text)
requestedServiceQuotaChange_serviceCode = Lens.lens (\RequestedServiceQuotaChange' {serviceCode} -> serviceCode) (\s@RequestedServiceQuotaChange' {} a -> s {serviceCode = a} :: RequestedServiceQuotaChange)

-- | The state of the quota increase request.
requestedServiceQuotaChange_status :: Lens.Lens' RequestedServiceQuotaChange (Prelude.Maybe RequestStatus)
requestedServiceQuotaChange_status = Lens.lens (\RequestedServiceQuotaChange' {status} -> status) (\s@RequestedServiceQuotaChange' {} a -> s {status = a} :: RequestedServiceQuotaChange)

-- | The unique identifier.
requestedServiceQuotaChange_id :: Lens.Lens' RequestedServiceQuotaChange (Prelude.Maybe Prelude.Text)
requestedServiceQuotaChange_id = Lens.lens (\RequestedServiceQuotaChange' {id} -> id) (\s@RequestedServiceQuotaChange' {} a -> s {id = a} :: RequestedServiceQuotaChange)

-- | The date and time of the most recent change.
requestedServiceQuotaChange_lastUpdated :: Lens.Lens' RequestedServiceQuotaChange (Prelude.Maybe Prelude.UTCTime)
requestedServiceQuotaChange_lastUpdated = Lens.lens (\RequestedServiceQuotaChange' {lastUpdated} -> lastUpdated) (\s@RequestedServiceQuotaChange' {} a -> s {lastUpdated = a} :: RequestedServiceQuotaChange) Prelude.. Lens.mapping Core._Time

-- | The quota name.
requestedServiceQuotaChange_quotaName :: Lens.Lens' RequestedServiceQuotaChange (Prelude.Maybe Prelude.Text)
requestedServiceQuotaChange_quotaName = Lens.lens (\RequestedServiceQuotaChange' {quotaName} -> quotaName) (\s@RequestedServiceQuotaChange' {} a -> s {quotaName = a} :: RequestedServiceQuotaChange)

-- | The service name.
requestedServiceQuotaChange_serviceName :: Lens.Lens' RequestedServiceQuotaChange (Prelude.Maybe Prelude.Text)
requestedServiceQuotaChange_serviceName = Lens.lens (\RequestedServiceQuotaChange' {serviceName} -> serviceName) (\s@RequestedServiceQuotaChange' {} a -> s {serviceName = a} :: RequestedServiceQuotaChange)

-- | The IAM identity of the requester.
requestedServiceQuotaChange_requester :: Lens.Lens' RequestedServiceQuotaChange (Prelude.Maybe Prelude.Text)
requestedServiceQuotaChange_requester = Lens.lens (\RequestedServiceQuotaChange' {requester} -> requester) (\s@RequestedServiceQuotaChange' {} a -> s {requester = a} :: RequestedServiceQuotaChange)

-- | The unit of measurement.
requestedServiceQuotaChange_unit :: Lens.Lens' RequestedServiceQuotaChange (Prelude.Maybe Prelude.Text)
requestedServiceQuotaChange_unit = Lens.lens (\RequestedServiceQuotaChange' {unit} -> unit) (\s@RequestedServiceQuotaChange' {} a -> s {unit = a} :: RequestedServiceQuotaChange)

-- | The new, increased value for the quota.
requestedServiceQuotaChange_desiredValue :: Lens.Lens' RequestedServiceQuotaChange (Prelude.Maybe Prelude.Double)
requestedServiceQuotaChange_desiredValue = Lens.lens (\RequestedServiceQuotaChange' {desiredValue} -> desiredValue) (\s@RequestedServiceQuotaChange' {} a -> s {desiredValue = a} :: RequestedServiceQuotaChange)

instance Core.FromJSON RequestedServiceQuotaChange where
  parseJSON =
    Core.withObject
      "RequestedServiceQuotaChange"
      ( \x ->
          RequestedServiceQuotaChange'
            Prelude.<$> (x Core..:? "QuotaArn")
            Prelude.<*> (x Core..:? "GlobalQuota")
            Prelude.<*> (x Core..:? "QuotaCode")
            Prelude.<*> (x Core..:? "CaseId")
            Prelude.<*> (x Core..:? "Created")
            Prelude.<*> (x Core..:? "ServiceCode")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "LastUpdated")
            Prelude.<*> (x Core..:? "QuotaName")
            Prelude.<*> (x Core..:? "ServiceName")
            Prelude.<*> (x Core..:? "Requester")
            Prelude.<*> (x Core..:? "Unit")
            Prelude.<*> (x Core..:? "DesiredValue")
      )

instance Prelude.Hashable RequestedServiceQuotaChange where
  hashWithSalt _salt RequestedServiceQuotaChange' {..} =
    _salt `Prelude.hashWithSalt` quotaArn
      `Prelude.hashWithSalt` globalQuota
      `Prelude.hashWithSalt` quotaCode
      `Prelude.hashWithSalt` caseId
      `Prelude.hashWithSalt` created
      `Prelude.hashWithSalt` serviceCode
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` lastUpdated
      `Prelude.hashWithSalt` quotaName
      `Prelude.hashWithSalt` serviceName
      `Prelude.hashWithSalt` requester
      `Prelude.hashWithSalt` unit
      `Prelude.hashWithSalt` desiredValue

instance Prelude.NFData RequestedServiceQuotaChange where
  rnf RequestedServiceQuotaChange' {..} =
    Prelude.rnf quotaArn
      `Prelude.seq` Prelude.rnf globalQuota
      `Prelude.seq` Prelude.rnf quotaCode
      `Prelude.seq` Prelude.rnf caseId
      `Prelude.seq` Prelude.rnf created
      `Prelude.seq` Prelude.rnf serviceCode
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf lastUpdated
      `Prelude.seq` Prelude.rnf quotaName
      `Prelude.seq` Prelude.rnf serviceName
      `Prelude.seq` Prelude.rnf requester
      `Prelude.seq` Prelude.rnf unit
      `Prelude.seq` Prelude.rnf desiredValue
