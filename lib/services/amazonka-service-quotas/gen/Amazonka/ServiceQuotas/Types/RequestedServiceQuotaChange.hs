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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceQuotas.Types.RequestedServiceQuotaChange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.ServiceQuotas.Types.RequestStatus

-- | Information about a quota increase request.
--
-- /See:/ 'newRequestedServiceQuotaChange' smart constructor.
data RequestedServiceQuotaChange = RequestedServiceQuotaChange'
  { -- | The case ID.
    caseId :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the quota increase request was received and the
    -- case ID was created.
    created :: Prelude.Maybe Data.POSIX,
    -- | The new, increased value for the quota.
    desiredValue :: Prelude.Maybe Prelude.Double,
    -- | Indicates whether the quota is global.
    globalQuota :: Prelude.Maybe Prelude.Bool,
    -- | The unique identifier.
    id :: Prelude.Maybe Prelude.Text,
    -- | The date and time of the most recent change.
    lastUpdated :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the quota.
    quotaArn :: Prelude.Maybe Prelude.Text,
    -- | The quota identifier.
    quotaCode :: Prelude.Maybe Prelude.Text,
    -- | The quota name.
    quotaName :: Prelude.Maybe Prelude.Text,
    -- | The IAM identity of the requester.
    requester :: Prelude.Maybe Prelude.Text,
    -- | The service identifier.
    serviceCode :: Prelude.Maybe Prelude.Text,
    -- | The service name.
    serviceName :: Prelude.Maybe Prelude.Text,
    -- | The state of the quota increase request.
    status :: Prelude.Maybe RequestStatus,
    -- | The unit of measurement.
    unit :: Prelude.Maybe Prelude.Text
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
-- 'caseId', 'requestedServiceQuotaChange_caseId' - The case ID.
--
-- 'created', 'requestedServiceQuotaChange_created' - The date and time when the quota increase request was received and the
-- case ID was created.
--
-- 'desiredValue', 'requestedServiceQuotaChange_desiredValue' - The new, increased value for the quota.
--
-- 'globalQuota', 'requestedServiceQuotaChange_globalQuota' - Indicates whether the quota is global.
--
-- 'id', 'requestedServiceQuotaChange_id' - The unique identifier.
--
-- 'lastUpdated', 'requestedServiceQuotaChange_lastUpdated' - The date and time of the most recent change.
--
-- 'quotaArn', 'requestedServiceQuotaChange_quotaArn' - The Amazon Resource Name (ARN) of the quota.
--
-- 'quotaCode', 'requestedServiceQuotaChange_quotaCode' - The quota identifier.
--
-- 'quotaName', 'requestedServiceQuotaChange_quotaName' - The quota name.
--
-- 'requester', 'requestedServiceQuotaChange_requester' - The IAM identity of the requester.
--
-- 'serviceCode', 'requestedServiceQuotaChange_serviceCode' - The service identifier.
--
-- 'serviceName', 'requestedServiceQuotaChange_serviceName' - The service name.
--
-- 'status', 'requestedServiceQuotaChange_status' - The state of the quota increase request.
--
-- 'unit', 'requestedServiceQuotaChange_unit' - The unit of measurement.
newRequestedServiceQuotaChange ::
  RequestedServiceQuotaChange
newRequestedServiceQuotaChange =
  RequestedServiceQuotaChange'
    { caseId =
        Prelude.Nothing,
      created = Prelude.Nothing,
      desiredValue = Prelude.Nothing,
      globalQuota = Prelude.Nothing,
      id = Prelude.Nothing,
      lastUpdated = Prelude.Nothing,
      quotaArn = Prelude.Nothing,
      quotaCode = Prelude.Nothing,
      quotaName = Prelude.Nothing,
      requester = Prelude.Nothing,
      serviceCode = Prelude.Nothing,
      serviceName = Prelude.Nothing,
      status = Prelude.Nothing,
      unit = Prelude.Nothing
    }

-- | The case ID.
requestedServiceQuotaChange_caseId :: Lens.Lens' RequestedServiceQuotaChange (Prelude.Maybe Prelude.Text)
requestedServiceQuotaChange_caseId = Lens.lens (\RequestedServiceQuotaChange' {caseId} -> caseId) (\s@RequestedServiceQuotaChange' {} a -> s {caseId = a} :: RequestedServiceQuotaChange)

-- | The date and time when the quota increase request was received and the
-- case ID was created.
requestedServiceQuotaChange_created :: Lens.Lens' RequestedServiceQuotaChange (Prelude.Maybe Prelude.UTCTime)
requestedServiceQuotaChange_created = Lens.lens (\RequestedServiceQuotaChange' {created} -> created) (\s@RequestedServiceQuotaChange' {} a -> s {created = a} :: RequestedServiceQuotaChange) Prelude.. Lens.mapping Data._Time

-- | The new, increased value for the quota.
requestedServiceQuotaChange_desiredValue :: Lens.Lens' RequestedServiceQuotaChange (Prelude.Maybe Prelude.Double)
requestedServiceQuotaChange_desiredValue = Lens.lens (\RequestedServiceQuotaChange' {desiredValue} -> desiredValue) (\s@RequestedServiceQuotaChange' {} a -> s {desiredValue = a} :: RequestedServiceQuotaChange)

-- | Indicates whether the quota is global.
requestedServiceQuotaChange_globalQuota :: Lens.Lens' RequestedServiceQuotaChange (Prelude.Maybe Prelude.Bool)
requestedServiceQuotaChange_globalQuota = Lens.lens (\RequestedServiceQuotaChange' {globalQuota} -> globalQuota) (\s@RequestedServiceQuotaChange' {} a -> s {globalQuota = a} :: RequestedServiceQuotaChange)

-- | The unique identifier.
requestedServiceQuotaChange_id :: Lens.Lens' RequestedServiceQuotaChange (Prelude.Maybe Prelude.Text)
requestedServiceQuotaChange_id = Lens.lens (\RequestedServiceQuotaChange' {id} -> id) (\s@RequestedServiceQuotaChange' {} a -> s {id = a} :: RequestedServiceQuotaChange)

-- | The date and time of the most recent change.
requestedServiceQuotaChange_lastUpdated :: Lens.Lens' RequestedServiceQuotaChange (Prelude.Maybe Prelude.UTCTime)
requestedServiceQuotaChange_lastUpdated = Lens.lens (\RequestedServiceQuotaChange' {lastUpdated} -> lastUpdated) (\s@RequestedServiceQuotaChange' {} a -> s {lastUpdated = a} :: RequestedServiceQuotaChange) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the quota.
requestedServiceQuotaChange_quotaArn :: Lens.Lens' RequestedServiceQuotaChange (Prelude.Maybe Prelude.Text)
requestedServiceQuotaChange_quotaArn = Lens.lens (\RequestedServiceQuotaChange' {quotaArn} -> quotaArn) (\s@RequestedServiceQuotaChange' {} a -> s {quotaArn = a} :: RequestedServiceQuotaChange)

-- | The quota identifier.
requestedServiceQuotaChange_quotaCode :: Lens.Lens' RequestedServiceQuotaChange (Prelude.Maybe Prelude.Text)
requestedServiceQuotaChange_quotaCode = Lens.lens (\RequestedServiceQuotaChange' {quotaCode} -> quotaCode) (\s@RequestedServiceQuotaChange' {} a -> s {quotaCode = a} :: RequestedServiceQuotaChange)

-- | The quota name.
requestedServiceQuotaChange_quotaName :: Lens.Lens' RequestedServiceQuotaChange (Prelude.Maybe Prelude.Text)
requestedServiceQuotaChange_quotaName = Lens.lens (\RequestedServiceQuotaChange' {quotaName} -> quotaName) (\s@RequestedServiceQuotaChange' {} a -> s {quotaName = a} :: RequestedServiceQuotaChange)

-- | The IAM identity of the requester.
requestedServiceQuotaChange_requester :: Lens.Lens' RequestedServiceQuotaChange (Prelude.Maybe Prelude.Text)
requestedServiceQuotaChange_requester = Lens.lens (\RequestedServiceQuotaChange' {requester} -> requester) (\s@RequestedServiceQuotaChange' {} a -> s {requester = a} :: RequestedServiceQuotaChange)

-- | The service identifier.
requestedServiceQuotaChange_serviceCode :: Lens.Lens' RequestedServiceQuotaChange (Prelude.Maybe Prelude.Text)
requestedServiceQuotaChange_serviceCode = Lens.lens (\RequestedServiceQuotaChange' {serviceCode} -> serviceCode) (\s@RequestedServiceQuotaChange' {} a -> s {serviceCode = a} :: RequestedServiceQuotaChange)

-- | The service name.
requestedServiceQuotaChange_serviceName :: Lens.Lens' RequestedServiceQuotaChange (Prelude.Maybe Prelude.Text)
requestedServiceQuotaChange_serviceName = Lens.lens (\RequestedServiceQuotaChange' {serviceName} -> serviceName) (\s@RequestedServiceQuotaChange' {} a -> s {serviceName = a} :: RequestedServiceQuotaChange)

-- | The state of the quota increase request.
requestedServiceQuotaChange_status :: Lens.Lens' RequestedServiceQuotaChange (Prelude.Maybe RequestStatus)
requestedServiceQuotaChange_status = Lens.lens (\RequestedServiceQuotaChange' {status} -> status) (\s@RequestedServiceQuotaChange' {} a -> s {status = a} :: RequestedServiceQuotaChange)

-- | The unit of measurement.
requestedServiceQuotaChange_unit :: Lens.Lens' RequestedServiceQuotaChange (Prelude.Maybe Prelude.Text)
requestedServiceQuotaChange_unit = Lens.lens (\RequestedServiceQuotaChange' {unit} -> unit) (\s@RequestedServiceQuotaChange' {} a -> s {unit = a} :: RequestedServiceQuotaChange)

instance Data.FromJSON RequestedServiceQuotaChange where
  parseJSON =
    Data.withObject
      "RequestedServiceQuotaChange"
      ( \x ->
          RequestedServiceQuotaChange'
            Prelude.<$> (x Data..:? "CaseId")
            Prelude.<*> (x Data..:? "Created")
            Prelude.<*> (x Data..:? "DesiredValue")
            Prelude.<*> (x Data..:? "GlobalQuota")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "LastUpdated")
            Prelude.<*> (x Data..:? "QuotaArn")
            Prelude.<*> (x Data..:? "QuotaCode")
            Prelude.<*> (x Data..:? "QuotaName")
            Prelude.<*> (x Data..:? "Requester")
            Prelude.<*> (x Data..:? "ServiceCode")
            Prelude.<*> (x Data..:? "ServiceName")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "Unit")
      )

instance Prelude.Hashable RequestedServiceQuotaChange where
  hashWithSalt _salt RequestedServiceQuotaChange' {..} =
    _salt
      `Prelude.hashWithSalt` caseId
      `Prelude.hashWithSalt` created
      `Prelude.hashWithSalt` desiredValue
      `Prelude.hashWithSalt` globalQuota
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` lastUpdated
      `Prelude.hashWithSalt` quotaArn
      `Prelude.hashWithSalt` quotaCode
      `Prelude.hashWithSalt` quotaName
      `Prelude.hashWithSalt` requester
      `Prelude.hashWithSalt` serviceCode
      `Prelude.hashWithSalt` serviceName
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` unit

instance Prelude.NFData RequestedServiceQuotaChange where
  rnf RequestedServiceQuotaChange' {..} =
    Prelude.rnf caseId
      `Prelude.seq` Prelude.rnf created
      `Prelude.seq` Prelude.rnf desiredValue
      `Prelude.seq` Prelude.rnf globalQuota
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf lastUpdated
      `Prelude.seq` Prelude.rnf quotaArn
      `Prelude.seq` Prelude.rnf quotaCode
      `Prelude.seq` Prelude.rnf quotaName
      `Prelude.seq` Prelude.rnf requester
      `Prelude.seq` Prelude.rnf serviceCode
      `Prelude.seq` Prelude.rnf serviceName
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf unit
