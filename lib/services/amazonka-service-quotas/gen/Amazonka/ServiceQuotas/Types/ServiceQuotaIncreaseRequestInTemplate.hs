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
-- Module      : Amazonka.ServiceQuotas.Types.ServiceQuotaIncreaseRequestInTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceQuotas.Types.ServiceQuotaIncreaseRequestInTemplate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a quota increase request.
--
-- /See:/ 'newServiceQuotaIncreaseRequestInTemplate' smart constructor.
data ServiceQuotaIncreaseRequestInTemplate = ServiceQuotaIncreaseRequestInTemplate'
  { -- | The AWS Region.
    awsRegion :: Prelude.Maybe Prelude.Text,
    -- | The new, increased value of the quota.
    desiredValue :: Prelude.Maybe Prelude.Double,
    -- | Indicates whether the quota is global.
    globalQuota :: Prelude.Maybe Prelude.Bool,
    -- | The quota identifier.
    quotaCode :: Prelude.Maybe Prelude.Text,
    -- | The quota name.
    quotaName :: Prelude.Maybe Prelude.Text,
    -- | The service identifier.
    serviceCode :: Prelude.Maybe Prelude.Text,
    -- | The service name.
    serviceName :: Prelude.Maybe Prelude.Text,
    -- | The unit of measurement.
    unit :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceQuotaIncreaseRequestInTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsRegion', 'serviceQuotaIncreaseRequestInTemplate_awsRegion' - The AWS Region.
--
-- 'desiredValue', 'serviceQuotaIncreaseRequestInTemplate_desiredValue' - The new, increased value of the quota.
--
-- 'globalQuota', 'serviceQuotaIncreaseRequestInTemplate_globalQuota' - Indicates whether the quota is global.
--
-- 'quotaCode', 'serviceQuotaIncreaseRequestInTemplate_quotaCode' - The quota identifier.
--
-- 'quotaName', 'serviceQuotaIncreaseRequestInTemplate_quotaName' - The quota name.
--
-- 'serviceCode', 'serviceQuotaIncreaseRequestInTemplate_serviceCode' - The service identifier.
--
-- 'serviceName', 'serviceQuotaIncreaseRequestInTemplate_serviceName' - The service name.
--
-- 'unit', 'serviceQuotaIncreaseRequestInTemplate_unit' - The unit of measurement.
newServiceQuotaIncreaseRequestInTemplate ::
  ServiceQuotaIncreaseRequestInTemplate
newServiceQuotaIncreaseRequestInTemplate =
  ServiceQuotaIncreaseRequestInTemplate'
    { awsRegion =
        Prelude.Nothing,
      desiredValue = Prelude.Nothing,
      globalQuota = Prelude.Nothing,
      quotaCode = Prelude.Nothing,
      quotaName = Prelude.Nothing,
      serviceCode = Prelude.Nothing,
      serviceName = Prelude.Nothing,
      unit = Prelude.Nothing
    }

-- | The AWS Region.
serviceQuotaIncreaseRequestInTemplate_awsRegion :: Lens.Lens' ServiceQuotaIncreaseRequestInTemplate (Prelude.Maybe Prelude.Text)
serviceQuotaIncreaseRequestInTemplate_awsRegion = Lens.lens (\ServiceQuotaIncreaseRequestInTemplate' {awsRegion} -> awsRegion) (\s@ServiceQuotaIncreaseRequestInTemplate' {} a -> s {awsRegion = a} :: ServiceQuotaIncreaseRequestInTemplate)

-- | The new, increased value of the quota.
serviceQuotaIncreaseRequestInTemplate_desiredValue :: Lens.Lens' ServiceQuotaIncreaseRequestInTemplate (Prelude.Maybe Prelude.Double)
serviceQuotaIncreaseRequestInTemplate_desiredValue = Lens.lens (\ServiceQuotaIncreaseRequestInTemplate' {desiredValue} -> desiredValue) (\s@ServiceQuotaIncreaseRequestInTemplate' {} a -> s {desiredValue = a} :: ServiceQuotaIncreaseRequestInTemplate)

-- | Indicates whether the quota is global.
serviceQuotaIncreaseRequestInTemplate_globalQuota :: Lens.Lens' ServiceQuotaIncreaseRequestInTemplate (Prelude.Maybe Prelude.Bool)
serviceQuotaIncreaseRequestInTemplate_globalQuota = Lens.lens (\ServiceQuotaIncreaseRequestInTemplate' {globalQuota} -> globalQuota) (\s@ServiceQuotaIncreaseRequestInTemplate' {} a -> s {globalQuota = a} :: ServiceQuotaIncreaseRequestInTemplate)

-- | The quota identifier.
serviceQuotaIncreaseRequestInTemplate_quotaCode :: Lens.Lens' ServiceQuotaIncreaseRequestInTemplate (Prelude.Maybe Prelude.Text)
serviceQuotaIncreaseRequestInTemplate_quotaCode = Lens.lens (\ServiceQuotaIncreaseRequestInTemplate' {quotaCode} -> quotaCode) (\s@ServiceQuotaIncreaseRequestInTemplate' {} a -> s {quotaCode = a} :: ServiceQuotaIncreaseRequestInTemplate)

-- | The quota name.
serviceQuotaIncreaseRequestInTemplate_quotaName :: Lens.Lens' ServiceQuotaIncreaseRequestInTemplate (Prelude.Maybe Prelude.Text)
serviceQuotaIncreaseRequestInTemplate_quotaName = Lens.lens (\ServiceQuotaIncreaseRequestInTemplate' {quotaName} -> quotaName) (\s@ServiceQuotaIncreaseRequestInTemplate' {} a -> s {quotaName = a} :: ServiceQuotaIncreaseRequestInTemplate)

-- | The service identifier.
serviceQuotaIncreaseRequestInTemplate_serviceCode :: Lens.Lens' ServiceQuotaIncreaseRequestInTemplate (Prelude.Maybe Prelude.Text)
serviceQuotaIncreaseRequestInTemplate_serviceCode = Lens.lens (\ServiceQuotaIncreaseRequestInTemplate' {serviceCode} -> serviceCode) (\s@ServiceQuotaIncreaseRequestInTemplate' {} a -> s {serviceCode = a} :: ServiceQuotaIncreaseRequestInTemplate)

-- | The service name.
serviceQuotaIncreaseRequestInTemplate_serviceName :: Lens.Lens' ServiceQuotaIncreaseRequestInTemplate (Prelude.Maybe Prelude.Text)
serviceQuotaIncreaseRequestInTemplate_serviceName = Lens.lens (\ServiceQuotaIncreaseRequestInTemplate' {serviceName} -> serviceName) (\s@ServiceQuotaIncreaseRequestInTemplate' {} a -> s {serviceName = a} :: ServiceQuotaIncreaseRequestInTemplate)

-- | The unit of measurement.
serviceQuotaIncreaseRequestInTemplate_unit :: Lens.Lens' ServiceQuotaIncreaseRequestInTemplate (Prelude.Maybe Prelude.Text)
serviceQuotaIncreaseRequestInTemplate_unit = Lens.lens (\ServiceQuotaIncreaseRequestInTemplate' {unit} -> unit) (\s@ServiceQuotaIncreaseRequestInTemplate' {} a -> s {unit = a} :: ServiceQuotaIncreaseRequestInTemplate)

instance
  Data.FromJSON
    ServiceQuotaIncreaseRequestInTemplate
  where
  parseJSON =
    Data.withObject
      "ServiceQuotaIncreaseRequestInTemplate"
      ( \x ->
          ServiceQuotaIncreaseRequestInTemplate'
            Prelude.<$> (x Data..:? "AwsRegion")
            Prelude.<*> (x Data..:? "DesiredValue")
            Prelude.<*> (x Data..:? "GlobalQuota")
            Prelude.<*> (x Data..:? "QuotaCode")
            Prelude.<*> (x Data..:? "QuotaName")
            Prelude.<*> (x Data..:? "ServiceCode")
            Prelude.<*> (x Data..:? "ServiceName")
            Prelude.<*> (x Data..:? "Unit")
      )

instance
  Prelude.Hashable
    ServiceQuotaIncreaseRequestInTemplate
  where
  hashWithSalt
    _salt
    ServiceQuotaIncreaseRequestInTemplate' {..} =
      _salt
        `Prelude.hashWithSalt` awsRegion
        `Prelude.hashWithSalt` desiredValue
        `Prelude.hashWithSalt` globalQuota
        `Prelude.hashWithSalt` quotaCode
        `Prelude.hashWithSalt` quotaName
        `Prelude.hashWithSalt` serviceCode
        `Prelude.hashWithSalt` serviceName
        `Prelude.hashWithSalt` unit

instance
  Prelude.NFData
    ServiceQuotaIncreaseRequestInTemplate
  where
  rnf ServiceQuotaIncreaseRequestInTemplate' {..} =
    Prelude.rnf awsRegion `Prelude.seq`
      Prelude.rnf desiredValue `Prelude.seq`
        Prelude.rnf globalQuota `Prelude.seq`
          Prelude.rnf quotaCode `Prelude.seq`
            Prelude.rnf quotaName `Prelude.seq`
              Prelude.rnf serviceCode `Prelude.seq`
                Prelude.rnf serviceName `Prelude.seq`
                  Prelude.rnf unit
