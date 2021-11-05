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
-- Module      : Network.AWS.ServiceQuotas.Types.ServiceQuotaIncreaseRequestInTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceQuotas.Types.ServiceQuotaIncreaseRequestInTemplate where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a quota increase request.
--
-- /See:/ 'newServiceQuotaIncreaseRequestInTemplate' smart constructor.
data ServiceQuotaIncreaseRequestInTemplate = ServiceQuotaIncreaseRequestInTemplate'
  { -- | Indicates whether the quota is global.
    globalQuota :: Prelude.Maybe Prelude.Bool,
    -- | The new, increased value of the quota.
    desiredValue :: Prelude.Maybe Prelude.Double,
    -- | The service name.
    serviceName :: Prelude.Maybe Prelude.Text,
    -- | The AWS Region.
    awsRegion :: Prelude.Maybe Prelude.Text,
    -- | The service identifier.
    serviceCode :: Prelude.Maybe Prelude.Text,
    -- | The quota identifier.
    quotaCode :: Prelude.Maybe Prelude.Text,
    -- | The unit of measurement.
    unit :: Prelude.Maybe Prelude.Text,
    -- | The quota name.
    quotaName :: Prelude.Maybe Prelude.Text
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
-- 'globalQuota', 'serviceQuotaIncreaseRequestInTemplate_globalQuota' - Indicates whether the quota is global.
--
-- 'desiredValue', 'serviceQuotaIncreaseRequestInTemplate_desiredValue' - The new, increased value of the quota.
--
-- 'serviceName', 'serviceQuotaIncreaseRequestInTemplate_serviceName' - The service name.
--
-- 'awsRegion', 'serviceQuotaIncreaseRequestInTemplate_awsRegion' - The AWS Region.
--
-- 'serviceCode', 'serviceQuotaIncreaseRequestInTemplate_serviceCode' - The service identifier.
--
-- 'quotaCode', 'serviceQuotaIncreaseRequestInTemplate_quotaCode' - The quota identifier.
--
-- 'unit', 'serviceQuotaIncreaseRequestInTemplate_unit' - The unit of measurement.
--
-- 'quotaName', 'serviceQuotaIncreaseRequestInTemplate_quotaName' - The quota name.
newServiceQuotaIncreaseRequestInTemplate ::
  ServiceQuotaIncreaseRequestInTemplate
newServiceQuotaIncreaseRequestInTemplate =
  ServiceQuotaIncreaseRequestInTemplate'
    { globalQuota =
        Prelude.Nothing,
      desiredValue = Prelude.Nothing,
      serviceName = Prelude.Nothing,
      awsRegion = Prelude.Nothing,
      serviceCode = Prelude.Nothing,
      quotaCode = Prelude.Nothing,
      unit = Prelude.Nothing,
      quotaName = Prelude.Nothing
    }

-- | Indicates whether the quota is global.
serviceQuotaIncreaseRequestInTemplate_globalQuota :: Lens.Lens' ServiceQuotaIncreaseRequestInTemplate (Prelude.Maybe Prelude.Bool)
serviceQuotaIncreaseRequestInTemplate_globalQuota = Lens.lens (\ServiceQuotaIncreaseRequestInTemplate' {globalQuota} -> globalQuota) (\s@ServiceQuotaIncreaseRequestInTemplate' {} a -> s {globalQuota = a} :: ServiceQuotaIncreaseRequestInTemplate)

-- | The new, increased value of the quota.
serviceQuotaIncreaseRequestInTemplate_desiredValue :: Lens.Lens' ServiceQuotaIncreaseRequestInTemplate (Prelude.Maybe Prelude.Double)
serviceQuotaIncreaseRequestInTemplate_desiredValue = Lens.lens (\ServiceQuotaIncreaseRequestInTemplate' {desiredValue} -> desiredValue) (\s@ServiceQuotaIncreaseRequestInTemplate' {} a -> s {desiredValue = a} :: ServiceQuotaIncreaseRequestInTemplate)

-- | The service name.
serviceQuotaIncreaseRequestInTemplate_serviceName :: Lens.Lens' ServiceQuotaIncreaseRequestInTemplate (Prelude.Maybe Prelude.Text)
serviceQuotaIncreaseRequestInTemplate_serviceName = Lens.lens (\ServiceQuotaIncreaseRequestInTemplate' {serviceName} -> serviceName) (\s@ServiceQuotaIncreaseRequestInTemplate' {} a -> s {serviceName = a} :: ServiceQuotaIncreaseRequestInTemplate)

-- | The AWS Region.
serviceQuotaIncreaseRequestInTemplate_awsRegion :: Lens.Lens' ServiceQuotaIncreaseRequestInTemplate (Prelude.Maybe Prelude.Text)
serviceQuotaIncreaseRequestInTemplate_awsRegion = Lens.lens (\ServiceQuotaIncreaseRequestInTemplate' {awsRegion} -> awsRegion) (\s@ServiceQuotaIncreaseRequestInTemplate' {} a -> s {awsRegion = a} :: ServiceQuotaIncreaseRequestInTemplate)

-- | The service identifier.
serviceQuotaIncreaseRequestInTemplate_serviceCode :: Lens.Lens' ServiceQuotaIncreaseRequestInTemplate (Prelude.Maybe Prelude.Text)
serviceQuotaIncreaseRequestInTemplate_serviceCode = Lens.lens (\ServiceQuotaIncreaseRequestInTemplate' {serviceCode} -> serviceCode) (\s@ServiceQuotaIncreaseRequestInTemplate' {} a -> s {serviceCode = a} :: ServiceQuotaIncreaseRequestInTemplate)

-- | The quota identifier.
serviceQuotaIncreaseRequestInTemplate_quotaCode :: Lens.Lens' ServiceQuotaIncreaseRequestInTemplate (Prelude.Maybe Prelude.Text)
serviceQuotaIncreaseRequestInTemplate_quotaCode = Lens.lens (\ServiceQuotaIncreaseRequestInTemplate' {quotaCode} -> quotaCode) (\s@ServiceQuotaIncreaseRequestInTemplate' {} a -> s {quotaCode = a} :: ServiceQuotaIncreaseRequestInTemplate)

-- | The unit of measurement.
serviceQuotaIncreaseRequestInTemplate_unit :: Lens.Lens' ServiceQuotaIncreaseRequestInTemplate (Prelude.Maybe Prelude.Text)
serviceQuotaIncreaseRequestInTemplate_unit = Lens.lens (\ServiceQuotaIncreaseRequestInTemplate' {unit} -> unit) (\s@ServiceQuotaIncreaseRequestInTemplate' {} a -> s {unit = a} :: ServiceQuotaIncreaseRequestInTemplate)

-- | The quota name.
serviceQuotaIncreaseRequestInTemplate_quotaName :: Lens.Lens' ServiceQuotaIncreaseRequestInTemplate (Prelude.Maybe Prelude.Text)
serviceQuotaIncreaseRequestInTemplate_quotaName = Lens.lens (\ServiceQuotaIncreaseRequestInTemplate' {quotaName} -> quotaName) (\s@ServiceQuotaIncreaseRequestInTemplate' {} a -> s {quotaName = a} :: ServiceQuotaIncreaseRequestInTemplate)

instance
  Core.FromJSON
    ServiceQuotaIncreaseRequestInTemplate
  where
  parseJSON =
    Core.withObject
      "ServiceQuotaIncreaseRequestInTemplate"
      ( \x ->
          ServiceQuotaIncreaseRequestInTemplate'
            Prelude.<$> (x Core..:? "GlobalQuota")
            Prelude.<*> (x Core..:? "DesiredValue")
            Prelude.<*> (x Core..:? "ServiceName")
            Prelude.<*> (x Core..:? "AwsRegion")
            Prelude.<*> (x Core..:? "ServiceCode")
            Prelude.<*> (x Core..:? "QuotaCode")
            Prelude.<*> (x Core..:? "Unit")
            Prelude.<*> (x Core..:? "QuotaName")
      )

instance
  Prelude.Hashable
    ServiceQuotaIncreaseRequestInTemplate

instance
  Prelude.NFData
    ServiceQuotaIncreaseRequestInTemplate
