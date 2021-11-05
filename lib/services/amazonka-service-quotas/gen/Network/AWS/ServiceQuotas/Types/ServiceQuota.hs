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
-- Module      : Network.AWS.ServiceQuotas.Types.ServiceQuota
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceQuotas.Types.ServiceQuota where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.ServiceQuotas.Types.ErrorReason
import Network.AWS.ServiceQuotas.Types.MetricInfo
import Network.AWS.ServiceQuotas.Types.QuotaPeriod

-- | Information about a quota.
--
-- /See:/ 'newServiceQuota' smart constructor.
data ServiceQuota = ServiceQuota'
  { -- | Indicates whether the quota is global.
    globalQuota :: Prelude.Maybe Prelude.Bool,
    -- | The period of time.
    period :: Prelude.Maybe QuotaPeriod,
    -- | The quota value.
    value :: Prelude.Maybe Prelude.Double,
    -- | The Amazon Resource Name (ARN) of the quota.
    quotaArn :: Prelude.Maybe Prelude.Text,
    -- | Information about the measurement.
    usageMetric :: Prelude.Maybe MetricInfo,
    -- | The error code and error reason.
    errorReason :: Prelude.Maybe ErrorReason,
    -- | Indicates whether the quota value can be increased.
    adjustable :: Prelude.Maybe Prelude.Bool,
    -- | The service name.
    serviceName :: Prelude.Maybe Prelude.Text,
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
-- Create a value of 'ServiceQuota' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalQuota', 'serviceQuota_globalQuota' - Indicates whether the quota is global.
--
-- 'period', 'serviceQuota_period' - The period of time.
--
-- 'value', 'serviceQuota_value' - The quota value.
--
-- 'quotaArn', 'serviceQuota_quotaArn' - The Amazon Resource Name (ARN) of the quota.
--
-- 'usageMetric', 'serviceQuota_usageMetric' - Information about the measurement.
--
-- 'errorReason', 'serviceQuota_errorReason' - The error code and error reason.
--
-- 'adjustable', 'serviceQuota_adjustable' - Indicates whether the quota value can be increased.
--
-- 'serviceName', 'serviceQuota_serviceName' - The service name.
--
-- 'serviceCode', 'serviceQuota_serviceCode' - The service identifier.
--
-- 'quotaCode', 'serviceQuota_quotaCode' - The quota identifier.
--
-- 'unit', 'serviceQuota_unit' - The unit of measurement.
--
-- 'quotaName', 'serviceQuota_quotaName' - The quota name.
newServiceQuota ::
  ServiceQuota
newServiceQuota =
  ServiceQuota'
    { globalQuota = Prelude.Nothing,
      period = Prelude.Nothing,
      value = Prelude.Nothing,
      quotaArn = Prelude.Nothing,
      usageMetric = Prelude.Nothing,
      errorReason = Prelude.Nothing,
      adjustable = Prelude.Nothing,
      serviceName = Prelude.Nothing,
      serviceCode = Prelude.Nothing,
      quotaCode = Prelude.Nothing,
      unit = Prelude.Nothing,
      quotaName = Prelude.Nothing
    }

-- | Indicates whether the quota is global.
serviceQuota_globalQuota :: Lens.Lens' ServiceQuota (Prelude.Maybe Prelude.Bool)
serviceQuota_globalQuota = Lens.lens (\ServiceQuota' {globalQuota} -> globalQuota) (\s@ServiceQuota' {} a -> s {globalQuota = a} :: ServiceQuota)

-- | The period of time.
serviceQuota_period :: Lens.Lens' ServiceQuota (Prelude.Maybe QuotaPeriod)
serviceQuota_period = Lens.lens (\ServiceQuota' {period} -> period) (\s@ServiceQuota' {} a -> s {period = a} :: ServiceQuota)

-- | The quota value.
serviceQuota_value :: Lens.Lens' ServiceQuota (Prelude.Maybe Prelude.Double)
serviceQuota_value = Lens.lens (\ServiceQuota' {value} -> value) (\s@ServiceQuota' {} a -> s {value = a} :: ServiceQuota)

-- | The Amazon Resource Name (ARN) of the quota.
serviceQuota_quotaArn :: Lens.Lens' ServiceQuota (Prelude.Maybe Prelude.Text)
serviceQuota_quotaArn = Lens.lens (\ServiceQuota' {quotaArn} -> quotaArn) (\s@ServiceQuota' {} a -> s {quotaArn = a} :: ServiceQuota)

-- | Information about the measurement.
serviceQuota_usageMetric :: Lens.Lens' ServiceQuota (Prelude.Maybe MetricInfo)
serviceQuota_usageMetric = Lens.lens (\ServiceQuota' {usageMetric} -> usageMetric) (\s@ServiceQuota' {} a -> s {usageMetric = a} :: ServiceQuota)

-- | The error code and error reason.
serviceQuota_errorReason :: Lens.Lens' ServiceQuota (Prelude.Maybe ErrorReason)
serviceQuota_errorReason = Lens.lens (\ServiceQuota' {errorReason} -> errorReason) (\s@ServiceQuota' {} a -> s {errorReason = a} :: ServiceQuota)

-- | Indicates whether the quota value can be increased.
serviceQuota_adjustable :: Lens.Lens' ServiceQuota (Prelude.Maybe Prelude.Bool)
serviceQuota_adjustable = Lens.lens (\ServiceQuota' {adjustable} -> adjustable) (\s@ServiceQuota' {} a -> s {adjustable = a} :: ServiceQuota)

-- | The service name.
serviceQuota_serviceName :: Lens.Lens' ServiceQuota (Prelude.Maybe Prelude.Text)
serviceQuota_serviceName = Lens.lens (\ServiceQuota' {serviceName} -> serviceName) (\s@ServiceQuota' {} a -> s {serviceName = a} :: ServiceQuota)

-- | The service identifier.
serviceQuota_serviceCode :: Lens.Lens' ServiceQuota (Prelude.Maybe Prelude.Text)
serviceQuota_serviceCode = Lens.lens (\ServiceQuota' {serviceCode} -> serviceCode) (\s@ServiceQuota' {} a -> s {serviceCode = a} :: ServiceQuota)

-- | The quota identifier.
serviceQuota_quotaCode :: Lens.Lens' ServiceQuota (Prelude.Maybe Prelude.Text)
serviceQuota_quotaCode = Lens.lens (\ServiceQuota' {quotaCode} -> quotaCode) (\s@ServiceQuota' {} a -> s {quotaCode = a} :: ServiceQuota)

-- | The unit of measurement.
serviceQuota_unit :: Lens.Lens' ServiceQuota (Prelude.Maybe Prelude.Text)
serviceQuota_unit = Lens.lens (\ServiceQuota' {unit} -> unit) (\s@ServiceQuota' {} a -> s {unit = a} :: ServiceQuota)

-- | The quota name.
serviceQuota_quotaName :: Lens.Lens' ServiceQuota (Prelude.Maybe Prelude.Text)
serviceQuota_quotaName = Lens.lens (\ServiceQuota' {quotaName} -> quotaName) (\s@ServiceQuota' {} a -> s {quotaName = a} :: ServiceQuota)

instance Core.FromJSON ServiceQuota where
  parseJSON =
    Core.withObject
      "ServiceQuota"
      ( \x ->
          ServiceQuota'
            Prelude.<$> (x Core..:? "GlobalQuota")
            Prelude.<*> (x Core..:? "Period")
            Prelude.<*> (x Core..:? "Value")
            Prelude.<*> (x Core..:? "QuotaArn")
            Prelude.<*> (x Core..:? "UsageMetric")
            Prelude.<*> (x Core..:? "ErrorReason")
            Prelude.<*> (x Core..:? "Adjustable")
            Prelude.<*> (x Core..:? "ServiceName")
            Prelude.<*> (x Core..:? "ServiceCode")
            Prelude.<*> (x Core..:? "QuotaCode")
            Prelude.<*> (x Core..:? "Unit")
            Prelude.<*> (x Core..:? "QuotaName")
      )

instance Prelude.Hashable ServiceQuota

instance Prelude.NFData ServiceQuota
