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
-- Module      : Amazonka.ServiceQuotas.Types.ServiceQuota
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceQuotas.Types.ServiceQuota where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.ServiceQuotas.Types.ErrorReason
import Amazonka.ServiceQuotas.Types.MetricInfo
import Amazonka.ServiceQuotas.Types.QuotaPeriod

-- | Information about a quota.
--
-- /See:/ 'newServiceQuota' smart constructor.
data ServiceQuota = ServiceQuota'
  { -- | Indicates whether the quota value can be increased.
    adjustable :: Prelude.Maybe Prelude.Bool,
    -- | The error code and error reason.
    errorReason :: Prelude.Maybe ErrorReason,
    -- | Indicates whether the quota is global.
    globalQuota :: Prelude.Maybe Prelude.Bool,
    -- | The period of time.
    period :: Prelude.Maybe QuotaPeriod,
    -- | The Amazon Resource Name (ARN) of the quota.
    quotaArn :: Prelude.Maybe Prelude.Text,
    -- | The quota identifier.
    quotaCode :: Prelude.Maybe Prelude.Text,
    -- | The quota name.
    quotaName :: Prelude.Maybe Prelude.Text,
    -- | The service identifier.
    serviceCode :: Prelude.Maybe Prelude.Text,
    -- | The service name.
    serviceName :: Prelude.Maybe Prelude.Text,
    -- | The unit of measurement.
    unit :: Prelude.Maybe Prelude.Text,
    -- | Information about the measurement.
    usageMetric :: Prelude.Maybe MetricInfo,
    -- | The quota value.
    value :: Prelude.Maybe Prelude.Double
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
-- 'adjustable', 'serviceQuota_adjustable' - Indicates whether the quota value can be increased.
--
-- 'errorReason', 'serviceQuota_errorReason' - The error code and error reason.
--
-- 'globalQuota', 'serviceQuota_globalQuota' - Indicates whether the quota is global.
--
-- 'period', 'serviceQuota_period' - The period of time.
--
-- 'quotaArn', 'serviceQuota_quotaArn' - The Amazon Resource Name (ARN) of the quota.
--
-- 'quotaCode', 'serviceQuota_quotaCode' - The quota identifier.
--
-- 'quotaName', 'serviceQuota_quotaName' - The quota name.
--
-- 'serviceCode', 'serviceQuota_serviceCode' - The service identifier.
--
-- 'serviceName', 'serviceQuota_serviceName' - The service name.
--
-- 'unit', 'serviceQuota_unit' - The unit of measurement.
--
-- 'usageMetric', 'serviceQuota_usageMetric' - Information about the measurement.
--
-- 'value', 'serviceQuota_value' - The quota value.
newServiceQuota ::
  ServiceQuota
newServiceQuota =
  ServiceQuota'
    { adjustable = Prelude.Nothing,
      errorReason = Prelude.Nothing,
      globalQuota = Prelude.Nothing,
      period = Prelude.Nothing,
      quotaArn = Prelude.Nothing,
      quotaCode = Prelude.Nothing,
      quotaName = Prelude.Nothing,
      serviceCode = Prelude.Nothing,
      serviceName = Prelude.Nothing,
      unit = Prelude.Nothing,
      usageMetric = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | Indicates whether the quota value can be increased.
serviceQuota_adjustable :: Lens.Lens' ServiceQuota (Prelude.Maybe Prelude.Bool)
serviceQuota_adjustable = Lens.lens (\ServiceQuota' {adjustable} -> adjustable) (\s@ServiceQuota' {} a -> s {adjustable = a} :: ServiceQuota)

-- | The error code and error reason.
serviceQuota_errorReason :: Lens.Lens' ServiceQuota (Prelude.Maybe ErrorReason)
serviceQuota_errorReason = Lens.lens (\ServiceQuota' {errorReason} -> errorReason) (\s@ServiceQuota' {} a -> s {errorReason = a} :: ServiceQuota)

-- | Indicates whether the quota is global.
serviceQuota_globalQuota :: Lens.Lens' ServiceQuota (Prelude.Maybe Prelude.Bool)
serviceQuota_globalQuota = Lens.lens (\ServiceQuota' {globalQuota} -> globalQuota) (\s@ServiceQuota' {} a -> s {globalQuota = a} :: ServiceQuota)

-- | The period of time.
serviceQuota_period :: Lens.Lens' ServiceQuota (Prelude.Maybe QuotaPeriod)
serviceQuota_period = Lens.lens (\ServiceQuota' {period} -> period) (\s@ServiceQuota' {} a -> s {period = a} :: ServiceQuota)

-- | The Amazon Resource Name (ARN) of the quota.
serviceQuota_quotaArn :: Lens.Lens' ServiceQuota (Prelude.Maybe Prelude.Text)
serviceQuota_quotaArn = Lens.lens (\ServiceQuota' {quotaArn} -> quotaArn) (\s@ServiceQuota' {} a -> s {quotaArn = a} :: ServiceQuota)

-- | The quota identifier.
serviceQuota_quotaCode :: Lens.Lens' ServiceQuota (Prelude.Maybe Prelude.Text)
serviceQuota_quotaCode = Lens.lens (\ServiceQuota' {quotaCode} -> quotaCode) (\s@ServiceQuota' {} a -> s {quotaCode = a} :: ServiceQuota)

-- | The quota name.
serviceQuota_quotaName :: Lens.Lens' ServiceQuota (Prelude.Maybe Prelude.Text)
serviceQuota_quotaName = Lens.lens (\ServiceQuota' {quotaName} -> quotaName) (\s@ServiceQuota' {} a -> s {quotaName = a} :: ServiceQuota)

-- | The service identifier.
serviceQuota_serviceCode :: Lens.Lens' ServiceQuota (Prelude.Maybe Prelude.Text)
serviceQuota_serviceCode = Lens.lens (\ServiceQuota' {serviceCode} -> serviceCode) (\s@ServiceQuota' {} a -> s {serviceCode = a} :: ServiceQuota)

-- | The service name.
serviceQuota_serviceName :: Lens.Lens' ServiceQuota (Prelude.Maybe Prelude.Text)
serviceQuota_serviceName = Lens.lens (\ServiceQuota' {serviceName} -> serviceName) (\s@ServiceQuota' {} a -> s {serviceName = a} :: ServiceQuota)

-- | The unit of measurement.
serviceQuota_unit :: Lens.Lens' ServiceQuota (Prelude.Maybe Prelude.Text)
serviceQuota_unit = Lens.lens (\ServiceQuota' {unit} -> unit) (\s@ServiceQuota' {} a -> s {unit = a} :: ServiceQuota)

-- | Information about the measurement.
serviceQuota_usageMetric :: Lens.Lens' ServiceQuota (Prelude.Maybe MetricInfo)
serviceQuota_usageMetric = Lens.lens (\ServiceQuota' {usageMetric} -> usageMetric) (\s@ServiceQuota' {} a -> s {usageMetric = a} :: ServiceQuota)

-- | The quota value.
serviceQuota_value :: Lens.Lens' ServiceQuota (Prelude.Maybe Prelude.Double)
serviceQuota_value = Lens.lens (\ServiceQuota' {value} -> value) (\s@ServiceQuota' {} a -> s {value = a} :: ServiceQuota)

instance Data.FromJSON ServiceQuota where
  parseJSON =
    Data.withObject
      "ServiceQuota"
      ( \x ->
          ServiceQuota'
            Prelude.<$> (x Data..:? "Adjustable")
            Prelude.<*> (x Data..:? "ErrorReason")
            Prelude.<*> (x Data..:? "GlobalQuota")
            Prelude.<*> (x Data..:? "Period")
            Prelude.<*> (x Data..:? "QuotaArn")
            Prelude.<*> (x Data..:? "QuotaCode")
            Prelude.<*> (x Data..:? "QuotaName")
            Prelude.<*> (x Data..:? "ServiceCode")
            Prelude.<*> (x Data..:? "ServiceName")
            Prelude.<*> (x Data..:? "Unit")
            Prelude.<*> (x Data..:? "UsageMetric")
            Prelude.<*> (x Data..:? "Value")
      )

instance Prelude.Hashable ServiceQuota where
  hashWithSalt _salt ServiceQuota' {..} =
    _salt `Prelude.hashWithSalt` adjustable
      `Prelude.hashWithSalt` errorReason
      `Prelude.hashWithSalt` globalQuota
      `Prelude.hashWithSalt` period
      `Prelude.hashWithSalt` quotaArn
      `Prelude.hashWithSalt` quotaCode
      `Prelude.hashWithSalt` quotaName
      `Prelude.hashWithSalt` serviceCode
      `Prelude.hashWithSalt` serviceName
      `Prelude.hashWithSalt` unit
      `Prelude.hashWithSalt` usageMetric
      `Prelude.hashWithSalt` value

instance Prelude.NFData ServiceQuota where
  rnf ServiceQuota' {..} =
    Prelude.rnf adjustable
      `Prelude.seq` Prelude.rnf errorReason
      `Prelude.seq` Prelude.rnf globalQuota
      `Prelude.seq` Prelude.rnf period
      `Prelude.seq` Prelude.rnf quotaArn
      `Prelude.seq` Prelude.rnf quotaCode
      `Prelude.seq` Prelude.rnf quotaName
      `Prelude.seq` Prelude.rnf serviceCode
      `Prelude.seq` Prelude.rnf serviceName
      `Prelude.seq` Prelude.rnf unit
      `Prelude.seq` Prelude.rnf usageMetric
      `Prelude.seq` Prelude.rnf value
