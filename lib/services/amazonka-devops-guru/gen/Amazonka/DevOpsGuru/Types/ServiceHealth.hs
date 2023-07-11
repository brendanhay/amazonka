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
-- Module      : Amazonka.DevOpsGuru.Types.ServiceHealth
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.ServiceHealth where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DevOpsGuru.Types.ServiceInsightHealth
import Amazonka.DevOpsGuru.Types.ServiceName
import qualified Amazonka.Prelude as Prelude

-- | Represents the health of an Amazon Web Services service.
--
-- /See:/ 'newServiceHealth' smart constructor.
data ServiceHealth = ServiceHealth'
  { -- | Number of resources that DevOps Guru is monitoring in an analyzed Amazon
    -- Web Services service.
    analyzedResourceCount :: Prelude.Maybe Prelude.Integer,
    -- | Represents the health of an Amazon Web Services service. This is a
    -- @ServiceInsightHealth@ that contains the number of open proactive and
    -- reactive insights for this service.
    insight :: Prelude.Maybe ServiceInsightHealth,
    -- | The name of the Amazon Web Services service.
    serviceName :: Prelude.Maybe ServiceName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceHealth' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'analyzedResourceCount', 'serviceHealth_analyzedResourceCount' - Number of resources that DevOps Guru is monitoring in an analyzed Amazon
-- Web Services service.
--
-- 'insight', 'serviceHealth_insight' - Represents the health of an Amazon Web Services service. This is a
-- @ServiceInsightHealth@ that contains the number of open proactive and
-- reactive insights for this service.
--
-- 'serviceName', 'serviceHealth_serviceName' - The name of the Amazon Web Services service.
newServiceHealth ::
  ServiceHealth
newServiceHealth =
  ServiceHealth'
    { analyzedResourceCount =
        Prelude.Nothing,
      insight = Prelude.Nothing,
      serviceName = Prelude.Nothing
    }

-- | Number of resources that DevOps Guru is monitoring in an analyzed Amazon
-- Web Services service.
serviceHealth_analyzedResourceCount :: Lens.Lens' ServiceHealth (Prelude.Maybe Prelude.Integer)
serviceHealth_analyzedResourceCount = Lens.lens (\ServiceHealth' {analyzedResourceCount} -> analyzedResourceCount) (\s@ServiceHealth' {} a -> s {analyzedResourceCount = a} :: ServiceHealth)

-- | Represents the health of an Amazon Web Services service. This is a
-- @ServiceInsightHealth@ that contains the number of open proactive and
-- reactive insights for this service.
serviceHealth_insight :: Lens.Lens' ServiceHealth (Prelude.Maybe ServiceInsightHealth)
serviceHealth_insight = Lens.lens (\ServiceHealth' {insight} -> insight) (\s@ServiceHealth' {} a -> s {insight = a} :: ServiceHealth)

-- | The name of the Amazon Web Services service.
serviceHealth_serviceName :: Lens.Lens' ServiceHealth (Prelude.Maybe ServiceName)
serviceHealth_serviceName = Lens.lens (\ServiceHealth' {serviceName} -> serviceName) (\s@ServiceHealth' {} a -> s {serviceName = a} :: ServiceHealth)

instance Data.FromJSON ServiceHealth where
  parseJSON =
    Data.withObject
      "ServiceHealth"
      ( \x ->
          ServiceHealth'
            Prelude.<$> (x Data..:? "AnalyzedResourceCount")
            Prelude.<*> (x Data..:? "Insight")
            Prelude.<*> (x Data..:? "ServiceName")
      )

instance Prelude.Hashable ServiceHealth where
  hashWithSalt _salt ServiceHealth' {..} =
    _salt
      `Prelude.hashWithSalt` analyzedResourceCount
      `Prelude.hashWithSalt` insight
      `Prelude.hashWithSalt` serviceName

instance Prelude.NFData ServiceHealth where
  rnf ServiceHealth' {..} =
    Prelude.rnf analyzedResourceCount
      `Prelude.seq` Prelude.rnf insight
      `Prelude.seq` Prelude.rnf serviceName
