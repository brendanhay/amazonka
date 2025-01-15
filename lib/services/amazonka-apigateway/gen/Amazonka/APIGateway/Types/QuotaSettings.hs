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
-- Module      : Amazonka.APIGateway.Types.QuotaSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.APIGateway.Types.QuotaSettings where

import Amazonka.APIGateway.Types.QuotaPeriodType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Quotas configured for a usage plan.
--
-- /See:/ 'newQuotaSettings' smart constructor.
data QuotaSettings = QuotaSettings'
  { -- | The target maximum number of requests that can be made in a given time
    -- period.
    limit :: Prelude.Maybe Prelude.Int,
    -- | The number of requests subtracted from the given limit in the initial
    -- time period.
    offset :: Prelude.Maybe Prelude.Int,
    -- | The time period in which the limit applies. Valid values are \"DAY\",
    -- \"WEEK\" or \"MONTH\".
    period :: Prelude.Maybe QuotaPeriodType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QuotaSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'quotaSettings_limit' - The target maximum number of requests that can be made in a given time
-- period.
--
-- 'offset', 'quotaSettings_offset' - The number of requests subtracted from the given limit in the initial
-- time period.
--
-- 'period', 'quotaSettings_period' - The time period in which the limit applies. Valid values are \"DAY\",
-- \"WEEK\" or \"MONTH\".
newQuotaSettings ::
  QuotaSettings
newQuotaSettings =
  QuotaSettings'
    { limit = Prelude.Nothing,
      offset = Prelude.Nothing,
      period = Prelude.Nothing
    }

-- | The target maximum number of requests that can be made in a given time
-- period.
quotaSettings_limit :: Lens.Lens' QuotaSettings (Prelude.Maybe Prelude.Int)
quotaSettings_limit = Lens.lens (\QuotaSettings' {limit} -> limit) (\s@QuotaSettings' {} a -> s {limit = a} :: QuotaSettings)

-- | The number of requests subtracted from the given limit in the initial
-- time period.
quotaSettings_offset :: Lens.Lens' QuotaSettings (Prelude.Maybe Prelude.Int)
quotaSettings_offset = Lens.lens (\QuotaSettings' {offset} -> offset) (\s@QuotaSettings' {} a -> s {offset = a} :: QuotaSettings)

-- | The time period in which the limit applies. Valid values are \"DAY\",
-- \"WEEK\" or \"MONTH\".
quotaSettings_period :: Lens.Lens' QuotaSettings (Prelude.Maybe QuotaPeriodType)
quotaSettings_period = Lens.lens (\QuotaSettings' {period} -> period) (\s@QuotaSettings' {} a -> s {period = a} :: QuotaSettings)

instance Data.FromJSON QuotaSettings where
  parseJSON =
    Data.withObject
      "QuotaSettings"
      ( \x ->
          QuotaSettings'
            Prelude.<$> (x Data..:? "limit")
            Prelude.<*> (x Data..:? "offset")
            Prelude.<*> (x Data..:? "period")
      )

instance Prelude.Hashable QuotaSettings where
  hashWithSalt _salt QuotaSettings' {..} =
    _salt
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` offset
      `Prelude.hashWithSalt` period

instance Prelude.NFData QuotaSettings where
  rnf QuotaSettings' {..} =
    Prelude.rnf limit `Prelude.seq`
      Prelude.rnf offset `Prelude.seq`
        Prelude.rnf period

instance Data.ToJSON QuotaSettings where
  toJSON QuotaSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("limit" Data..=) Prelude.<$> limit,
            ("offset" Data..=) Prelude.<$> offset,
            ("period" Data..=) Prelude.<$> period
          ]
      )
