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
-- Module      : Network.AWS.APIGateway.Types.QuotaSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.QuotaSettings where

import Network.AWS.APIGateway.Types.QuotaPeriodType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Quotas configured for a usage plan.
--
-- /See:/ 'newQuotaSettings' smart constructor.
data QuotaSettings = QuotaSettings'
  { -- | The time period in which the limit applies. Valid values are \"DAY\",
    -- \"WEEK\" or \"MONTH\".
    period :: Core.Maybe QuotaPeriodType,
    -- | The maximum number of requests that can be made in a given time period.
    limit :: Core.Maybe Core.Int,
    -- | The day that a time period starts. For example, with a time period of
    -- @WEEK@, an offset of @0@ starts on Sunday, and an offset of @1@ starts
    -- on Monday.
    offset :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'QuotaSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'period', 'quotaSettings_period' - The time period in which the limit applies. Valid values are \"DAY\",
-- \"WEEK\" or \"MONTH\".
--
-- 'limit', 'quotaSettings_limit' - The maximum number of requests that can be made in a given time period.
--
-- 'offset', 'quotaSettings_offset' - The day that a time period starts. For example, with a time period of
-- @WEEK@, an offset of @0@ starts on Sunday, and an offset of @1@ starts
-- on Monday.
newQuotaSettings ::
  QuotaSettings
newQuotaSettings =
  QuotaSettings'
    { period = Core.Nothing,
      limit = Core.Nothing,
      offset = Core.Nothing
    }

-- | The time period in which the limit applies. Valid values are \"DAY\",
-- \"WEEK\" or \"MONTH\".
quotaSettings_period :: Lens.Lens' QuotaSettings (Core.Maybe QuotaPeriodType)
quotaSettings_period = Lens.lens (\QuotaSettings' {period} -> period) (\s@QuotaSettings' {} a -> s {period = a} :: QuotaSettings)

-- | The maximum number of requests that can be made in a given time period.
quotaSettings_limit :: Lens.Lens' QuotaSettings (Core.Maybe Core.Int)
quotaSettings_limit = Lens.lens (\QuotaSettings' {limit} -> limit) (\s@QuotaSettings' {} a -> s {limit = a} :: QuotaSettings)

-- | The day that a time period starts. For example, with a time period of
-- @WEEK@, an offset of @0@ starts on Sunday, and an offset of @1@ starts
-- on Monday.
quotaSettings_offset :: Lens.Lens' QuotaSettings (Core.Maybe Core.Int)
quotaSettings_offset = Lens.lens (\QuotaSettings' {offset} -> offset) (\s@QuotaSettings' {} a -> s {offset = a} :: QuotaSettings)

instance Core.FromJSON QuotaSettings where
  parseJSON =
    Core.withObject
      "QuotaSettings"
      ( \x ->
          QuotaSettings'
            Core.<$> (x Core..:? "period")
            Core.<*> (x Core..:? "limit")
            Core.<*> (x Core..:? "offset")
      )

instance Core.Hashable QuotaSettings

instance Core.NFData QuotaSettings

instance Core.ToJSON QuotaSettings where
  toJSON QuotaSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("period" Core..=) Core.<$> period,
            ("limit" Core..=) Core.<$> limit,
            ("offset" Core..=) Core.<$> offset
          ]
      )
