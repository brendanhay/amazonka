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
-- Module      : Network.AWS.APIGateway.Types.Usage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.Usage where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents the usage data of a usage plan.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-api-usage-plans.html Create and Use Usage Plans>,
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-create-usage-plans-with-console.html#api-gateway-usage-plan-manage-usage Manage Usage in a Usage Plan>
--
-- /See:/ 'newUsage' smart constructor.
data Usage = Usage'
  { -- | The starting date of the usage data.
    startDate :: Core.Maybe Core.Text,
    -- | The usage data, as daily logs of used and remaining quotas, over the
    -- specified time interval indexed over the API keys in a usage plan. For
    -- example,
    -- @{..., \"values\" : { \"{api_key}\" : [ [0, 100], [10, 90], [100, 10]]}@,
    -- where @{api_key}@ stands for an API key value and the daily log entry is
    -- of the format @[used quota, remaining quota]@.
    items :: Core.Maybe (Core.HashMap Core.Text [[Core.Integer]]),
    position :: Core.Maybe Core.Text,
    -- | The plan Id associated with this usage data.
    usagePlanId :: Core.Maybe Core.Text,
    -- | The ending date of the usage data.
    endDate :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Usage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startDate', 'usage_startDate' - The starting date of the usage data.
--
-- 'items', 'usage_items' - The usage data, as daily logs of used and remaining quotas, over the
-- specified time interval indexed over the API keys in a usage plan. For
-- example,
-- @{..., \"values\" : { \"{api_key}\" : [ [0, 100], [10, 90], [100, 10]]}@,
-- where @{api_key}@ stands for an API key value and the daily log entry is
-- of the format @[used quota, remaining quota]@.
--
-- 'position', 'usage_position' - Undocumented member.
--
-- 'usagePlanId', 'usage_usagePlanId' - The plan Id associated with this usage data.
--
-- 'endDate', 'usage_endDate' - The ending date of the usage data.
newUsage ::
  Usage
newUsage =
  Usage'
    { startDate = Core.Nothing,
      items = Core.Nothing,
      position = Core.Nothing,
      usagePlanId = Core.Nothing,
      endDate = Core.Nothing
    }

-- | The starting date of the usage data.
usage_startDate :: Lens.Lens' Usage (Core.Maybe Core.Text)
usage_startDate = Lens.lens (\Usage' {startDate} -> startDate) (\s@Usage' {} a -> s {startDate = a} :: Usage)

-- | The usage data, as daily logs of used and remaining quotas, over the
-- specified time interval indexed over the API keys in a usage plan. For
-- example,
-- @{..., \"values\" : { \"{api_key}\" : [ [0, 100], [10, 90], [100, 10]]}@,
-- where @{api_key}@ stands for an API key value and the daily log entry is
-- of the format @[used quota, remaining quota]@.
usage_items :: Lens.Lens' Usage (Core.Maybe (Core.HashMap Core.Text [[Core.Integer]]))
usage_items = Lens.lens (\Usage' {items} -> items) (\s@Usage' {} a -> s {items = a} :: Usage) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
usage_position :: Lens.Lens' Usage (Core.Maybe Core.Text)
usage_position = Lens.lens (\Usage' {position} -> position) (\s@Usage' {} a -> s {position = a} :: Usage)

-- | The plan Id associated with this usage data.
usage_usagePlanId :: Lens.Lens' Usage (Core.Maybe Core.Text)
usage_usagePlanId = Lens.lens (\Usage' {usagePlanId} -> usagePlanId) (\s@Usage' {} a -> s {usagePlanId = a} :: Usage)

-- | The ending date of the usage data.
usage_endDate :: Lens.Lens' Usage (Core.Maybe Core.Text)
usage_endDate = Lens.lens (\Usage' {endDate} -> endDate) (\s@Usage' {} a -> s {endDate = a} :: Usage)

instance Core.FromJSON Usage where
  parseJSON =
    Core.withObject
      "Usage"
      ( \x ->
          Usage'
            Core.<$> (x Core..:? "startDate")
            Core.<*> (x Core..:? "values" Core..!= Core.mempty)
            Core.<*> (x Core..:? "position")
            Core.<*> (x Core..:? "usagePlanId")
            Core.<*> (x Core..:? "endDate")
      )

instance Core.Hashable Usage

instance Core.NFData Usage
