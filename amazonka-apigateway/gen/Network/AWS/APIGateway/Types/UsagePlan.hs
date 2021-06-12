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
-- Module      : Network.AWS.APIGateway.Types.UsagePlan
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.UsagePlan where

import Network.AWS.APIGateway.Types.ApiStage
import Network.AWS.APIGateway.Types.QuotaSettings
import Network.AWS.APIGateway.Types.ThrottleSettings
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents a usage plan than can specify who can assess associated API
-- stages with specified request limits and quotas.
--
-- In a usage plan, you associate an API by specifying the API\'s Id and a
-- stage name of the specified API. You add plan customers by adding API
-- keys to the plan.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-api-usage-plans.html Create and Use Usage Plans>
--
-- /See:/ 'newUsagePlan' smart constructor.
data UsagePlan = UsagePlan'
  { -- | The identifier of a UsagePlan resource.
    id :: Core.Maybe Core.Text,
    -- | The name of a usage plan.
    name :: Core.Maybe Core.Text,
    -- | The associated API stages of a usage plan.
    apiStages :: Core.Maybe [ApiStage],
    -- | The collection of tags. Each tag element is associated with a given
    -- resource.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The description of a usage plan.
    description :: Core.Maybe Core.Text,
    -- | The maximum number of permitted requests per a given unit time interval.
    quota :: Core.Maybe QuotaSettings,
    -- | The AWS Markeplace product identifier to associate with the usage plan
    -- as a SaaS product on AWS Marketplace.
    productCode :: Core.Maybe Core.Text,
    -- | The request throttle limits of a usage plan.
    throttle :: Core.Maybe ThrottleSettings
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UsagePlan' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'usagePlan_id' - The identifier of a UsagePlan resource.
--
-- 'name', 'usagePlan_name' - The name of a usage plan.
--
-- 'apiStages', 'usagePlan_apiStages' - The associated API stages of a usage plan.
--
-- 'tags', 'usagePlan_tags' - The collection of tags. Each tag element is associated with a given
-- resource.
--
-- 'description', 'usagePlan_description' - The description of a usage plan.
--
-- 'quota', 'usagePlan_quota' - The maximum number of permitted requests per a given unit time interval.
--
-- 'productCode', 'usagePlan_productCode' - The AWS Markeplace product identifier to associate with the usage plan
-- as a SaaS product on AWS Marketplace.
--
-- 'throttle', 'usagePlan_throttle' - The request throttle limits of a usage plan.
newUsagePlan ::
  UsagePlan
newUsagePlan =
  UsagePlan'
    { id = Core.Nothing,
      name = Core.Nothing,
      apiStages = Core.Nothing,
      tags = Core.Nothing,
      description = Core.Nothing,
      quota = Core.Nothing,
      productCode = Core.Nothing,
      throttle = Core.Nothing
    }

-- | The identifier of a UsagePlan resource.
usagePlan_id :: Lens.Lens' UsagePlan (Core.Maybe Core.Text)
usagePlan_id = Lens.lens (\UsagePlan' {id} -> id) (\s@UsagePlan' {} a -> s {id = a} :: UsagePlan)

-- | The name of a usage plan.
usagePlan_name :: Lens.Lens' UsagePlan (Core.Maybe Core.Text)
usagePlan_name = Lens.lens (\UsagePlan' {name} -> name) (\s@UsagePlan' {} a -> s {name = a} :: UsagePlan)

-- | The associated API stages of a usage plan.
usagePlan_apiStages :: Lens.Lens' UsagePlan (Core.Maybe [ApiStage])
usagePlan_apiStages = Lens.lens (\UsagePlan' {apiStages} -> apiStages) (\s@UsagePlan' {} a -> s {apiStages = a} :: UsagePlan) Core.. Lens.mapping Lens._Coerce

-- | The collection of tags. Each tag element is associated with a given
-- resource.
usagePlan_tags :: Lens.Lens' UsagePlan (Core.Maybe (Core.HashMap Core.Text Core.Text))
usagePlan_tags = Lens.lens (\UsagePlan' {tags} -> tags) (\s@UsagePlan' {} a -> s {tags = a} :: UsagePlan) Core.. Lens.mapping Lens._Coerce

-- | The description of a usage plan.
usagePlan_description :: Lens.Lens' UsagePlan (Core.Maybe Core.Text)
usagePlan_description = Lens.lens (\UsagePlan' {description} -> description) (\s@UsagePlan' {} a -> s {description = a} :: UsagePlan)

-- | The maximum number of permitted requests per a given unit time interval.
usagePlan_quota :: Lens.Lens' UsagePlan (Core.Maybe QuotaSettings)
usagePlan_quota = Lens.lens (\UsagePlan' {quota} -> quota) (\s@UsagePlan' {} a -> s {quota = a} :: UsagePlan)

-- | The AWS Markeplace product identifier to associate with the usage plan
-- as a SaaS product on AWS Marketplace.
usagePlan_productCode :: Lens.Lens' UsagePlan (Core.Maybe Core.Text)
usagePlan_productCode = Lens.lens (\UsagePlan' {productCode} -> productCode) (\s@UsagePlan' {} a -> s {productCode = a} :: UsagePlan)

-- | The request throttle limits of a usage plan.
usagePlan_throttle :: Lens.Lens' UsagePlan (Core.Maybe ThrottleSettings)
usagePlan_throttle = Lens.lens (\UsagePlan' {throttle} -> throttle) (\s@UsagePlan' {} a -> s {throttle = a} :: UsagePlan)

instance Core.FromJSON UsagePlan where
  parseJSON =
    Core.withObject
      "UsagePlan"
      ( \x ->
          UsagePlan'
            Core.<$> (x Core..:? "id")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "apiStages" Core..!= Core.mempty)
            Core.<*> (x Core..:? "tags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "description")
            Core.<*> (x Core..:? "quota")
            Core.<*> (x Core..:? "productCode")
            Core.<*> (x Core..:? "throttle")
      )

instance Core.Hashable UsagePlan

instance Core.NFData UsagePlan
