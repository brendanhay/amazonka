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
import qualified Network.AWS.Prelude as Prelude

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
  { -- | The associated API stages of a usage plan.
    apiStages :: Prelude.Maybe [ApiStage],
    -- | The name of a usage plan.
    name :: Prelude.Maybe Prelude.Text,
    -- | The identifier of a UsagePlan resource.
    id :: Prelude.Maybe Prelude.Text,
    -- | The request throttle limits of a usage plan.
    throttle :: Prelude.Maybe ThrottleSettings,
    -- | The maximum number of permitted requests per a given unit time interval.
    quota :: Prelude.Maybe QuotaSettings,
    -- | The description of a usage plan.
    description :: Prelude.Maybe Prelude.Text,
    -- | The AWS Markeplace product identifier to associate with the usage plan
    -- as a SaaS product on AWS Marketplace.
    productCode :: Prelude.Maybe Prelude.Text,
    -- | The collection of tags. Each tag element is associated with a given
    -- resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UsagePlan' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiStages', 'usagePlan_apiStages' - The associated API stages of a usage plan.
--
-- 'name', 'usagePlan_name' - The name of a usage plan.
--
-- 'id', 'usagePlan_id' - The identifier of a UsagePlan resource.
--
-- 'throttle', 'usagePlan_throttle' - The request throttle limits of a usage plan.
--
-- 'quota', 'usagePlan_quota' - The maximum number of permitted requests per a given unit time interval.
--
-- 'description', 'usagePlan_description' - The description of a usage plan.
--
-- 'productCode', 'usagePlan_productCode' - The AWS Markeplace product identifier to associate with the usage plan
-- as a SaaS product on AWS Marketplace.
--
-- 'tags', 'usagePlan_tags' - The collection of tags. Each tag element is associated with a given
-- resource.
newUsagePlan ::
  UsagePlan
newUsagePlan =
  UsagePlan'
    { apiStages = Prelude.Nothing,
      name = Prelude.Nothing,
      id = Prelude.Nothing,
      throttle = Prelude.Nothing,
      quota = Prelude.Nothing,
      description = Prelude.Nothing,
      productCode = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The associated API stages of a usage plan.
usagePlan_apiStages :: Lens.Lens' UsagePlan (Prelude.Maybe [ApiStage])
usagePlan_apiStages = Lens.lens (\UsagePlan' {apiStages} -> apiStages) (\s@UsagePlan' {} a -> s {apiStages = a} :: UsagePlan) Prelude.. Lens.mapping Lens.coerced

-- | The name of a usage plan.
usagePlan_name :: Lens.Lens' UsagePlan (Prelude.Maybe Prelude.Text)
usagePlan_name = Lens.lens (\UsagePlan' {name} -> name) (\s@UsagePlan' {} a -> s {name = a} :: UsagePlan)

-- | The identifier of a UsagePlan resource.
usagePlan_id :: Lens.Lens' UsagePlan (Prelude.Maybe Prelude.Text)
usagePlan_id = Lens.lens (\UsagePlan' {id} -> id) (\s@UsagePlan' {} a -> s {id = a} :: UsagePlan)

-- | The request throttle limits of a usage plan.
usagePlan_throttle :: Lens.Lens' UsagePlan (Prelude.Maybe ThrottleSettings)
usagePlan_throttle = Lens.lens (\UsagePlan' {throttle} -> throttle) (\s@UsagePlan' {} a -> s {throttle = a} :: UsagePlan)

-- | The maximum number of permitted requests per a given unit time interval.
usagePlan_quota :: Lens.Lens' UsagePlan (Prelude.Maybe QuotaSettings)
usagePlan_quota = Lens.lens (\UsagePlan' {quota} -> quota) (\s@UsagePlan' {} a -> s {quota = a} :: UsagePlan)

-- | The description of a usage plan.
usagePlan_description :: Lens.Lens' UsagePlan (Prelude.Maybe Prelude.Text)
usagePlan_description = Lens.lens (\UsagePlan' {description} -> description) (\s@UsagePlan' {} a -> s {description = a} :: UsagePlan)

-- | The AWS Markeplace product identifier to associate with the usage plan
-- as a SaaS product on AWS Marketplace.
usagePlan_productCode :: Lens.Lens' UsagePlan (Prelude.Maybe Prelude.Text)
usagePlan_productCode = Lens.lens (\UsagePlan' {productCode} -> productCode) (\s@UsagePlan' {} a -> s {productCode = a} :: UsagePlan)

-- | The collection of tags. Each tag element is associated with a given
-- resource.
usagePlan_tags :: Lens.Lens' UsagePlan (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
usagePlan_tags = Lens.lens (\UsagePlan' {tags} -> tags) (\s@UsagePlan' {} a -> s {tags = a} :: UsagePlan) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON UsagePlan where
  parseJSON =
    Core.withObject
      "UsagePlan"
      ( \x ->
          UsagePlan'
            Prelude.<$> (x Core..:? "apiStages" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "throttle")
            Prelude.<*> (x Core..:? "quota")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "productCode")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable UsagePlan

instance Prelude.NFData UsagePlan
