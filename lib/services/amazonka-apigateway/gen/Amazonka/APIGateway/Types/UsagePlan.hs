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
-- Module      : Amazonka.APIGateway.Types.UsagePlan
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.APIGateway.Types.UsagePlan where

import Amazonka.APIGateway.Types.ApiStage
import Amazonka.APIGateway.Types.QuotaSettings
import Amazonka.APIGateway.Types.ThrottleSettings
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents a usage plan used to specify who can assess associated API
-- stages. Optionally, target request rate and quota limits can be set. In
-- some cases clients can exceed the targets that you set. Don’t rely on
-- usage plans to control costs. Consider using
-- <https://docs.aws.amazon.com/cost-management/latest/userguide/budgets-managing-costs.html Amazon Web Services Budgets>
-- to monitor costs and
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html WAF>
-- to manage API requests.
--
-- /See:/ 'newUsagePlan' smart constructor.
data UsagePlan = UsagePlan'
  { -- | The associated API stages of a usage plan.
    apiStages :: Prelude.Maybe [ApiStage],
    -- | The description of a usage plan.
    description :: Prelude.Maybe Prelude.Text,
    -- | The identifier of a UsagePlan resource.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of a usage plan.
    name :: Prelude.Maybe Prelude.Text,
    -- | The AWS Markeplace product identifier to associate with the usage plan
    -- as a SaaS product on AWS Marketplace.
    productCode :: Prelude.Maybe Prelude.Text,
    -- | The target maximum number of permitted requests per a given unit time
    -- interval.
    quota :: Prelude.Maybe QuotaSettings,
    -- | The collection of tags. Each tag element is associated with a given
    -- resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A map containing method level throttling information for API stage in a
    -- usage plan.
    throttle :: Prelude.Maybe ThrottleSettings
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
-- 'description', 'usagePlan_description' - The description of a usage plan.
--
-- 'id', 'usagePlan_id' - The identifier of a UsagePlan resource.
--
-- 'name', 'usagePlan_name' - The name of a usage plan.
--
-- 'productCode', 'usagePlan_productCode' - The AWS Markeplace product identifier to associate with the usage plan
-- as a SaaS product on AWS Marketplace.
--
-- 'quota', 'usagePlan_quota' - The target maximum number of permitted requests per a given unit time
-- interval.
--
-- 'tags', 'usagePlan_tags' - The collection of tags. Each tag element is associated with a given
-- resource.
--
-- 'throttle', 'usagePlan_throttle' - A map containing method level throttling information for API stage in a
-- usage plan.
newUsagePlan ::
  UsagePlan
newUsagePlan =
  UsagePlan'
    { apiStages = Prelude.Nothing,
      description = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      productCode = Prelude.Nothing,
      quota = Prelude.Nothing,
      tags = Prelude.Nothing,
      throttle = Prelude.Nothing
    }

-- | The associated API stages of a usage plan.
usagePlan_apiStages :: Lens.Lens' UsagePlan (Prelude.Maybe [ApiStage])
usagePlan_apiStages = Lens.lens (\UsagePlan' {apiStages} -> apiStages) (\s@UsagePlan' {} a -> s {apiStages = a} :: UsagePlan) Prelude.. Lens.mapping Lens.coerced

-- | The description of a usage plan.
usagePlan_description :: Lens.Lens' UsagePlan (Prelude.Maybe Prelude.Text)
usagePlan_description = Lens.lens (\UsagePlan' {description} -> description) (\s@UsagePlan' {} a -> s {description = a} :: UsagePlan)

-- | The identifier of a UsagePlan resource.
usagePlan_id :: Lens.Lens' UsagePlan (Prelude.Maybe Prelude.Text)
usagePlan_id = Lens.lens (\UsagePlan' {id} -> id) (\s@UsagePlan' {} a -> s {id = a} :: UsagePlan)

-- | The name of a usage plan.
usagePlan_name :: Lens.Lens' UsagePlan (Prelude.Maybe Prelude.Text)
usagePlan_name = Lens.lens (\UsagePlan' {name} -> name) (\s@UsagePlan' {} a -> s {name = a} :: UsagePlan)

-- | The AWS Markeplace product identifier to associate with the usage plan
-- as a SaaS product on AWS Marketplace.
usagePlan_productCode :: Lens.Lens' UsagePlan (Prelude.Maybe Prelude.Text)
usagePlan_productCode = Lens.lens (\UsagePlan' {productCode} -> productCode) (\s@UsagePlan' {} a -> s {productCode = a} :: UsagePlan)

-- | The target maximum number of permitted requests per a given unit time
-- interval.
usagePlan_quota :: Lens.Lens' UsagePlan (Prelude.Maybe QuotaSettings)
usagePlan_quota = Lens.lens (\UsagePlan' {quota} -> quota) (\s@UsagePlan' {} a -> s {quota = a} :: UsagePlan)

-- | The collection of tags. Each tag element is associated with a given
-- resource.
usagePlan_tags :: Lens.Lens' UsagePlan (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
usagePlan_tags = Lens.lens (\UsagePlan' {tags} -> tags) (\s@UsagePlan' {} a -> s {tags = a} :: UsagePlan) Prelude.. Lens.mapping Lens.coerced

-- | A map containing method level throttling information for API stage in a
-- usage plan.
usagePlan_throttle :: Lens.Lens' UsagePlan (Prelude.Maybe ThrottleSettings)
usagePlan_throttle = Lens.lens (\UsagePlan' {throttle} -> throttle) (\s@UsagePlan' {} a -> s {throttle = a} :: UsagePlan)

instance Data.FromJSON UsagePlan where
  parseJSON =
    Data.withObject
      "UsagePlan"
      ( \x ->
          UsagePlan'
            Prelude.<$> (x Data..:? "apiStages" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "productCode")
            Prelude.<*> (x Data..:? "quota")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "throttle")
      )

instance Prelude.Hashable UsagePlan where
  hashWithSalt _salt UsagePlan' {..} =
    _salt
      `Prelude.hashWithSalt` apiStages
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` productCode
      `Prelude.hashWithSalt` quota
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` throttle

instance Prelude.NFData UsagePlan where
  rnf UsagePlan' {..} =
    Prelude.rnf apiStages
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf productCode
      `Prelude.seq` Prelude.rnf quota
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf throttle
