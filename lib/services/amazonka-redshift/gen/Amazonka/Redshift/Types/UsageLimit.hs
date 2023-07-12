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
-- Module      : Amazonka.Redshift.Types.UsageLimit
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.UsageLimit where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal
import Amazonka.Redshift.Types.Tag
import Amazonka.Redshift.Types.UsageLimitBreachAction
import Amazonka.Redshift.Types.UsageLimitFeatureType
import Amazonka.Redshift.Types.UsageLimitLimitType
import Amazonka.Redshift.Types.UsageLimitPeriod

-- | Describes a usage limit object for a cluster.
--
-- /See:/ 'newUsageLimit' smart constructor.
data UsageLimit = UsageLimit'
  { -- | The limit amount. If time-based, this amount is in minutes. If
    -- data-based, this amount is in terabytes (TB).
    amount :: Prelude.Maybe Prelude.Integer,
    -- | The action that Amazon Redshift takes when the limit is reached.
    -- Possible values are:
    --
    -- -   __log__ - To log an event in a system table. The default is log.
    --
    -- -   __emit-metric__ - To emit CloudWatch metrics.
    --
    -- -   __disable__ - To disable the feature until the next usage period
    --     begins.
    breachAction :: Prelude.Maybe UsageLimitBreachAction,
    -- | The identifier of the cluster with a usage limit.
    clusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Redshift feature to which the limit applies.
    featureType :: Prelude.Maybe UsageLimitFeatureType,
    -- | The type of limit. Depending on the feature type, this can be based on a
    -- time duration or data size.
    limitType :: Prelude.Maybe UsageLimitLimitType,
    -- | The time period that the amount applies to. A @weekly@ period begins on
    -- Sunday. The default is @monthly@.
    period :: Prelude.Maybe UsageLimitPeriod,
    -- | A list of tag instances.
    tags :: Prelude.Maybe [Tag],
    -- | The identifier of the usage limit.
    usageLimitId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UsageLimit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'amount', 'usageLimit_amount' - The limit amount. If time-based, this amount is in minutes. If
-- data-based, this amount is in terabytes (TB).
--
-- 'breachAction', 'usageLimit_breachAction' - The action that Amazon Redshift takes when the limit is reached.
-- Possible values are:
--
-- -   __log__ - To log an event in a system table. The default is log.
--
-- -   __emit-metric__ - To emit CloudWatch metrics.
--
-- -   __disable__ - To disable the feature until the next usage period
--     begins.
--
-- 'clusterIdentifier', 'usageLimit_clusterIdentifier' - The identifier of the cluster with a usage limit.
--
-- 'featureType', 'usageLimit_featureType' - The Amazon Redshift feature to which the limit applies.
--
-- 'limitType', 'usageLimit_limitType' - The type of limit. Depending on the feature type, this can be based on a
-- time duration or data size.
--
-- 'period', 'usageLimit_period' - The time period that the amount applies to. A @weekly@ period begins on
-- Sunday. The default is @monthly@.
--
-- 'tags', 'usageLimit_tags' - A list of tag instances.
--
-- 'usageLimitId', 'usageLimit_usageLimitId' - The identifier of the usage limit.
newUsageLimit ::
  UsageLimit
newUsageLimit =
  UsageLimit'
    { amount = Prelude.Nothing,
      breachAction = Prelude.Nothing,
      clusterIdentifier = Prelude.Nothing,
      featureType = Prelude.Nothing,
      limitType = Prelude.Nothing,
      period = Prelude.Nothing,
      tags = Prelude.Nothing,
      usageLimitId = Prelude.Nothing
    }

-- | The limit amount. If time-based, this amount is in minutes. If
-- data-based, this amount is in terabytes (TB).
usageLimit_amount :: Lens.Lens' UsageLimit (Prelude.Maybe Prelude.Integer)
usageLimit_amount = Lens.lens (\UsageLimit' {amount} -> amount) (\s@UsageLimit' {} a -> s {amount = a} :: UsageLimit)

-- | The action that Amazon Redshift takes when the limit is reached.
-- Possible values are:
--
-- -   __log__ - To log an event in a system table. The default is log.
--
-- -   __emit-metric__ - To emit CloudWatch metrics.
--
-- -   __disable__ - To disable the feature until the next usage period
--     begins.
usageLimit_breachAction :: Lens.Lens' UsageLimit (Prelude.Maybe UsageLimitBreachAction)
usageLimit_breachAction = Lens.lens (\UsageLimit' {breachAction} -> breachAction) (\s@UsageLimit' {} a -> s {breachAction = a} :: UsageLimit)

-- | The identifier of the cluster with a usage limit.
usageLimit_clusterIdentifier :: Lens.Lens' UsageLimit (Prelude.Maybe Prelude.Text)
usageLimit_clusterIdentifier = Lens.lens (\UsageLimit' {clusterIdentifier} -> clusterIdentifier) (\s@UsageLimit' {} a -> s {clusterIdentifier = a} :: UsageLimit)

-- | The Amazon Redshift feature to which the limit applies.
usageLimit_featureType :: Lens.Lens' UsageLimit (Prelude.Maybe UsageLimitFeatureType)
usageLimit_featureType = Lens.lens (\UsageLimit' {featureType} -> featureType) (\s@UsageLimit' {} a -> s {featureType = a} :: UsageLimit)

-- | The type of limit. Depending on the feature type, this can be based on a
-- time duration or data size.
usageLimit_limitType :: Lens.Lens' UsageLimit (Prelude.Maybe UsageLimitLimitType)
usageLimit_limitType = Lens.lens (\UsageLimit' {limitType} -> limitType) (\s@UsageLimit' {} a -> s {limitType = a} :: UsageLimit)

-- | The time period that the amount applies to. A @weekly@ period begins on
-- Sunday. The default is @monthly@.
usageLimit_period :: Lens.Lens' UsageLimit (Prelude.Maybe UsageLimitPeriod)
usageLimit_period = Lens.lens (\UsageLimit' {period} -> period) (\s@UsageLimit' {} a -> s {period = a} :: UsageLimit)

-- | A list of tag instances.
usageLimit_tags :: Lens.Lens' UsageLimit (Prelude.Maybe [Tag])
usageLimit_tags = Lens.lens (\UsageLimit' {tags} -> tags) (\s@UsageLimit' {} a -> s {tags = a} :: UsageLimit) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the usage limit.
usageLimit_usageLimitId :: Lens.Lens' UsageLimit (Prelude.Maybe Prelude.Text)
usageLimit_usageLimitId = Lens.lens (\UsageLimit' {usageLimitId} -> usageLimitId) (\s@UsageLimit' {} a -> s {usageLimitId = a} :: UsageLimit)

instance Data.FromXML UsageLimit where
  parseXML x =
    UsageLimit'
      Prelude.<$> (x Data..@? "Amount")
      Prelude.<*> (x Data..@? "BreachAction")
      Prelude.<*> (x Data..@? "ClusterIdentifier")
      Prelude.<*> (x Data..@? "FeatureType")
      Prelude.<*> (x Data..@? "LimitType")
      Prelude.<*> (x Data..@? "Period")
      Prelude.<*> ( x
                      Data..@? "Tags"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "Tag")
                  )
      Prelude.<*> (x Data..@? "UsageLimitId")

instance Prelude.Hashable UsageLimit where
  hashWithSalt _salt UsageLimit' {..} =
    _salt
      `Prelude.hashWithSalt` amount
      `Prelude.hashWithSalt` breachAction
      `Prelude.hashWithSalt` clusterIdentifier
      `Prelude.hashWithSalt` featureType
      `Prelude.hashWithSalt` limitType
      `Prelude.hashWithSalt` period
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` usageLimitId

instance Prelude.NFData UsageLimit where
  rnf UsageLimit' {..} =
    Prelude.rnf amount
      `Prelude.seq` Prelude.rnf breachAction
      `Prelude.seq` Prelude.rnf clusterIdentifier
      `Prelude.seq` Prelude.rnf featureType
      `Prelude.seq` Prelude.rnf limitType
      `Prelude.seq` Prelude.rnf period
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf usageLimitId
