{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Redshift.Types.UsageLimit
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.UsageLimit where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.Tag
import Network.AWS.Redshift.Types.UsageLimitBreachAction
import Network.AWS.Redshift.Types.UsageLimitFeatureType
import Network.AWS.Redshift.Types.UsageLimitLimitType
import Network.AWS.Redshift.Types.UsageLimitPeriod

-- | Describes a usage limit object for a cluster.
--
-- /See:/ 'newUsageLimit' smart constructor.
data UsageLimit = UsageLimit'
  { -- | The limit amount. If time-based, this amount is in minutes. If
    -- data-based, this amount is in terabytes (TB).
    amount :: Prelude.Maybe Prelude.Integer,
    -- | The Amazon Redshift feature to which the limit applies.
    featureType :: Prelude.Maybe UsageLimitFeatureType,
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
    -- | The type of limit. Depending on the feature type, this can be based on a
    -- time duration or data size.
    limitType :: Prelude.Maybe UsageLimitLimitType,
    -- | The identifier of the cluster with a usage limit.
    clusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | A list of tag instances.
    tags :: Prelude.Maybe [Tag],
    -- | The time period that the amount applies to. A @weekly@ period begins on
    -- Sunday. The default is @monthly@.
    period :: Prelude.Maybe UsageLimitPeriod,
    -- | The identifier of the usage limit.
    usageLimitId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'featureType', 'usageLimit_featureType' - The Amazon Redshift feature to which the limit applies.
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
-- 'limitType', 'usageLimit_limitType' - The type of limit. Depending on the feature type, this can be based on a
-- time duration or data size.
--
-- 'clusterIdentifier', 'usageLimit_clusterIdentifier' - The identifier of the cluster with a usage limit.
--
-- 'tags', 'usageLimit_tags' - A list of tag instances.
--
-- 'period', 'usageLimit_period' - The time period that the amount applies to. A @weekly@ period begins on
-- Sunday. The default is @monthly@.
--
-- 'usageLimitId', 'usageLimit_usageLimitId' - The identifier of the usage limit.
newUsageLimit ::
  UsageLimit
newUsageLimit =
  UsageLimit'
    { amount = Prelude.Nothing,
      featureType = Prelude.Nothing,
      breachAction = Prelude.Nothing,
      limitType = Prelude.Nothing,
      clusterIdentifier = Prelude.Nothing,
      tags = Prelude.Nothing,
      period = Prelude.Nothing,
      usageLimitId = Prelude.Nothing
    }

-- | The limit amount. If time-based, this amount is in minutes. If
-- data-based, this amount is in terabytes (TB).
usageLimit_amount :: Lens.Lens' UsageLimit (Prelude.Maybe Prelude.Integer)
usageLimit_amount = Lens.lens (\UsageLimit' {amount} -> amount) (\s@UsageLimit' {} a -> s {amount = a} :: UsageLimit)

-- | The Amazon Redshift feature to which the limit applies.
usageLimit_featureType :: Lens.Lens' UsageLimit (Prelude.Maybe UsageLimitFeatureType)
usageLimit_featureType = Lens.lens (\UsageLimit' {featureType} -> featureType) (\s@UsageLimit' {} a -> s {featureType = a} :: UsageLimit)

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

-- | The type of limit. Depending on the feature type, this can be based on a
-- time duration or data size.
usageLimit_limitType :: Lens.Lens' UsageLimit (Prelude.Maybe UsageLimitLimitType)
usageLimit_limitType = Lens.lens (\UsageLimit' {limitType} -> limitType) (\s@UsageLimit' {} a -> s {limitType = a} :: UsageLimit)

-- | The identifier of the cluster with a usage limit.
usageLimit_clusterIdentifier :: Lens.Lens' UsageLimit (Prelude.Maybe Prelude.Text)
usageLimit_clusterIdentifier = Lens.lens (\UsageLimit' {clusterIdentifier} -> clusterIdentifier) (\s@UsageLimit' {} a -> s {clusterIdentifier = a} :: UsageLimit)

-- | A list of tag instances.
usageLimit_tags :: Lens.Lens' UsageLimit (Prelude.Maybe [Tag])
usageLimit_tags = Lens.lens (\UsageLimit' {tags} -> tags) (\s@UsageLimit' {} a -> s {tags = a} :: UsageLimit) Prelude.. Lens.mapping Prelude._Coerce

-- | The time period that the amount applies to. A @weekly@ period begins on
-- Sunday. The default is @monthly@.
usageLimit_period :: Lens.Lens' UsageLimit (Prelude.Maybe UsageLimitPeriod)
usageLimit_period = Lens.lens (\UsageLimit' {period} -> period) (\s@UsageLimit' {} a -> s {period = a} :: UsageLimit)

-- | The identifier of the usage limit.
usageLimit_usageLimitId :: Lens.Lens' UsageLimit (Prelude.Maybe Prelude.Text)
usageLimit_usageLimitId = Lens.lens (\UsageLimit' {usageLimitId} -> usageLimitId) (\s@UsageLimit' {} a -> s {usageLimitId = a} :: UsageLimit)

instance Prelude.FromXML UsageLimit where
  parseXML x =
    UsageLimit'
      Prelude.<$> (x Prelude..@? "Amount")
      Prelude.<*> (x Prelude..@? "FeatureType")
      Prelude.<*> (x Prelude..@? "BreachAction")
      Prelude.<*> (x Prelude..@? "LimitType")
      Prelude.<*> (x Prelude..@? "ClusterIdentifier")
      Prelude.<*> ( x Prelude..@? "Tags" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "Tag")
                  )
      Prelude.<*> (x Prelude..@? "Period")
      Prelude.<*> (x Prelude..@? "UsageLimitId")

instance Prelude.Hashable UsageLimit

instance Prelude.NFData UsageLimit
