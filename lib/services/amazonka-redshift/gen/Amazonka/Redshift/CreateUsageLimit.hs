{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Redshift.CreateUsageLimit
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a usage limit for a specified Amazon Redshift feature on a
-- cluster. The usage limit is identified by the returned usage limit
-- identifier.
module Amazonka.Redshift.CreateUsageLimit
  ( -- * Creating a Request
    CreateUsageLimit (..),
    newCreateUsageLimit,

    -- * Request Lenses
    createUsageLimit_breachAction,
    createUsageLimit_period,
    createUsageLimit_tags,
    createUsageLimit_clusterIdentifier,
    createUsageLimit_featureType,
    createUsageLimit_limitType,
    createUsageLimit_amount,

    -- * Destructuring the Response
    UsageLimit (..),
    newUsageLimit,

    -- * Response Lenses
    usageLimit_amount,
    usageLimit_breachAction,
    usageLimit_clusterIdentifier,
    usageLimit_featureType,
    usageLimit_limitType,
    usageLimit_period,
    usageLimit_tags,
    usageLimit_usageLimitId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateUsageLimit' smart constructor.
data CreateUsageLimit = CreateUsageLimit'
  { -- | The action that Amazon Redshift takes when the limit is reached. The
    -- default is log. For more information about this parameter, see
    -- UsageLimit.
    breachAction :: Prelude.Maybe UsageLimitBreachAction,
    -- | The time period that the amount applies to. A @weekly@ period begins on
    -- Sunday. The default is @monthly@.
    period :: Prelude.Maybe UsageLimitPeriod,
    -- | A list of tag instances.
    tags :: Prelude.Maybe [Tag],
    -- | The identifier of the cluster that you want to limit usage.
    clusterIdentifier :: Prelude.Text,
    -- | The Amazon Redshift feature that you want to limit.
    featureType :: UsageLimitFeatureType,
    -- | The type of limit. Depending on the feature type, this can be based on a
    -- time duration or data size. If @FeatureType@ is @spectrum@, then
    -- @LimitType@ must be @data-scanned@. If @FeatureType@ is
    -- @concurrency-scaling@, then @LimitType@ must be @time@. If @FeatureType@
    -- is @cross-region-datasharing@, then @LimitType@ must be @data-scanned@.
    limitType :: UsageLimitLimitType,
    -- | The limit amount. If time-based, this amount is in minutes. If
    -- data-based, this amount is in terabytes (TB). The value must be a
    -- positive number.
    amount :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateUsageLimit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'breachAction', 'createUsageLimit_breachAction' - The action that Amazon Redshift takes when the limit is reached. The
-- default is log. For more information about this parameter, see
-- UsageLimit.
--
-- 'period', 'createUsageLimit_period' - The time period that the amount applies to. A @weekly@ period begins on
-- Sunday. The default is @monthly@.
--
-- 'tags', 'createUsageLimit_tags' - A list of tag instances.
--
-- 'clusterIdentifier', 'createUsageLimit_clusterIdentifier' - The identifier of the cluster that you want to limit usage.
--
-- 'featureType', 'createUsageLimit_featureType' - The Amazon Redshift feature that you want to limit.
--
-- 'limitType', 'createUsageLimit_limitType' - The type of limit. Depending on the feature type, this can be based on a
-- time duration or data size. If @FeatureType@ is @spectrum@, then
-- @LimitType@ must be @data-scanned@. If @FeatureType@ is
-- @concurrency-scaling@, then @LimitType@ must be @time@. If @FeatureType@
-- is @cross-region-datasharing@, then @LimitType@ must be @data-scanned@.
--
-- 'amount', 'createUsageLimit_amount' - The limit amount. If time-based, this amount is in minutes. If
-- data-based, this amount is in terabytes (TB). The value must be a
-- positive number.
newCreateUsageLimit ::
  -- | 'clusterIdentifier'
  Prelude.Text ->
  -- | 'featureType'
  UsageLimitFeatureType ->
  -- | 'limitType'
  UsageLimitLimitType ->
  -- | 'amount'
  Prelude.Integer ->
  CreateUsageLimit
newCreateUsageLimit
  pClusterIdentifier_
  pFeatureType_
  pLimitType_
  pAmount_ =
    CreateUsageLimit'
      { breachAction = Prelude.Nothing,
        period = Prelude.Nothing,
        tags = Prelude.Nothing,
        clusterIdentifier = pClusterIdentifier_,
        featureType = pFeatureType_,
        limitType = pLimitType_,
        amount = pAmount_
      }

-- | The action that Amazon Redshift takes when the limit is reached. The
-- default is log. For more information about this parameter, see
-- UsageLimit.
createUsageLimit_breachAction :: Lens.Lens' CreateUsageLimit (Prelude.Maybe UsageLimitBreachAction)
createUsageLimit_breachAction = Lens.lens (\CreateUsageLimit' {breachAction} -> breachAction) (\s@CreateUsageLimit' {} a -> s {breachAction = a} :: CreateUsageLimit)

-- | The time period that the amount applies to. A @weekly@ period begins on
-- Sunday. The default is @monthly@.
createUsageLimit_period :: Lens.Lens' CreateUsageLimit (Prelude.Maybe UsageLimitPeriod)
createUsageLimit_period = Lens.lens (\CreateUsageLimit' {period} -> period) (\s@CreateUsageLimit' {} a -> s {period = a} :: CreateUsageLimit)

-- | A list of tag instances.
createUsageLimit_tags :: Lens.Lens' CreateUsageLimit (Prelude.Maybe [Tag])
createUsageLimit_tags = Lens.lens (\CreateUsageLimit' {tags} -> tags) (\s@CreateUsageLimit' {} a -> s {tags = a} :: CreateUsageLimit) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the cluster that you want to limit usage.
createUsageLimit_clusterIdentifier :: Lens.Lens' CreateUsageLimit Prelude.Text
createUsageLimit_clusterIdentifier = Lens.lens (\CreateUsageLimit' {clusterIdentifier} -> clusterIdentifier) (\s@CreateUsageLimit' {} a -> s {clusterIdentifier = a} :: CreateUsageLimit)

-- | The Amazon Redshift feature that you want to limit.
createUsageLimit_featureType :: Lens.Lens' CreateUsageLimit UsageLimitFeatureType
createUsageLimit_featureType = Lens.lens (\CreateUsageLimit' {featureType} -> featureType) (\s@CreateUsageLimit' {} a -> s {featureType = a} :: CreateUsageLimit)

-- | The type of limit. Depending on the feature type, this can be based on a
-- time duration or data size. If @FeatureType@ is @spectrum@, then
-- @LimitType@ must be @data-scanned@. If @FeatureType@ is
-- @concurrency-scaling@, then @LimitType@ must be @time@. If @FeatureType@
-- is @cross-region-datasharing@, then @LimitType@ must be @data-scanned@.
createUsageLimit_limitType :: Lens.Lens' CreateUsageLimit UsageLimitLimitType
createUsageLimit_limitType = Lens.lens (\CreateUsageLimit' {limitType} -> limitType) (\s@CreateUsageLimit' {} a -> s {limitType = a} :: CreateUsageLimit)

-- | The limit amount. If time-based, this amount is in minutes. If
-- data-based, this amount is in terabytes (TB). The value must be a
-- positive number.
createUsageLimit_amount :: Lens.Lens' CreateUsageLimit Prelude.Integer
createUsageLimit_amount = Lens.lens (\CreateUsageLimit' {amount} -> amount) (\s@CreateUsageLimit' {} a -> s {amount = a} :: CreateUsageLimit)

instance Core.AWSRequest CreateUsageLimit where
  type AWSResponse CreateUsageLimit = UsageLimit
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CreateUsageLimitResult"
      (\s h x -> Data.parseXML x)

instance Prelude.Hashable CreateUsageLimit where
  hashWithSalt _salt CreateUsageLimit' {..} =
    _salt
      `Prelude.hashWithSalt` breachAction
      `Prelude.hashWithSalt` period
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` clusterIdentifier
      `Prelude.hashWithSalt` featureType
      `Prelude.hashWithSalt` limitType
      `Prelude.hashWithSalt` amount

instance Prelude.NFData CreateUsageLimit where
  rnf CreateUsageLimit' {..} =
    Prelude.rnf breachAction
      `Prelude.seq` Prelude.rnf period
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf clusterIdentifier
      `Prelude.seq` Prelude.rnf featureType
      `Prelude.seq` Prelude.rnf limitType
      `Prelude.seq` Prelude.rnf amount

instance Data.ToHeaders CreateUsageLimit where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateUsageLimit where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateUsageLimit where
  toQuery CreateUsageLimit' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateUsageLimit" :: Prelude.ByteString),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "BreachAction" Data.=: breachAction,
        "Period" Data.=: period,
        "Tags"
          Data.=: Data.toQuery
            (Data.toQueryList "Tag" Prelude.<$> tags),
        "ClusterIdentifier" Data.=: clusterIdentifier,
        "FeatureType" Data.=: featureType,
        "LimitType" Data.=: limitType,
        "Amount" Data.=: amount
      ]
