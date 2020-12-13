{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.CreateUsageLimit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a usage limit for a specified Amazon Redshift feature on a cluster. The usage limit is identified by the returned usage limit identifier.
module Network.AWS.Redshift.CreateUsageLimit
  ( -- * Creating a request
    CreateUsageLimit (..),
    mkCreateUsageLimit,

    -- ** Request lenses
    culAmount,
    culLimitType,
    culPeriod,
    culClusterIdentifier,
    culBreachAction,
    culFeatureType,
    culTags,

    -- * Destructuring the response
    UsageLimit (..),
    mkUsageLimit,

    -- ** Response lenses
    ulAmount,
    ulLimitType,
    ulUsageLimitId,
    ulPeriod,
    ulClusterIdentifier,
    ulBreachAction,
    ulFeatureType,
    ulTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateUsageLimit' smart constructor.
data CreateUsageLimit = CreateUsageLimit'
  { -- | The limit amount. If time-based, this amount is in minutes. If data-based, this amount is in terabytes (TB). The value must be a positive number.
    amount :: Lude.Integer,
    -- | The type of limit. Depending on the feature type, this can be based on a time duration or data size. If @FeatureType@ is @spectrum@ , then @LimitType@ must be @data-scanned@ . If @FeatureType@ is @concurrency-scaling@ , then @LimitType@ must be @time@ .
    limitType :: UsageLimitLimitType,
    -- | The time period that the amount applies to. A @weekly@ period begins on Sunday. The default is @monthly@ .
    period :: Lude.Maybe UsageLimitPeriod,
    -- | The identifier of the cluster that you want to limit usage.
    clusterIdentifier :: Lude.Text,
    -- | The action that Amazon Redshift takes when the limit is reached. The default is log. For more information about this parameter, see 'UsageLimit' .
    breachAction :: Lude.Maybe UsageLimitBreachAction,
    -- | The Amazon Redshift feature that you want to limit.
    featureType :: UsageLimitFeatureType,
    -- | A list of tag instances.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateUsageLimit' with the minimum fields required to make a request.
--
-- * 'amount' - The limit amount. If time-based, this amount is in minutes. If data-based, this amount is in terabytes (TB). The value must be a positive number.
-- * 'limitType' - The type of limit. Depending on the feature type, this can be based on a time duration or data size. If @FeatureType@ is @spectrum@ , then @LimitType@ must be @data-scanned@ . If @FeatureType@ is @concurrency-scaling@ , then @LimitType@ must be @time@ .
-- * 'period' - The time period that the amount applies to. A @weekly@ period begins on Sunday. The default is @monthly@ .
-- * 'clusterIdentifier' - The identifier of the cluster that you want to limit usage.
-- * 'breachAction' - The action that Amazon Redshift takes when the limit is reached. The default is log. For more information about this parameter, see 'UsageLimit' .
-- * 'featureType' - The Amazon Redshift feature that you want to limit.
-- * 'tags' - A list of tag instances.
mkCreateUsageLimit ::
  -- | 'amount'
  Lude.Integer ->
  -- | 'limitType'
  UsageLimitLimitType ->
  -- | 'clusterIdentifier'
  Lude.Text ->
  -- | 'featureType'
  UsageLimitFeatureType ->
  CreateUsageLimit
mkCreateUsageLimit
  pAmount_
  pLimitType_
  pClusterIdentifier_
  pFeatureType_ =
    CreateUsageLimit'
      { amount = pAmount_,
        limitType = pLimitType_,
        period = Lude.Nothing,
        clusterIdentifier = pClusterIdentifier_,
        breachAction = Lude.Nothing,
        featureType = pFeatureType_,
        tags = Lude.Nothing
      }

-- | The limit amount. If time-based, this amount is in minutes. If data-based, this amount is in terabytes (TB). The value must be a positive number.
--
-- /Note:/ Consider using 'amount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
culAmount :: Lens.Lens' CreateUsageLimit Lude.Integer
culAmount = Lens.lens (amount :: CreateUsageLimit -> Lude.Integer) (\s a -> s {amount = a} :: CreateUsageLimit)
{-# DEPRECATED culAmount "Use generic-lens or generic-optics with 'amount' instead." #-}

-- | The type of limit. Depending on the feature type, this can be based on a time duration or data size. If @FeatureType@ is @spectrum@ , then @LimitType@ must be @data-scanned@ . If @FeatureType@ is @concurrency-scaling@ , then @LimitType@ must be @time@ .
--
-- /Note:/ Consider using 'limitType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
culLimitType :: Lens.Lens' CreateUsageLimit UsageLimitLimitType
culLimitType = Lens.lens (limitType :: CreateUsageLimit -> UsageLimitLimitType) (\s a -> s {limitType = a} :: CreateUsageLimit)
{-# DEPRECATED culLimitType "Use generic-lens or generic-optics with 'limitType' instead." #-}

-- | The time period that the amount applies to. A @weekly@ period begins on Sunday. The default is @monthly@ .
--
-- /Note:/ Consider using 'period' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
culPeriod :: Lens.Lens' CreateUsageLimit (Lude.Maybe UsageLimitPeriod)
culPeriod = Lens.lens (period :: CreateUsageLimit -> Lude.Maybe UsageLimitPeriod) (\s a -> s {period = a} :: CreateUsageLimit)
{-# DEPRECATED culPeriod "Use generic-lens or generic-optics with 'period' instead." #-}

-- | The identifier of the cluster that you want to limit usage.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
culClusterIdentifier :: Lens.Lens' CreateUsageLimit Lude.Text
culClusterIdentifier = Lens.lens (clusterIdentifier :: CreateUsageLimit -> Lude.Text) (\s a -> s {clusterIdentifier = a} :: CreateUsageLimit)
{-# DEPRECATED culClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

-- | The action that Amazon Redshift takes when the limit is reached. The default is log. For more information about this parameter, see 'UsageLimit' .
--
-- /Note:/ Consider using 'breachAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
culBreachAction :: Lens.Lens' CreateUsageLimit (Lude.Maybe UsageLimitBreachAction)
culBreachAction = Lens.lens (breachAction :: CreateUsageLimit -> Lude.Maybe UsageLimitBreachAction) (\s a -> s {breachAction = a} :: CreateUsageLimit)
{-# DEPRECATED culBreachAction "Use generic-lens or generic-optics with 'breachAction' instead." #-}

-- | The Amazon Redshift feature that you want to limit.
--
-- /Note:/ Consider using 'featureType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
culFeatureType :: Lens.Lens' CreateUsageLimit UsageLimitFeatureType
culFeatureType = Lens.lens (featureType :: CreateUsageLimit -> UsageLimitFeatureType) (\s a -> s {featureType = a} :: CreateUsageLimit)
{-# DEPRECATED culFeatureType "Use generic-lens or generic-optics with 'featureType' instead." #-}

-- | A list of tag instances.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
culTags :: Lens.Lens' CreateUsageLimit (Lude.Maybe [Tag])
culTags = Lens.lens (tags :: CreateUsageLimit -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateUsageLimit)
{-# DEPRECATED culTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateUsageLimit where
  type Rs CreateUsageLimit = UsageLimit
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "CreateUsageLimitResult"
      (\s h x -> Lude.parseXML x)

instance Lude.ToHeaders CreateUsageLimit where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateUsageLimit where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateUsageLimit where
  toQuery CreateUsageLimit' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateUsageLimit" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "Amount" Lude.=: amount,
        "LimitType" Lude.=: limitType,
        "Period" Lude.=: period,
        "ClusterIdentifier" Lude.=: clusterIdentifier,
        "BreachAction" Lude.=: breachAction,
        "FeatureType" Lude.=: featureType,
        "Tags" Lude.=: Lude.toQuery (Lude.toQueryList "Tag" Lude.<$> tags)
      ]
