-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.UsageLimit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.UsageLimit
  ( UsageLimit (..),

    -- * Smart constructor
    mkUsageLimit,

    -- * Lenses
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
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.Tag
import Network.AWS.Redshift.Types.UsageLimitBreachAction
import Network.AWS.Redshift.Types.UsageLimitFeatureType
import Network.AWS.Redshift.Types.UsageLimitLimitType
import Network.AWS.Redshift.Types.UsageLimitPeriod

-- | Describes a usage limit object for a cluster.
--
-- /See:/ 'mkUsageLimit' smart constructor.
data UsageLimit = UsageLimit'
  { amount :: Lude.Maybe Lude.Integer,
    limitType :: Lude.Maybe UsageLimitLimitType,
    usageLimitId :: Lude.Maybe Lude.Text,
    period :: Lude.Maybe UsageLimitPeriod,
    clusterIdentifier :: Lude.Maybe Lude.Text,
    breachAction :: Lude.Maybe UsageLimitBreachAction,
    featureType :: Lude.Maybe UsageLimitFeatureType,
    tags :: Lude.Maybe [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UsageLimit' with the minimum fields required to make a request.
--
-- * 'amount' - The limit amount. If time-based, this amount is in minutes. If data-based, this amount is in terabytes (TB).
-- * 'breachAction' - The action that Amazon Redshift takes when the limit is reached. Possible values are:
--
--
--     * __log__ - To log an event in a system table. The default is log.
--
--
--     * __emit-metric__ - To emit CloudWatch metrics.
--
--
--     * __disable__ - To disable the feature until the next usage period begins.
--
--
-- * 'clusterIdentifier' - The identifier of the cluster with a usage limit.
-- * 'featureType' - The Amazon Redshift feature to which the limit applies.
-- * 'limitType' - The type of limit. Depending on the feature type, this can be based on a time duration or data size.
-- * 'period' - The time period that the amount applies to. A @weekly@ period begins on Sunday. The default is @monthly@ .
-- * 'tags' - A list of tag instances.
-- * 'usageLimitId' - The identifier of the usage limit.
mkUsageLimit ::
  UsageLimit
mkUsageLimit =
  UsageLimit'
    { amount = Lude.Nothing,
      limitType = Lude.Nothing,
      usageLimitId = Lude.Nothing,
      period = Lude.Nothing,
      clusterIdentifier = Lude.Nothing,
      breachAction = Lude.Nothing,
      featureType = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The limit amount. If time-based, this amount is in minutes. If data-based, this amount is in terabytes (TB).
--
-- /Note:/ Consider using 'amount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulAmount :: Lens.Lens' UsageLimit (Lude.Maybe Lude.Integer)
ulAmount = Lens.lens (amount :: UsageLimit -> Lude.Maybe Lude.Integer) (\s a -> s {amount = a} :: UsageLimit)
{-# DEPRECATED ulAmount "Use generic-lens or generic-optics with 'amount' instead." #-}

-- | The type of limit. Depending on the feature type, this can be based on a time duration or data size.
--
-- /Note:/ Consider using 'limitType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulLimitType :: Lens.Lens' UsageLimit (Lude.Maybe UsageLimitLimitType)
ulLimitType = Lens.lens (limitType :: UsageLimit -> Lude.Maybe UsageLimitLimitType) (\s a -> s {limitType = a} :: UsageLimit)
{-# DEPRECATED ulLimitType "Use generic-lens or generic-optics with 'limitType' instead." #-}

-- | The identifier of the usage limit.
--
-- /Note:/ Consider using 'usageLimitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulUsageLimitId :: Lens.Lens' UsageLimit (Lude.Maybe Lude.Text)
ulUsageLimitId = Lens.lens (usageLimitId :: UsageLimit -> Lude.Maybe Lude.Text) (\s a -> s {usageLimitId = a} :: UsageLimit)
{-# DEPRECATED ulUsageLimitId "Use generic-lens or generic-optics with 'usageLimitId' instead." #-}

-- | The time period that the amount applies to. A @weekly@ period begins on Sunday. The default is @monthly@ .
--
-- /Note:/ Consider using 'period' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulPeriod :: Lens.Lens' UsageLimit (Lude.Maybe UsageLimitPeriod)
ulPeriod = Lens.lens (period :: UsageLimit -> Lude.Maybe UsageLimitPeriod) (\s a -> s {period = a} :: UsageLimit)
{-# DEPRECATED ulPeriod "Use generic-lens or generic-optics with 'period' instead." #-}

-- | The identifier of the cluster with a usage limit.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulClusterIdentifier :: Lens.Lens' UsageLimit (Lude.Maybe Lude.Text)
ulClusterIdentifier = Lens.lens (clusterIdentifier :: UsageLimit -> Lude.Maybe Lude.Text) (\s a -> s {clusterIdentifier = a} :: UsageLimit)
{-# DEPRECATED ulClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

-- | The action that Amazon Redshift takes when the limit is reached. Possible values are:
--
--
--     * __log__ - To log an event in a system table. The default is log.
--
--
--     * __emit-metric__ - To emit CloudWatch metrics.
--
--
--     * __disable__ - To disable the feature until the next usage period begins.
--
--
--
-- /Note:/ Consider using 'breachAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulBreachAction :: Lens.Lens' UsageLimit (Lude.Maybe UsageLimitBreachAction)
ulBreachAction = Lens.lens (breachAction :: UsageLimit -> Lude.Maybe UsageLimitBreachAction) (\s a -> s {breachAction = a} :: UsageLimit)
{-# DEPRECATED ulBreachAction "Use generic-lens or generic-optics with 'breachAction' instead." #-}

-- | The Amazon Redshift feature to which the limit applies.
--
-- /Note:/ Consider using 'featureType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulFeatureType :: Lens.Lens' UsageLimit (Lude.Maybe UsageLimitFeatureType)
ulFeatureType = Lens.lens (featureType :: UsageLimit -> Lude.Maybe UsageLimitFeatureType) (\s a -> s {featureType = a} :: UsageLimit)
{-# DEPRECATED ulFeatureType "Use generic-lens or generic-optics with 'featureType' instead." #-}

-- | A list of tag instances.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulTags :: Lens.Lens' UsageLimit (Lude.Maybe [Tag])
ulTags = Lens.lens (tags :: UsageLimit -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: UsageLimit)
{-# DEPRECATED ulTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromXML UsageLimit where
  parseXML x =
    UsageLimit'
      Lude.<$> (x Lude..@? "Amount")
      Lude.<*> (x Lude..@? "LimitType")
      Lude.<*> (x Lude..@? "UsageLimitId")
      Lude.<*> (x Lude..@? "Period")
      Lude.<*> (x Lude..@? "ClusterIdentifier")
      Lude.<*> (x Lude..@? "BreachAction")
      Lude.<*> (x Lude..@? "FeatureType")
      Lude.<*> ( x Lude..@? "Tags" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "Tag")
               )
