-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.SubscriptionFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Types.SubscriptionFilter
  ( SubscriptionFilter (..),

    -- * Smart constructor
    mkSubscriptionFilter,

    -- * Lenses
    sfCreationTime,
    sfFilterName,
    sfDistribution,
    sfDestinationARN,
    sfLogGroupName,
    sfFilterPattern,
    sfRoleARN,
  )
where

import Network.AWS.CloudWatchLogs.Types.Distribution
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a subscription filter.
--
-- /See:/ 'mkSubscriptionFilter' smart constructor.
data SubscriptionFilter = SubscriptionFilter'
  { creationTime ::
      Lude.Maybe Lude.Natural,
    filterName :: Lude.Maybe Lude.Text,
    distribution :: Lude.Maybe Distribution,
    destinationARN :: Lude.Maybe Lude.Text,
    logGroupName :: Lude.Maybe Lude.Text,
    filterPattern :: Lude.Maybe Lude.Text,
    roleARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SubscriptionFilter' with the minimum fields required to make a request.
--
-- * 'creationTime' - The creation time of the subscription filter, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
-- * 'destinationARN' - The Amazon Resource Name (ARN) of the destination.
-- * 'distribution' - Undocumented field.
-- * 'filterName' - The name of the subscription filter.
-- * 'filterPattern' - Undocumented field.
-- * 'logGroupName' - The name of the log group.
-- * 'roleARN' -
mkSubscriptionFilter ::
  SubscriptionFilter
mkSubscriptionFilter =
  SubscriptionFilter'
    { creationTime = Lude.Nothing,
      filterName = Lude.Nothing,
      distribution = Lude.Nothing,
      destinationARN = Lude.Nothing,
      logGroupName = Lude.Nothing,
      filterPattern = Lude.Nothing,
      roleARN = Lude.Nothing
    }

-- | The creation time of the subscription filter, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfCreationTime :: Lens.Lens' SubscriptionFilter (Lude.Maybe Lude.Natural)
sfCreationTime = Lens.lens (creationTime :: SubscriptionFilter -> Lude.Maybe Lude.Natural) (\s a -> s {creationTime = a} :: SubscriptionFilter)
{-# DEPRECATED sfCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The name of the subscription filter.
--
-- /Note:/ Consider using 'filterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfFilterName :: Lens.Lens' SubscriptionFilter (Lude.Maybe Lude.Text)
sfFilterName = Lens.lens (filterName :: SubscriptionFilter -> Lude.Maybe Lude.Text) (\s a -> s {filterName = a} :: SubscriptionFilter)
{-# DEPRECATED sfFilterName "Use generic-lens or generic-optics with 'filterName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'distribution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfDistribution :: Lens.Lens' SubscriptionFilter (Lude.Maybe Distribution)
sfDistribution = Lens.lens (distribution :: SubscriptionFilter -> Lude.Maybe Distribution) (\s a -> s {distribution = a} :: SubscriptionFilter)
{-# DEPRECATED sfDistribution "Use generic-lens or generic-optics with 'distribution' instead." #-}

-- | The Amazon Resource Name (ARN) of the destination.
--
-- /Note:/ Consider using 'destinationARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfDestinationARN :: Lens.Lens' SubscriptionFilter (Lude.Maybe Lude.Text)
sfDestinationARN = Lens.lens (destinationARN :: SubscriptionFilter -> Lude.Maybe Lude.Text) (\s a -> s {destinationARN = a} :: SubscriptionFilter)
{-# DEPRECATED sfDestinationARN "Use generic-lens or generic-optics with 'destinationARN' instead." #-}

-- | The name of the log group.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfLogGroupName :: Lens.Lens' SubscriptionFilter (Lude.Maybe Lude.Text)
sfLogGroupName = Lens.lens (logGroupName :: SubscriptionFilter -> Lude.Maybe Lude.Text) (\s a -> s {logGroupName = a} :: SubscriptionFilter)
{-# DEPRECATED sfLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'filterPattern' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfFilterPattern :: Lens.Lens' SubscriptionFilter (Lude.Maybe Lude.Text)
sfFilterPattern = Lens.lens (filterPattern :: SubscriptionFilter -> Lude.Maybe Lude.Text) (\s a -> s {filterPattern = a} :: SubscriptionFilter)
{-# DEPRECATED sfFilterPattern "Use generic-lens or generic-optics with 'filterPattern' instead." #-}

-- |
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfRoleARN :: Lens.Lens' SubscriptionFilter (Lude.Maybe Lude.Text)
sfRoleARN = Lens.lens (roleARN :: SubscriptionFilter -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: SubscriptionFilter)
{-# DEPRECATED sfRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON SubscriptionFilter where
  parseJSON =
    Lude.withObject
      "SubscriptionFilter"
      ( \x ->
          SubscriptionFilter'
            Lude.<$> (x Lude..:? "creationTime")
            Lude.<*> (x Lude..:? "filterName")
            Lude.<*> (x Lude..:? "distribution")
            Lude.<*> (x Lude..:? "destinationArn")
            Lude.<*> (x Lude..:? "logGroupName")
            Lude.<*> (x Lude..:? "filterPattern")
            Lude.<*> (x Lude..:? "roleArn")
      )
