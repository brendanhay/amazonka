{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConfigSnapshotDeliveryProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConfigSnapshotDeliveryProperties
  ( ConfigSnapshotDeliveryProperties (..),

    -- * Smart constructor
    mkConfigSnapshotDeliveryProperties,

    -- * Lenses
    csdpDeliveryFrequency,
  )
where

import qualified Network.AWS.Config.Types.MaximumExecutionFrequency as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides options for how often AWS Config delivers configuration snapshots to the Amazon S3 bucket in your delivery channel.
--
-- The frequency for a rule that triggers evaluations for your resources when AWS Config delivers the configuration snapshot is set by one of two values, depending on which is less frequent:
--
--     * The value for the @deliveryFrequency@ parameter within the delivery channel configuration, which sets how often AWS Config delivers configuration snapshots. This value also sets how often AWS Config invokes evaluations for AWS Config rules.
--
--
--     * The value for the @MaximumExecutionFrequency@ parameter, which sets the maximum frequency with which AWS Config invokes evaluations for the rule. For more information, see 'ConfigRule' .
--
--
-- If the @deliveryFrequency@ value is less frequent than the @MaximumExecutionFrequency@ value for a rule, AWS Config invokes the rule only as often as the @deliveryFrequency@ value.
--
--     * For example, you want your rule to run evaluations when AWS Config delivers the configuration snapshot.
--
--
--     * You specify the @MaximumExecutionFrequency@ value for @Six_Hours@ .
--
--
--     * You then specify the delivery channel @deliveryFrequency@ value for @TwentyFour_Hours@ .
--
--
--     * Because the value for @deliveryFrequency@ is less frequent than @MaximumExecutionFrequency@ , AWS Config invokes evaluations for the rule every 24 hours.
--
--
-- You should set the @MaximumExecutionFrequency@ value to be at least as frequent as the @deliveryFrequency@ value. You can view the @deliveryFrequency@ value by using the @DescribeDeliveryChannnels@ action.
-- To update the @deliveryFrequency@ with which AWS Config delivers your configuration snapshots, use the @PutDeliveryChannel@ action.
--
-- /See:/ 'mkConfigSnapshotDeliveryProperties' smart constructor.
newtype ConfigSnapshotDeliveryProperties = ConfigSnapshotDeliveryProperties'
  { -- | The frequency with which AWS Config delivers configuration snapshots.
    deliveryFrequency :: Core.Maybe Types.MaximumExecutionFrequency
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ConfigSnapshotDeliveryProperties' value with any optional fields omitted.
mkConfigSnapshotDeliveryProperties ::
  ConfigSnapshotDeliveryProperties
mkConfigSnapshotDeliveryProperties =
  ConfigSnapshotDeliveryProperties'
    { deliveryFrequency =
        Core.Nothing
    }

-- | The frequency with which AWS Config delivers configuration snapshots.
--
-- /Note:/ Consider using 'deliveryFrequency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdpDeliveryFrequency :: Lens.Lens' ConfigSnapshotDeliveryProperties (Core.Maybe Types.MaximumExecutionFrequency)
csdpDeliveryFrequency = Lens.field @"deliveryFrequency"
{-# DEPRECATED csdpDeliveryFrequency "Use generic-lens or generic-optics with 'deliveryFrequency' instead." #-}

instance Core.FromJSON ConfigSnapshotDeliveryProperties where
  toJSON ConfigSnapshotDeliveryProperties {..} =
    Core.object
      ( Core.catMaybes
          [("deliveryFrequency" Core..=) Core.<$> deliveryFrequency]
      )

instance Core.FromJSON ConfigSnapshotDeliveryProperties where
  parseJSON =
    Core.withObject "ConfigSnapshotDeliveryProperties" Core.$
      \x ->
        ConfigSnapshotDeliveryProperties'
          Core.<$> (x Core..:? "deliveryFrequency")
