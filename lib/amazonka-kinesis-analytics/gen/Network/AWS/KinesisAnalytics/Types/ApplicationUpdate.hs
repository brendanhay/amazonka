{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.ApplicationUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.ApplicationUpdate
  ( ApplicationUpdate (..),

    -- * Smart constructor
    mkApplicationUpdate,

    -- * Lenses
    auApplicationCodeUpdate,
    auCloudWatchLoggingOptionUpdates,
    auInputUpdates,
    auOutputUpdates,
    auReferenceDataSourceUpdates,
  )
where

import qualified Network.AWS.KinesisAnalytics.Types.ApplicationCode as Types
import qualified Network.AWS.KinesisAnalytics.Types.CloudWatchLoggingOptionUpdate as Types
import qualified Network.AWS.KinesisAnalytics.Types.InputUpdate as Types
import qualified Network.AWS.KinesisAnalytics.Types.OutputUpdate as Types
import qualified Network.AWS.KinesisAnalytics.Types.ReferenceDataSourceUpdate as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes updates to apply to an existing Amazon Kinesis Analytics application.
--
-- /See:/ 'mkApplicationUpdate' smart constructor.
data ApplicationUpdate = ApplicationUpdate'
  { -- | Describes application code updates.
    applicationCodeUpdate :: Core.Maybe Types.ApplicationCode,
    -- | Describes application CloudWatch logging option updates.
    cloudWatchLoggingOptionUpdates :: Core.Maybe [Types.CloudWatchLoggingOptionUpdate],
    -- | Describes application input configuration updates.
    inputUpdates :: Core.Maybe [Types.InputUpdate],
    -- | Describes application output configuration updates.
    outputUpdates :: Core.Maybe [Types.OutputUpdate],
    -- | Describes application reference data source updates.
    referenceDataSourceUpdates :: Core.Maybe [Types.ReferenceDataSourceUpdate]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ApplicationUpdate' value with any optional fields omitted.
mkApplicationUpdate ::
  ApplicationUpdate
mkApplicationUpdate =
  ApplicationUpdate'
    { applicationCodeUpdate = Core.Nothing,
      cloudWatchLoggingOptionUpdates = Core.Nothing,
      inputUpdates = Core.Nothing,
      outputUpdates = Core.Nothing,
      referenceDataSourceUpdates = Core.Nothing
    }

-- | Describes application code updates.
--
-- /Note:/ Consider using 'applicationCodeUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
auApplicationCodeUpdate :: Lens.Lens' ApplicationUpdate (Core.Maybe Types.ApplicationCode)
auApplicationCodeUpdate = Lens.field @"applicationCodeUpdate"
{-# DEPRECATED auApplicationCodeUpdate "Use generic-lens or generic-optics with 'applicationCodeUpdate' instead." #-}

-- | Describes application CloudWatch logging option updates.
--
-- /Note:/ Consider using 'cloudWatchLoggingOptionUpdates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
auCloudWatchLoggingOptionUpdates :: Lens.Lens' ApplicationUpdate (Core.Maybe [Types.CloudWatchLoggingOptionUpdate])
auCloudWatchLoggingOptionUpdates = Lens.field @"cloudWatchLoggingOptionUpdates"
{-# DEPRECATED auCloudWatchLoggingOptionUpdates "Use generic-lens or generic-optics with 'cloudWatchLoggingOptionUpdates' instead." #-}

-- | Describes application input configuration updates.
--
-- /Note:/ Consider using 'inputUpdates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
auInputUpdates :: Lens.Lens' ApplicationUpdate (Core.Maybe [Types.InputUpdate])
auInputUpdates = Lens.field @"inputUpdates"
{-# DEPRECATED auInputUpdates "Use generic-lens or generic-optics with 'inputUpdates' instead." #-}

-- | Describes application output configuration updates.
--
-- /Note:/ Consider using 'outputUpdates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
auOutputUpdates :: Lens.Lens' ApplicationUpdate (Core.Maybe [Types.OutputUpdate])
auOutputUpdates = Lens.field @"outputUpdates"
{-# DEPRECATED auOutputUpdates "Use generic-lens or generic-optics with 'outputUpdates' instead." #-}

-- | Describes application reference data source updates.
--
-- /Note:/ Consider using 'referenceDataSourceUpdates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
auReferenceDataSourceUpdates :: Lens.Lens' ApplicationUpdate (Core.Maybe [Types.ReferenceDataSourceUpdate])
auReferenceDataSourceUpdates = Lens.field @"referenceDataSourceUpdates"
{-# DEPRECATED auReferenceDataSourceUpdates "Use generic-lens or generic-optics with 'referenceDataSourceUpdates' instead." #-}

instance Core.FromJSON ApplicationUpdate where
  toJSON ApplicationUpdate {..} =
    Core.object
      ( Core.catMaybes
          [ ("ApplicationCodeUpdate" Core..=) Core.<$> applicationCodeUpdate,
            ("CloudWatchLoggingOptionUpdates" Core..=)
              Core.<$> cloudWatchLoggingOptionUpdates,
            ("InputUpdates" Core..=) Core.<$> inputUpdates,
            ("OutputUpdates" Core..=) Core.<$> outputUpdates,
            ("ReferenceDataSourceUpdates" Core..=)
              Core.<$> referenceDataSourceUpdates
          ]
      )
