{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConfigExportDeliveryInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConfigExportDeliveryInfo
  ( ConfigExportDeliveryInfo (..),

    -- * Smart constructor
    mkConfigExportDeliveryInfo,

    -- * Lenses
    cediLastAttemptTime,
    cediLastErrorCode,
    cediLastErrorMessage,
    cediLastStatus,
    cediLastSuccessfulTime,
    cediNextDeliveryTime,
  )
where

import qualified Network.AWS.Config.Types.DeliveryStatus as Types
import qualified Network.AWS.Config.Types.LastErrorCode as Types
import qualified Network.AWS.Config.Types.LastErrorMessage as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides status of the delivery of the snapshot or the configuration history to the specified Amazon S3 bucket. Also provides the status of notifications about the Amazon S3 delivery to the specified Amazon SNS topic.
--
-- /See:/ 'mkConfigExportDeliveryInfo' smart constructor.
data ConfigExportDeliveryInfo = ConfigExportDeliveryInfo'
  { -- | The time of the last attempted delivery.
    lastAttemptTime :: Core.Maybe Core.NominalDiffTime,
    -- | The error code from the last attempted delivery.
    lastErrorCode :: Core.Maybe Types.LastErrorCode,
    -- | The error message from the last attempted delivery.
    lastErrorMessage :: Core.Maybe Types.LastErrorMessage,
    -- | Status of the last attempted delivery.
    lastStatus :: Core.Maybe Types.DeliveryStatus,
    -- | The time of the last successful delivery.
    lastSuccessfulTime :: Core.Maybe Core.NominalDiffTime,
    -- | The time that the next delivery occurs.
    nextDeliveryTime :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ConfigExportDeliveryInfo' value with any optional fields omitted.
mkConfigExportDeliveryInfo ::
  ConfigExportDeliveryInfo
mkConfigExportDeliveryInfo =
  ConfigExportDeliveryInfo'
    { lastAttemptTime = Core.Nothing,
      lastErrorCode = Core.Nothing,
      lastErrorMessage = Core.Nothing,
      lastStatus = Core.Nothing,
      lastSuccessfulTime = Core.Nothing,
      nextDeliveryTime = Core.Nothing
    }

-- | The time of the last attempted delivery.
--
-- /Note:/ Consider using 'lastAttemptTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cediLastAttemptTime :: Lens.Lens' ConfigExportDeliveryInfo (Core.Maybe Core.NominalDiffTime)
cediLastAttemptTime = Lens.field @"lastAttemptTime"
{-# DEPRECATED cediLastAttemptTime "Use generic-lens or generic-optics with 'lastAttemptTime' instead." #-}

-- | The error code from the last attempted delivery.
--
-- /Note:/ Consider using 'lastErrorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cediLastErrorCode :: Lens.Lens' ConfigExportDeliveryInfo (Core.Maybe Types.LastErrorCode)
cediLastErrorCode = Lens.field @"lastErrorCode"
{-# DEPRECATED cediLastErrorCode "Use generic-lens or generic-optics with 'lastErrorCode' instead." #-}

-- | The error message from the last attempted delivery.
--
-- /Note:/ Consider using 'lastErrorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cediLastErrorMessage :: Lens.Lens' ConfigExportDeliveryInfo (Core.Maybe Types.LastErrorMessage)
cediLastErrorMessage = Lens.field @"lastErrorMessage"
{-# DEPRECATED cediLastErrorMessage "Use generic-lens or generic-optics with 'lastErrorMessage' instead." #-}

-- | Status of the last attempted delivery.
--
-- /Note:/ Consider using 'lastStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cediLastStatus :: Lens.Lens' ConfigExportDeliveryInfo (Core.Maybe Types.DeliveryStatus)
cediLastStatus = Lens.field @"lastStatus"
{-# DEPRECATED cediLastStatus "Use generic-lens or generic-optics with 'lastStatus' instead." #-}

-- | The time of the last successful delivery.
--
-- /Note:/ Consider using 'lastSuccessfulTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cediLastSuccessfulTime :: Lens.Lens' ConfigExportDeliveryInfo (Core.Maybe Core.NominalDiffTime)
cediLastSuccessfulTime = Lens.field @"lastSuccessfulTime"
{-# DEPRECATED cediLastSuccessfulTime "Use generic-lens or generic-optics with 'lastSuccessfulTime' instead." #-}

-- | The time that the next delivery occurs.
--
-- /Note:/ Consider using 'nextDeliveryTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cediNextDeliveryTime :: Lens.Lens' ConfigExportDeliveryInfo (Core.Maybe Core.NominalDiffTime)
cediNextDeliveryTime = Lens.field @"nextDeliveryTime"
{-# DEPRECATED cediNextDeliveryTime "Use generic-lens or generic-optics with 'nextDeliveryTime' instead." #-}

instance Core.FromJSON ConfigExportDeliveryInfo where
  parseJSON =
    Core.withObject "ConfigExportDeliveryInfo" Core.$
      \x ->
        ConfigExportDeliveryInfo'
          Core.<$> (x Core..:? "lastAttemptTime")
          Core.<*> (x Core..:? "lastErrorCode")
          Core.<*> (x Core..:? "lastErrorMessage")
          Core.<*> (x Core..:? "lastStatus")
          Core.<*> (x Core..:? "lastSuccessfulTime")
          Core.<*> (x Core..:? "nextDeliveryTime")
