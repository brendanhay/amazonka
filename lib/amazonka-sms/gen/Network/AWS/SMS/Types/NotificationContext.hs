{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.NotificationContext
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.NotificationContext
  ( NotificationContext (..),

    -- * Smart constructor
    mkNotificationContext,

    -- * Lenses
    ncStatus,
    ncStatusMessage,
    ncValidationId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SMS.Types.ValidationId as Types
import qualified Network.AWS.SMS.Types.ValidationStatus as Types
import qualified Network.AWS.SMS.Types.ValidationStatusMessage as Types

-- | Contains the status of validating an application.
--
-- /See:/ 'mkNotificationContext' smart constructor.
data NotificationContext = NotificationContext'
  { -- | The status of the validation.
    status :: Core.Maybe Types.ValidationStatus,
    -- | The status message.
    statusMessage :: Core.Maybe Types.ValidationStatusMessage,
    -- | The ID of the validation.
    validationId :: Core.Maybe Types.ValidationId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NotificationContext' value with any optional fields omitted.
mkNotificationContext ::
  NotificationContext
mkNotificationContext =
  NotificationContext'
    { status = Core.Nothing,
      statusMessage = Core.Nothing,
      validationId = Core.Nothing
    }

-- | The status of the validation.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncStatus :: Lens.Lens' NotificationContext (Core.Maybe Types.ValidationStatus)
ncStatus = Lens.field @"status"
{-# DEPRECATED ncStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The status message.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncStatusMessage :: Lens.Lens' NotificationContext (Core.Maybe Types.ValidationStatusMessage)
ncStatusMessage = Lens.field @"statusMessage"
{-# DEPRECATED ncStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

-- | The ID of the validation.
--
-- /Note:/ Consider using 'validationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncValidationId :: Lens.Lens' NotificationContext (Core.Maybe Types.ValidationId)
ncValidationId = Lens.field @"validationId"
{-# DEPRECATED ncValidationId "Use generic-lens or generic-optics with 'validationId' instead." #-}

instance Core.FromJSON NotificationContext where
  toJSON NotificationContext {..} =
    Core.object
      ( Core.catMaybes
          [ ("status" Core..=) Core.<$> status,
            ("statusMessage" Core..=) Core.<$> statusMessage,
            ("validationId" Core..=) Core.<$> validationId
          ]
      )
