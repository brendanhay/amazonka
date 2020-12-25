{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.NotifyWorkersFailureStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.NotifyWorkersFailureStatus
  ( NotifyWorkersFailureStatus (..),

    -- * Smart constructor
    mkNotifyWorkersFailureStatus,

    -- * Lenses
    nwfsNotifyWorkersFailureCode,
    nwfsNotifyWorkersFailureMessage,
    nwfsWorkerId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MechanicalTurk.Types.NotifyWorkersFailureCode as Types
import qualified Network.AWS.MechanicalTurk.Types.NotifyWorkersFailureMessage as Types
import qualified Network.AWS.MechanicalTurk.Types.WorkerId as Types
import qualified Network.AWS.Prelude as Core

-- | When MTurk encounters an issue with notifying the Workers you specified, it returns back this object with failure details.
--
-- /See:/ 'mkNotifyWorkersFailureStatus' smart constructor.
data NotifyWorkersFailureStatus = NotifyWorkersFailureStatus'
  { -- | Encoded value for the failure type.
    notifyWorkersFailureCode :: Core.Maybe Types.NotifyWorkersFailureCode,
    -- | A message detailing the reason the Worker could not be notified.
    notifyWorkersFailureMessage :: Core.Maybe Types.NotifyWorkersFailureMessage,
    -- | The ID of the Worker.
    workerId :: Core.Maybe Types.WorkerId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NotifyWorkersFailureStatus' value with any optional fields omitted.
mkNotifyWorkersFailureStatus ::
  NotifyWorkersFailureStatus
mkNotifyWorkersFailureStatus =
  NotifyWorkersFailureStatus'
    { notifyWorkersFailureCode =
        Core.Nothing,
      notifyWorkersFailureMessage = Core.Nothing,
      workerId = Core.Nothing
    }

-- | Encoded value for the failure type.
--
-- /Note:/ Consider using 'notifyWorkersFailureCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nwfsNotifyWorkersFailureCode :: Lens.Lens' NotifyWorkersFailureStatus (Core.Maybe Types.NotifyWorkersFailureCode)
nwfsNotifyWorkersFailureCode = Lens.field @"notifyWorkersFailureCode"
{-# DEPRECATED nwfsNotifyWorkersFailureCode "Use generic-lens or generic-optics with 'notifyWorkersFailureCode' instead." #-}

-- | A message detailing the reason the Worker could not be notified.
--
-- /Note:/ Consider using 'notifyWorkersFailureMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nwfsNotifyWorkersFailureMessage :: Lens.Lens' NotifyWorkersFailureStatus (Core.Maybe Types.NotifyWorkersFailureMessage)
nwfsNotifyWorkersFailureMessage = Lens.field @"notifyWorkersFailureMessage"
{-# DEPRECATED nwfsNotifyWorkersFailureMessage "Use generic-lens or generic-optics with 'notifyWorkersFailureMessage' instead." #-}

-- | The ID of the Worker.
--
-- /Note:/ Consider using 'workerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nwfsWorkerId :: Lens.Lens' NotifyWorkersFailureStatus (Core.Maybe Types.WorkerId)
nwfsWorkerId = Lens.field @"workerId"
{-# DEPRECATED nwfsWorkerId "Use generic-lens or generic-optics with 'workerId' instead." #-}

instance Core.FromJSON NotifyWorkersFailureStatus where
  parseJSON =
    Core.withObject "NotifyWorkersFailureStatus" Core.$
      \x ->
        NotifyWorkersFailureStatus'
          Core.<$> (x Core..:? "NotifyWorkersFailureCode")
          Core.<*> (x Core..:? "NotifyWorkersFailureMessage")
          Core.<*> (x Core..:? "WorkerId")
