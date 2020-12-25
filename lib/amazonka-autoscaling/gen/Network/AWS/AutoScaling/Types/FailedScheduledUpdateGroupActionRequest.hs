{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.FailedScheduledUpdateGroupActionRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.FailedScheduledUpdateGroupActionRequest
  ( FailedScheduledUpdateGroupActionRequest (..),

    -- * Smart constructor
    mkFailedScheduledUpdateGroupActionRequest,

    -- * Lenses
    fsugarScheduledActionName,
    fsugarErrorCode,
    fsugarErrorMessage,
  )
where

import qualified Network.AWS.AutoScaling.Types.XmlString as Types
import qualified Network.AWS.AutoScaling.Types.XmlStringMaxLen255 as Types
import qualified Network.AWS.AutoScaling.Types.XmlStringMaxLen64 as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a scheduled action that could not be created, updated, or deleted.
--
-- /See:/ 'mkFailedScheduledUpdateGroupActionRequest' smart constructor.
data FailedScheduledUpdateGroupActionRequest = FailedScheduledUpdateGroupActionRequest'
  { -- | The name of the scheduled action.
    scheduledActionName :: Types.XmlStringMaxLen255,
    -- | The error code.
    errorCode :: Core.Maybe Types.XmlStringMaxLen64,
    -- | The error message accompanying the error code.
    errorMessage :: Core.Maybe Types.XmlString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FailedScheduledUpdateGroupActionRequest' value with any optional fields omitted.
mkFailedScheduledUpdateGroupActionRequest ::
  -- | 'scheduledActionName'
  Types.XmlStringMaxLen255 ->
  FailedScheduledUpdateGroupActionRequest
mkFailedScheduledUpdateGroupActionRequest scheduledActionName =
  FailedScheduledUpdateGroupActionRequest'
    { scheduledActionName,
      errorCode = Core.Nothing,
      errorMessage = Core.Nothing
    }

-- | The name of the scheduled action.
--
-- /Note:/ Consider using 'scheduledActionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsugarScheduledActionName :: Lens.Lens' FailedScheduledUpdateGroupActionRequest Types.XmlStringMaxLen255
fsugarScheduledActionName = Lens.field @"scheduledActionName"
{-# DEPRECATED fsugarScheduledActionName "Use generic-lens or generic-optics with 'scheduledActionName' instead." #-}

-- | The error code.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsugarErrorCode :: Lens.Lens' FailedScheduledUpdateGroupActionRequest (Core.Maybe Types.XmlStringMaxLen64)
fsugarErrorCode = Lens.field @"errorCode"
{-# DEPRECATED fsugarErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | The error message accompanying the error code.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsugarErrorMessage :: Lens.Lens' FailedScheduledUpdateGroupActionRequest (Core.Maybe Types.XmlString)
fsugarErrorMessage = Lens.field @"errorMessage"
{-# DEPRECATED fsugarErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

instance Core.FromXML FailedScheduledUpdateGroupActionRequest where
  parseXML x =
    FailedScheduledUpdateGroupActionRequest'
      Core.<$> (x Core..@ "ScheduledActionName")
      Core.<*> (x Core..@? "ErrorCode")
      Core.<*> (x Core..@? "ErrorMessage")
