{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.FailedScheduledUpdateGroupActionRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScaling.Types.FailedScheduledUpdateGroupActionRequest
  ( FailedScheduledUpdateGroupActionRequest (..)
  -- * Smart constructor
  , mkFailedScheduledUpdateGroupActionRequest
  -- * Lenses
  , fsugarScheduledActionName
  , fsugarErrorCode
  , fsugarErrorMessage
  ) where

import qualified Network.AWS.AutoScaling.Types.XmlString as Types
import qualified Network.AWS.AutoScaling.Types.XmlStringMaxLen255 as Types
import qualified Network.AWS.AutoScaling.Types.XmlStringMaxLen64 as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a scheduled action that could not be created, updated, or deleted.
--
-- /See:/ 'mkFailedScheduledUpdateGroupActionRequest' smart constructor.
data FailedScheduledUpdateGroupActionRequest = FailedScheduledUpdateGroupActionRequest'
  { scheduledActionName :: Types.XmlStringMaxLen255
    -- ^ The name of the scheduled action.
  , errorCode :: Core.Maybe Types.XmlStringMaxLen64
    -- ^ The error code.
  , errorMessage :: Core.Maybe Types.XmlString
    -- ^ The error message accompanying the error code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FailedScheduledUpdateGroupActionRequest' value with any optional fields omitted.
mkFailedScheduledUpdateGroupActionRequest
    :: Types.XmlStringMaxLen255 -- ^ 'scheduledActionName'
    -> FailedScheduledUpdateGroupActionRequest
mkFailedScheduledUpdateGroupActionRequest scheduledActionName
  = FailedScheduledUpdateGroupActionRequest'{scheduledActionName,
                                             errorCode = Core.Nothing, errorMessage = Core.Nothing}

-- | The name of the scheduled action.
--
-- /Note:/ Consider using 'scheduledActionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsugarScheduledActionName :: Lens.Lens' FailedScheduledUpdateGroupActionRequest Types.XmlStringMaxLen255
fsugarScheduledActionName = Lens.field @"scheduledActionName"
{-# INLINEABLE fsugarScheduledActionName #-}
{-# DEPRECATED scheduledActionName "Use generic-lens or generic-optics with 'scheduledActionName' instead"  #-}

-- | The error code.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsugarErrorCode :: Lens.Lens' FailedScheduledUpdateGroupActionRequest (Core.Maybe Types.XmlStringMaxLen64)
fsugarErrorCode = Lens.field @"errorCode"
{-# INLINEABLE fsugarErrorCode #-}
{-# DEPRECATED errorCode "Use generic-lens or generic-optics with 'errorCode' instead"  #-}

-- | The error message accompanying the error code.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsugarErrorMessage :: Lens.Lens' FailedScheduledUpdateGroupActionRequest (Core.Maybe Types.XmlString)
fsugarErrorMessage = Lens.field @"errorMessage"
{-# INLINEABLE fsugarErrorMessage #-}
{-# DEPRECATED errorMessage "Use generic-lens or generic-optics with 'errorMessage' instead"  #-}

instance Core.FromXML FailedScheduledUpdateGroupActionRequest where
        parseXML x
          = FailedScheduledUpdateGroupActionRequest' Core.<$>
              (x Core..@ "ScheduledActionName") Core.<*> x Core..@? "ErrorCode"
                Core.<*> x Core..@? "ErrorMessage"
