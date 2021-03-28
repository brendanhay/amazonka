{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DeleteQueuedReservedInstancesError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.DeleteQueuedReservedInstancesError
  ( DeleteQueuedReservedInstancesError (..)
  -- * Smart constructor
  , mkDeleteQueuedReservedInstancesError
  -- * Lenses
  , dqrieCode
  , dqrieMessage
  ) where

import qualified Network.AWS.EC2.Types.DeleteQueuedReservedInstancesErrorCode as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the error for a Reserved Instance whose queued purchase could not be deleted.
--
-- /See:/ 'mkDeleteQueuedReservedInstancesError' smart constructor.
data DeleteQueuedReservedInstancesError = DeleteQueuedReservedInstancesError'
  { code :: Core.Maybe Types.DeleteQueuedReservedInstancesErrorCode
    -- ^ The error code.
  , message :: Core.Maybe Core.Text
    -- ^ The error message.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteQueuedReservedInstancesError' value with any optional fields omitted.
mkDeleteQueuedReservedInstancesError
    :: DeleteQueuedReservedInstancesError
mkDeleteQueuedReservedInstancesError
  = DeleteQueuedReservedInstancesError'{code = Core.Nothing,
                                        message = Core.Nothing}

-- | The error code.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqrieCode :: Lens.Lens' DeleteQueuedReservedInstancesError (Core.Maybe Types.DeleteQueuedReservedInstancesErrorCode)
dqrieCode = Lens.field @"code"
{-# INLINEABLE dqrieCode #-}
{-# DEPRECATED code "Use generic-lens or generic-optics with 'code' instead"  #-}

-- | The error message.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqrieMessage :: Lens.Lens' DeleteQueuedReservedInstancesError (Core.Maybe Core.Text)
dqrieMessage = Lens.field @"message"
{-# INLINEABLE dqrieMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

instance Core.FromXML DeleteQueuedReservedInstancesError where
        parseXML x
          = DeleteQueuedReservedInstancesError' Core.<$>
              (x Core..@? "code") Core.<*> x Core..@? "message"
