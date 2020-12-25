{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackInstanceComprehensiveStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackInstanceComprehensiveStatus
  ( StackInstanceComprehensiveStatus (..),

    -- * Smart constructor
    mkStackInstanceComprehensiveStatus,

    -- * Lenses
    sicsDetailedStatus,
  )
where

import qualified Network.AWS.CloudFormation.Types.StackInstanceDetailedStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The detailed status of the stack instance.
--
-- /See:/ 'mkStackInstanceComprehensiveStatus' smart constructor.
newtype StackInstanceComprehensiveStatus = StackInstanceComprehensiveStatus'
  { -- |
    --
    --     * @CANCELLED@ : The operation in the specified account and Region has been cancelled. This is either because a user has stopped the stack set operation, or because the failure tolerance of the stack set operation has been exceeded.
    --
    --
    --     * @FAILED@ : The operation in the specified account and Region failed. If the stack set operation fails in enough accounts within a Region, the failure tolerance for the stack set operation as a whole might be exceeded.
    --
    --
    --     * @INOPERABLE@ : A @DeleteStackInstances@ operation has failed and left the stack in an unstable state. Stacks in this state are excluded from further @UpdateStackSet@ operations. You might need to perform a @DeleteStackInstances@ operation, with @RetainStacks@ set to @true@ , to delete the stack instance, and then delete the stack manually.
    --
    --
    --     * @PENDING@ : The operation in the specified account and Region has yet to start.
    --
    --
    --     * @RUNNING@ : The operation in the specified account and Region is currently in progress.
    --
    --
    --     * @SUCCEEDED@ : The operation in the specified account and Region completed successfully.
    detailedStatus :: Core.Maybe Types.StackInstanceDetailedStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StackInstanceComprehensiveStatus' value with any optional fields omitted.
mkStackInstanceComprehensiveStatus ::
  StackInstanceComprehensiveStatus
mkStackInstanceComprehensiveStatus =
  StackInstanceComprehensiveStatus' {detailedStatus = Core.Nothing}

-- |
--
--     * @CANCELLED@ : The operation in the specified account and Region has been cancelled. This is either because a user has stopped the stack set operation, or because the failure tolerance of the stack set operation has been exceeded.
--
--
--     * @FAILED@ : The operation in the specified account and Region failed. If the stack set operation fails in enough accounts within a Region, the failure tolerance for the stack set operation as a whole might be exceeded.
--
--
--     * @INOPERABLE@ : A @DeleteStackInstances@ operation has failed and left the stack in an unstable state. Stacks in this state are excluded from further @UpdateStackSet@ operations. You might need to perform a @DeleteStackInstances@ operation, with @RetainStacks@ set to @true@ , to delete the stack instance, and then delete the stack manually.
--
--
--     * @PENDING@ : The operation in the specified account and Region has yet to start.
--
--
--     * @RUNNING@ : The operation in the specified account and Region is currently in progress.
--
--
--     * @SUCCEEDED@ : The operation in the specified account and Region completed successfully.
--
--
--
-- /Note:/ Consider using 'detailedStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sicsDetailedStatus :: Lens.Lens' StackInstanceComprehensiveStatus (Core.Maybe Types.StackInstanceDetailedStatus)
sicsDetailedStatus = Lens.field @"detailedStatus"
{-# DEPRECATED sicsDetailedStatus "Use generic-lens or generic-optics with 'detailedStatus' instead." #-}

instance Core.FromXML StackInstanceComprehensiveStatus where
  parseXML x =
    StackInstanceComprehensiveStatus'
      Core.<$> (x Core..@? "DetailedStatus")
