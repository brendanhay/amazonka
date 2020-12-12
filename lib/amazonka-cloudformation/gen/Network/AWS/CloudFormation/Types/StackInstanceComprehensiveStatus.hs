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

import Network.AWS.CloudFormation.Types.StackInstanceDetailedStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The detailed status of the stack instance.
--
-- /See:/ 'mkStackInstanceComprehensiveStatus' smart constructor.
newtype StackInstanceComprehensiveStatus = StackInstanceComprehensiveStatus'
  { detailedStatus ::
      Lude.Maybe
        StackInstanceDetailedStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StackInstanceComprehensiveStatus' with the minimum fields required to make a request.
--
-- * 'detailedStatus' -
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
mkStackInstanceComprehensiveStatus ::
  StackInstanceComprehensiveStatus
mkStackInstanceComprehensiveStatus =
  StackInstanceComprehensiveStatus' {detailedStatus = Lude.Nothing}

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
sicsDetailedStatus :: Lens.Lens' StackInstanceComprehensiveStatus (Lude.Maybe StackInstanceDetailedStatus)
sicsDetailedStatus = Lens.lens (detailedStatus :: StackInstanceComprehensiveStatus -> Lude.Maybe StackInstanceDetailedStatus) (\s a -> s {detailedStatus = a} :: StackInstanceComprehensiveStatus)
{-# DEPRECATED sicsDetailedStatus "Use generic-lens or generic-optics with 'detailedStatus' instead." #-}

instance Lude.FromXML StackInstanceComprehensiveStatus where
  parseXML x =
    StackInstanceComprehensiveStatus'
      Lude.<$> (x Lude..@? "DetailedStatus")
