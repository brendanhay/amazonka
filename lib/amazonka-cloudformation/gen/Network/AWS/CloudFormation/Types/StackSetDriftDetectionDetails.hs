{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackSetDriftDetectionDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackSetDriftDetectionDetails
  ( StackSetDriftDetectionDetails (..),

    -- * Smart constructor
    mkStackSetDriftDetectionDetails,

    -- * Lenses
    ssdddDriftDetectionStatus,
    ssdddDriftStatus,
    ssdddDriftedStackInstancesCount,
    ssdddFailedStackInstancesCount,
    ssdddInProgressStackInstancesCount,
    ssdddInSyncStackInstancesCount,
    ssdddLastDriftCheckTimestamp,
    ssdddTotalStackInstancesCount,
  )
where

import qualified Network.AWS.CloudFormation.Types.StackSetDriftDetectionStatus as Types
import qualified Network.AWS.CloudFormation.Types.StackSetDriftStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Detailed information about the drift status of the stack set.
--
-- For stack sets, contains information about the last /completed/ drift operation performed on the stack set. Information about drift operations in-progress is not included.
-- For stack set operations, includes information about drift operations currently being performed on the stack set.
-- For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-drift.html Detecting Unmanaged Changes in Stack Sets> in the /AWS CloudFormation User Guide/ .
--
-- /See:/ 'mkStackSetDriftDetectionDetails' smart constructor.
data StackSetDriftDetectionDetails = StackSetDriftDetectionDetails'
  { -- | The status of the stack set drift detection operation.
    --
    --
    --     * @COMPLETED@ : The drift detection operation completed without failing on any stack instances.
    --
    --
    --     * @FAILED@ : The drift detection operation exceeded the specified failure tolerance.
    --
    --
    --     * @PARTIAL_SUCCESS@ : The drift detection operation completed without exceeding the failure tolerance for the operation.
    --
    --
    --     * @IN_PROGRESS@ : The drift detection operation is currently being performed.
    --
    --
    --     * @STOPPED@ : The user has cancelled the drift detection operation.
    driftDetectionStatus :: Core.Maybe Types.StackSetDriftDetectionStatus,
    -- | Status of the stack set's actual configuration compared to its expected template and parameter configuration. A stack set is considered to have drifted if one or more of its stack instances have drifted from their expected template and parameter configuration.
    --
    --
    --     * @DRIFTED@ : One or more of the stack instances belonging to the stack set stack differs from the expected template and parameter configuration. A stack instance is considered to have drifted if one or more of the resources in the associated stack have drifted.
    --
    --
    --     * @NOT_CHECKED@ : AWS CloudFormation has not checked the stack set for drift.
    --
    --
    --     * @IN_SYNC@ : All of the stack instances belonging to the stack set stack match from the expected template and parameter configuration.
    driftStatus :: Core.Maybe Types.StackSetDriftStatus,
    -- | The number of stack instances that have drifted from the expected template and parameter configuration of the stack set. A stack instance is considered to have drifted if one or more of the resources in the associated stack do not match their expected configuration.
    driftedStackInstancesCount :: Core.Maybe Core.Natural,
    -- | The number of stack instances for which the drift detection operation failed.
    failedStackInstancesCount :: Core.Maybe Core.Natural,
    -- | The number of stack instances that are currently being checked for drift.
    inProgressStackInstancesCount :: Core.Maybe Core.Natural,
    -- | The number of stack instances which match the expected template and parameter configuration of the stack set.
    inSyncStackInstancesCount :: Core.Maybe Core.Natural,
    -- | Most recent time when CloudFormation performed a drift detection operation on the stack set. This value will be @NULL@ for any stack set on which drift detection has not yet been performed.
    lastDriftCheckTimestamp :: Core.Maybe Core.UTCTime,
    -- | The total number of stack instances belonging to this stack set.
    --
    -- The total number of stack instances is equal to the total of:
    --
    --     * Stack instances that match the stack set configuration.
    --
    --
    --     * Stack instances that have drifted from the stack set configuration.
    --
    --
    --     * Stack instances where the drift detection operation has failed.
    --
    --
    --     * Stack instances currently being checked for drift.
    totalStackInstancesCount :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'StackSetDriftDetectionDetails' value with any optional fields omitted.
mkStackSetDriftDetectionDetails ::
  StackSetDriftDetectionDetails
mkStackSetDriftDetectionDetails =
  StackSetDriftDetectionDetails'
    { driftDetectionStatus =
        Core.Nothing,
      driftStatus = Core.Nothing,
      driftedStackInstancesCount = Core.Nothing,
      failedStackInstancesCount = Core.Nothing,
      inProgressStackInstancesCount = Core.Nothing,
      inSyncStackInstancesCount = Core.Nothing,
      lastDriftCheckTimestamp = Core.Nothing,
      totalStackInstancesCount = Core.Nothing
    }

-- | The status of the stack set drift detection operation.
--
--
--     * @COMPLETED@ : The drift detection operation completed without failing on any stack instances.
--
--
--     * @FAILED@ : The drift detection operation exceeded the specified failure tolerance.
--
--
--     * @PARTIAL_SUCCESS@ : The drift detection operation completed without exceeding the failure tolerance for the operation.
--
--
--     * @IN_PROGRESS@ : The drift detection operation is currently being performed.
--
--
--     * @STOPPED@ : The user has cancelled the drift detection operation.
--
--
--
-- /Note:/ Consider using 'driftDetectionStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdddDriftDetectionStatus :: Lens.Lens' StackSetDriftDetectionDetails (Core.Maybe Types.StackSetDriftDetectionStatus)
ssdddDriftDetectionStatus = Lens.field @"driftDetectionStatus"
{-# DEPRECATED ssdddDriftDetectionStatus "Use generic-lens or generic-optics with 'driftDetectionStatus' instead." #-}

-- | Status of the stack set's actual configuration compared to its expected template and parameter configuration. A stack set is considered to have drifted if one or more of its stack instances have drifted from their expected template and parameter configuration.
--
--
--     * @DRIFTED@ : One or more of the stack instances belonging to the stack set stack differs from the expected template and parameter configuration. A stack instance is considered to have drifted if one or more of the resources in the associated stack have drifted.
--
--
--     * @NOT_CHECKED@ : AWS CloudFormation has not checked the stack set for drift.
--
--
--     * @IN_SYNC@ : All of the stack instances belonging to the stack set stack match from the expected template and parameter configuration.
--
--
--
-- /Note:/ Consider using 'driftStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdddDriftStatus :: Lens.Lens' StackSetDriftDetectionDetails (Core.Maybe Types.StackSetDriftStatus)
ssdddDriftStatus = Lens.field @"driftStatus"
{-# DEPRECATED ssdddDriftStatus "Use generic-lens or generic-optics with 'driftStatus' instead." #-}

-- | The number of stack instances that have drifted from the expected template and parameter configuration of the stack set. A stack instance is considered to have drifted if one or more of the resources in the associated stack do not match their expected configuration.
--
-- /Note:/ Consider using 'driftedStackInstancesCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdddDriftedStackInstancesCount :: Lens.Lens' StackSetDriftDetectionDetails (Core.Maybe Core.Natural)
ssdddDriftedStackInstancesCount = Lens.field @"driftedStackInstancesCount"
{-# DEPRECATED ssdddDriftedStackInstancesCount "Use generic-lens or generic-optics with 'driftedStackInstancesCount' instead." #-}

-- | The number of stack instances for which the drift detection operation failed.
--
-- /Note:/ Consider using 'failedStackInstancesCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdddFailedStackInstancesCount :: Lens.Lens' StackSetDriftDetectionDetails (Core.Maybe Core.Natural)
ssdddFailedStackInstancesCount = Lens.field @"failedStackInstancesCount"
{-# DEPRECATED ssdddFailedStackInstancesCount "Use generic-lens or generic-optics with 'failedStackInstancesCount' instead." #-}

-- | The number of stack instances that are currently being checked for drift.
--
-- /Note:/ Consider using 'inProgressStackInstancesCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdddInProgressStackInstancesCount :: Lens.Lens' StackSetDriftDetectionDetails (Core.Maybe Core.Natural)
ssdddInProgressStackInstancesCount = Lens.field @"inProgressStackInstancesCount"
{-# DEPRECATED ssdddInProgressStackInstancesCount "Use generic-lens or generic-optics with 'inProgressStackInstancesCount' instead." #-}

-- | The number of stack instances which match the expected template and parameter configuration of the stack set.
--
-- /Note:/ Consider using 'inSyncStackInstancesCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdddInSyncStackInstancesCount :: Lens.Lens' StackSetDriftDetectionDetails (Core.Maybe Core.Natural)
ssdddInSyncStackInstancesCount = Lens.field @"inSyncStackInstancesCount"
{-# DEPRECATED ssdddInSyncStackInstancesCount "Use generic-lens or generic-optics with 'inSyncStackInstancesCount' instead." #-}

-- | Most recent time when CloudFormation performed a drift detection operation on the stack set. This value will be @NULL@ for any stack set on which drift detection has not yet been performed.
--
-- /Note:/ Consider using 'lastDriftCheckTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdddLastDriftCheckTimestamp :: Lens.Lens' StackSetDriftDetectionDetails (Core.Maybe Core.UTCTime)
ssdddLastDriftCheckTimestamp = Lens.field @"lastDriftCheckTimestamp"
{-# DEPRECATED ssdddLastDriftCheckTimestamp "Use generic-lens or generic-optics with 'lastDriftCheckTimestamp' instead." #-}

-- | The total number of stack instances belonging to this stack set.
--
-- The total number of stack instances is equal to the total of:
--
--     * Stack instances that match the stack set configuration.
--
--
--     * Stack instances that have drifted from the stack set configuration.
--
--
--     * Stack instances where the drift detection operation has failed.
--
--
--     * Stack instances currently being checked for drift.
--
--
--
-- /Note:/ Consider using 'totalStackInstancesCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdddTotalStackInstancesCount :: Lens.Lens' StackSetDriftDetectionDetails (Core.Maybe Core.Natural)
ssdddTotalStackInstancesCount = Lens.field @"totalStackInstancesCount"
{-# DEPRECATED ssdddTotalStackInstancesCount "Use generic-lens or generic-optics with 'totalStackInstancesCount' instead." #-}

instance Core.FromXML StackSetDriftDetectionDetails where
  parseXML x =
    StackSetDriftDetectionDetails'
      Core.<$> (x Core..@? "DriftDetectionStatus")
      Core.<*> (x Core..@? "DriftStatus")
      Core.<*> (x Core..@? "DriftedStackInstancesCount")
      Core.<*> (x Core..@? "FailedStackInstancesCount")
      Core.<*> (x Core..@? "InProgressStackInstancesCount")
      Core.<*> (x Core..@? "InSyncStackInstancesCount")
      Core.<*> (x Core..@? "LastDriftCheckTimestamp")
      Core.<*> (x Core..@? "TotalStackInstancesCount")
