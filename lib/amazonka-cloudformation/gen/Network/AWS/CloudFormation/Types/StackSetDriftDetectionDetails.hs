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
    ssdddLastDriftCheckTimestamp,
    ssdddTotalStackInstancesCount,
    ssdddInProgressStackInstancesCount,
    ssdddDriftedStackInstancesCount,
    ssdddDriftDetectionStatus,
    ssdddDriftStatus,
    ssdddFailedStackInstancesCount,
    ssdddInSyncStackInstancesCount,
  )
where

import Network.AWS.CloudFormation.Types.StackSetDriftDetectionStatus
import Network.AWS.CloudFormation.Types.StackSetDriftStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Detailed information about the drift status of the stack set.
--
-- For stack sets, contains information about the last /completed/ drift operation performed on the stack set. Information about drift operations in-progress is not included.
-- For stack set operations, includes information about drift operations currently being performed on the stack set.
-- For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-drift.html Detecting Unmanaged Changes in Stack Sets> in the /AWS CloudFormation User Guide/ .
--
-- /See:/ 'mkStackSetDriftDetectionDetails' smart constructor.
data StackSetDriftDetectionDetails = StackSetDriftDetectionDetails'
  { lastDriftCheckTimestamp ::
      Lude.Maybe Lude.ISO8601,
    totalStackInstancesCount ::
      Lude.Maybe Lude.Natural,
    inProgressStackInstancesCount ::
      Lude.Maybe Lude.Natural,
    driftedStackInstancesCount ::
      Lude.Maybe Lude.Natural,
    driftDetectionStatus ::
      Lude.Maybe
        StackSetDriftDetectionStatus,
    driftStatus ::
      Lude.Maybe StackSetDriftStatus,
    failedStackInstancesCount ::
      Lude.Maybe Lude.Natural,
    inSyncStackInstancesCount ::
      Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StackSetDriftDetectionDetails' with the minimum fields required to make a request.
--
-- * 'driftDetectionStatus' - The status of the stack set drift detection operation.
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
-- * 'driftStatus' - Status of the stack set's actual configuration compared to its expected template and parameter configuration. A stack set is considered to have drifted if one or more of its stack instances have drifted from their expected template and parameter configuration.
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
-- * 'driftedStackInstancesCount' - The number of stack instances that have drifted from the expected template and parameter configuration of the stack set. A stack instance is considered to have drifted if one or more of the resources in the associated stack do not match their expected configuration.
-- * 'failedStackInstancesCount' - The number of stack instances for which the drift detection operation failed.
-- * 'inProgressStackInstancesCount' - The number of stack instances that are currently being checked for drift.
-- * 'inSyncStackInstancesCount' - The number of stack instances which match the expected template and parameter configuration of the stack set.
-- * 'lastDriftCheckTimestamp' - Most recent time when CloudFormation performed a drift detection operation on the stack set. This value will be @NULL@ for any stack set on which drift detection has not yet been performed.
-- * 'totalStackInstancesCount' - The total number of stack instances belonging to this stack set.
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
mkStackSetDriftDetectionDetails ::
  StackSetDriftDetectionDetails
mkStackSetDriftDetectionDetails =
  StackSetDriftDetectionDetails'
    { lastDriftCheckTimestamp =
        Lude.Nothing,
      totalStackInstancesCount = Lude.Nothing,
      inProgressStackInstancesCount = Lude.Nothing,
      driftedStackInstancesCount = Lude.Nothing,
      driftDetectionStatus = Lude.Nothing,
      driftStatus = Lude.Nothing,
      failedStackInstancesCount = Lude.Nothing,
      inSyncStackInstancesCount = Lude.Nothing
    }

-- | Most recent time when CloudFormation performed a drift detection operation on the stack set. This value will be @NULL@ for any stack set on which drift detection has not yet been performed.
--
-- /Note:/ Consider using 'lastDriftCheckTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdddLastDriftCheckTimestamp :: Lens.Lens' StackSetDriftDetectionDetails (Lude.Maybe Lude.ISO8601)
ssdddLastDriftCheckTimestamp = Lens.lens (lastDriftCheckTimestamp :: StackSetDriftDetectionDetails -> Lude.Maybe Lude.ISO8601) (\s a -> s {lastDriftCheckTimestamp = a} :: StackSetDriftDetectionDetails)
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
ssdddTotalStackInstancesCount :: Lens.Lens' StackSetDriftDetectionDetails (Lude.Maybe Lude.Natural)
ssdddTotalStackInstancesCount = Lens.lens (totalStackInstancesCount :: StackSetDriftDetectionDetails -> Lude.Maybe Lude.Natural) (\s a -> s {totalStackInstancesCount = a} :: StackSetDriftDetectionDetails)
{-# DEPRECATED ssdddTotalStackInstancesCount "Use generic-lens or generic-optics with 'totalStackInstancesCount' instead." #-}

-- | The number of stack instances that are currently being checked for drift.
--
-- /Note:/ Consider using 'inProgressStackInstancesCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdddInProgressStackInstancesCount :: Lens.Lens' StackSetDriftDetectionDetails (Lude.Maybe Lude.Natural)
ssdddInProgressStackInstancesCount = Lens.lens (inProgressStackInstancesCount :: StackSetDriftDetectionDetails -> Lude.Maybe Lude.Natural) (\s a -> s {inProgressStackInstancesCount = a} :: StackSetDriftDetectionDetails)
{-# DEPRECATED ssdddInProgressStackInstancesCount "Use generic-lens or generic-optics with 'inProgressStackInstancesCount' instead." #-}

-- | The number of stack instances that have drifted from the expected template and parameter configuration of the stack set. A stack instance is considered to have drifted if one or more of the resources in the associated stack do not match their expected configuration.
--
-- /Note:/ Consider using 'driftedStackInstancesCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdddDriftedStackInstancesCount :: Lens.Lens' StackSetDriftDetectionDetails (Lude.Maybe Lude.Natural)
ssdddDriftedStackInstancesCount = Lens.lens (driftedStackInstancesCount :: StackSetDriftDetectionDetails -> Lude.Maybe Lude.Natural) (\s a -> s {driftedStackInstancesCount = a} :: StackSetDriftDetectionDetails)
{-# DEPRECATED ssdddDriftedStackInstancesCount "Use generic-lens or generic-optics with 'driftedStackInstancesCount' instead." #-}

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
ssdddDriftDetectionStatus :: Lens.Lens' StackSetDriftDetectionDetails (Lude.Maybe StackSetDriftDetectionStatus)
ssdddDriftDetectionStatus = Lens.lens (driftDetectionStatus :: StackSetDriftDetectionDetails -> Lude.Maybe StackSetDriftDetectionStatus) (\s a -> s {driftDetectionStatus = a} :: StackSetDriftDetectionDetails)
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
ssdddDriftStatus :: Lens.Lens' StackSetDriftDetectionDetails (Lude.Maybe StackSetDriftStatus)
ssdddDriftStatus = Lens.lens (driftStatus :: StackSetDriftDetectionDetails -> Lude.Maybe StackSetDriftStatus) (\s a -> s {driftStatus = a} :: StackSetDriftDetectionDetails)
{-# DEPRECATED ssdddDriftStatus "Use generic-lens or generic-optics with 'driftStatus' instead." #-}

-- | The number of stack instances for which the drift detection operation failed.
--
-- /Note:/ Consider using 'failedStackInstancesCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdddFailedStackInstancesCount :: Lens.Lens' StackSetDriftDetectionDetails (Lude.Maybe Lude.Natural)
ssdddFailedStackInstancesCount = Lens.lens (failedStackInstancesCount :: StackSetDriftDetectionDetails -> Lude.Maybe Lude.Natural) (\s a -> s {failedStackInstancesCount = a} :: StackSetDriftDetectionDetails)
{-# DEPRECATED ssdddFailedStackInstancesCount "Use generic-lens or generic-optics with 'failedStackInstancesCount' instead." #-}

-- | The number of stack instances which match the expected template and parameter configuration of the stack set.
--
-- /Note:/ Consider using 'inSyncStackInstancesCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdddInSyncStackInstancesCount :: Lens.Lens' StackSetDriftDetectionDetails (Lude.Maybe Lude.Natural)
ssdddInSyncStackInstancesCount = Lens.lens (inSyncStackInstancesCount :: StackSetDriftDetectionDetails -> Lude.Maybe Lude.Natural) (\s a -> s {inSyncStackInstancesCount = a} :: StackSetDriftDetectionDetails)
{-# DEPRECATED ssdddInSyncStackInstancesCount "Use generic-lens or generic-optics with 'inSyncStackInstancesCount' instead." #-}

instance Lude.FromXML StackSetDriftDetectionDetails where
  parseXML x =
    StackSetDriftDetectionDetails'
      Lude.<$> (x Lude..@? "LastDriftCheckTimestamp")
      Lude.<*> (x Lude..@? "TotalStackInstancesCount")
      Lude.<*> (x Lude..@? "InProgressStackInstancesCount")
      Lude.<*> (x Lude..@? "DriftedStackInstancesCount")
      Lude.<*> (x Lude..@? "DriftDetectionStatus")
      Lude.<*> (x Lude..@? "DriftStatus")
      Lude.<*> (x Lude..@? "FailedStackInstancesCount")
      Lude.<*> (x Lude..@? "InSyncStackInstancesCount")
