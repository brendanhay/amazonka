{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ReportInstanceStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Submits feedback about the status of an instance. The instance must be in the @running@ state. If your experience with the instance differs from the instance status returned by 'DescribeInstanceStatus' , use 'ReportInstanceStatus' to report your experience with the instance. Amazon EC2 collects this information to improve the accuracy of status checks.
--
-- Use of this action does not change the value returned by 'DescribeInstanceStatus' .
module Network.AWS.EC2.ReportInstanceStatus
    (
    -- * Creating a request
      ReportInstanceStatus (..)
    , mkReportInstanceStatus
    -- ** Request lenses
    , rissInstances
    , rissReasonCodes
    , rissStatus
    , rissDescription
    , rissDryRun
    , rissEndTime
    , rissStartTime

    -- * Destructuring the response
    , ReportInstanceStatusResponse (..)
    , mkReportInstanceStatusResponse
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkReportInstanceStatus' smart constructor.
data ReportInstanceStatus = ReportInstanceStatus'
  { instances :: [Types.InstanceId]
    -- ^ The instances.
  , reasonCodes :: [Types.ReportInstanceReasonCodes]
    -- ^ The reason codes that describe the health state of your instance.
--
--
--     * @instance-stuck-in-state@ : My instance is stuck in a state.
--
--
--     * @unresponsive@ : My instance is unresponsive.
--
--
--     * @not-accepting-credentials@ : My instance is not accepting my credentials.
--
--
--     * @password-not-available@ : A password is not available for my instance.
--
--
--     * @performance-network@ : My instance is experiencing performance problems that I believe are network related.
--
--
--     * @performance-instance-store@ : My instance is experiencing performance problems that I believe are related to the instance stores.
--
--
--     * @performance-ebs-volume@ : My instance is experiencing performance problems that I believe are related to an EBS volume.
--
--
--     * @performance-other@ : My instance is experiencing performance problems.
--
--
--     * @other@ : [explain using the description parameter]
--
--
  , status :: Types.ReportStatusType
    -- ^ The status of all instances listed.
  , description :: Core.Maybe Core.Text
    -- ^ Descriptive text about the health state of your instance.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , endTime :: Core.Maybe Core.UTCTime
    -- ^ The time at which the reported instance health state ended.
  , startTime :: Core.Maybe Core.UTCTime
    -- ^ The time at which the reported instance health state began.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ReportInstanceStatus' value with any optional fields omitted.
mkReportInstanceStatus
    :: Types.ReportStatusType -- ^ 'status'
    -> ReportInstanceStatus
mkReportInstanceStatus status
  = ReportInstanceStatus'{instances = Core.mempty,
                          reasonCodes = Core.mempty, status, description = Core.Nothing,
                          dryRun = Core.Nothing, endTime = Core.Nothing,
                          startTime = Core.Nothing}

-- | The instances.
--
-- /Note:/ Consider using 'instances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rissInstances :: Lens.Lens' ReportInstanceStatus [Types.InstanceId]
rissInstances = Lens.field @"instances"
{-# INLINEABLE rissInstances #-}
{-# DEPRECATED instances "Use generic-lens or generic-optics with 'instances' instead"  #-}

-- | The reason codes that describe the health state of your instance.
--
--
--     * @instance-stuck-in-state@ : My instance is stuck in a state.
--
--
--     * @unresponsive@ : My instance is unresponsive.
--
--
--     * @not-accepting-credentials@ : My instance is not accepting my credentials.
--
--
--     * @password-not-available@ : A password is not available for my instance.
--
--
--     * @performance-network@ : My instance is experiencing performance problems that I believe are network related.
--
--
--     * @performance-instance-store@ : My instance is experiencing performance problems that I believe are related to the instance stores.
--
--
--     * @performance-ebs-volume@ : My instance is experiencing performance problems that I believe are related to an EBS volume.
--
--
--     * @performance-other@ : My instance is experiencing performance problems.
--
--
--     * @other@ : [explain using the description parameter]
--
--
--
-- /Note:/ Consider using 'reasonCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rissReasonCodes :: Lens.Lens' ReportInstanceStatus [Types.ReportInstanceReasonCodes]
rissReasonCodes = Lens.field @"reasonCodes"
{-# INLINEABLE rissReasonCodes #-}
{-# DEPRECATED reasonCodes "Use generic-lens or generic-optics with 'reasonCodes' instead"  #-}

-- | The status of all instances listed.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rissStatus :: Lens.Lens' ReportInstanceStatus Types.ReportStatusType
rissStatus = Lens.field @"status"
{-# INLINEABLE rissStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | Descriptive text about the health state of your instance.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rissDescription :: Lens.Lens' ReportInstanceStatus (Core.Maybe Core.Text)
rissDescription = Lens.field @"description"
{-# INLINEABLE rissDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rissDryRun :: Lens.Lens' ReportInstanceStatus (Core.Maybe Core.Bool)
rissDryRun = Lens.field @"dryRun"
{-# INLINEABLE rissDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The time at which the reported instance health state ended.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rissEndTime :: Lens.Lens' ReportInstanceStatus (Core.Maybe Core.UTCTime)
rissEndTime = Lens.field @"endTime"
{-# INLINEABLE rissEndTime #-}
{-# DEPRECATED endTime "Use generic-lens or generic-optics with 'endTime' instead"  #-}

-- | The time at which the reported instance health state began.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rissStartTime :: Lens.Lens' ReportInstanceStatus (Core.Maybe Core.UTCTime)
rissStartTime = Lens.field @"startTime"
{-# INLINEABLE rissStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

instance Core.ToQuery ReportInstanceStatus where
        toQuery ReportInstanceStatus{..}
          = Core.toQueryPair "Action" ("ReportInstanceStatus" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryList "InstanceId" instances
              Core.<> Core.toQueryList "ReasonCode" reasonCodes
              Core.<> Core.toQueryPair "Status" status
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Description") description
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "EndTime") endTime
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "StartTime") startTime

instance Core.ToHeaders ReportInstanceStatus where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ReportInstanceStatus where
        type Rs ReportInstanceStatus = ReportInstanceStatusResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull ReportInstanceStatusResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkReportInstanceStatusResponse' smart constructor.
data ReportInstanceStatusResponse = ReportInstanceStatusResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReportInstanceStatusResponse' value with any optional fields omitted.
mkReportInstanceStatusResponse
    :: ReportInstanceStatusResponse
mkReportInstanceStatusResponse = ReportInstanceStatusResponse'
