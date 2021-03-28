{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CancelConversionTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels an active conversion task. The task can be the import of an instance or volume. The action removes all artifacts of the conversion, including a partially uploaded volume or instance. If the conversion is complete or is in the process of transferring the final disk image, the command fails and returns an exception.
--
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/CommandLineReference/ec2-cli-vmimport-export.html Importing a Virtual Machine Using the Amazon EC2 CLI> .
module Network.AWS.EC2.CancelConversionTask
    (
    -- * Creating a request
      CancelConversionTask (..)
    , mkCancelConversionTask
    -- ** Request lenses
    , cctConversionTaskId
    , cctDryRun
    , cctReasonMessage

    -- * Destructuring the response
    , CancelConversionTaskResponse (..)
    , mkCancelConversionTaskResponse
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCancelConversionTask' smart constructor.
data CancelConversionTask = CancelConversionTask'
  { conversionTaskId :: Types.ConversionTaskId
    -- ^ The ID of the conversion task.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , reasonMessage :: Core.Maybe Core.Text
    -- ^ The reason for canceling the conversion task.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CancelConversionTask' value with any optional fields omitted.
mkCancelConversionTask
    :: Types.ConversionTaskId -- ^ 'conversionTaskId'
    -> CancelConversionTask
mkCancelConversionTask conversionTaskId
  = CancelConversionTask'{conversionTaskId, dryRun = Core.Nothing,
                          reasonMessage = Core.Nothing}

-- | The ID of the conversion task.
--
-- /Note:/ Consider using 'conversionTaskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cctConversionTaskId :: Lens.Lens' CancelConversionTask Types.ConversionTaskId
cctConversionTaskId = Lens.field @"conversionTaskId"
{-# INLINEABLE cctConversionTaskId #-}
{-# DEPRECATED conversionTaskId "Use generic-lens or generic-optics with 'conversionTaskId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cctDryRun :: Lens.Lens' CancelConversionTask (Core.Maybe Core.Bool)
cctDryRun = Lens.field @"dryRun"
{-# INLINEABLE cctDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The reason for canceling the conversion task.
--
-- /Note:/ Consider using 'reasonMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cctReasonMessage :: Lens.Lens' CancelConversionTask (Core.Maybe Core.Text)
cctReasonMessage = Lens.field @"reasonMessage"
{-# INLINEABLE cctReasonMessage #-}
{-# DEPRECATED reasonMessage "Use generic-lens or generic-optics with 'reasonMessage' instead"  #-}

instance Core.ToQuery CancelConversionTask where
        toQuery CancelConversionTask{..}
          = Core.toQueryPair "Action" ("CancelConversionTask" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "ConversionTaskId" conversionTaskId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ReasonMessage")
                reasonMessage

instance Core.ToHeaders CancelConversionTask where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CancelConversionTask where
        type Rs CancelConversionTask = CancelConversionTaskResponse
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
        parseResponse = Response.receiveNull CancelConversionTaskResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCancelConversionTaskResponse' smart constructor.
data CancelConversionTaskResponse = CancelConversionTaskResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CancelConversionTaskResponse' value with any optional fields omitted.
mkCancelConversionTaskResponse
    :: CancelConversionTaskResponse
mkCancelConversionTaskResponse = CancelConversionTaskResponse'
