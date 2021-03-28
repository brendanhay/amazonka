{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateTrafficMirrorFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Traffic Mirror filter.
--
-- A Traffic Mirror filter is a set of rules that defines the traffic to mirror.
-- By default, no traffic is mirrored. To mirror traffic, use <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTrafficMirrorFilterRule.htm CreateTrafficMirrorFilterRule> to add Traffic Mirror rules to the filter. The rules you add define what traffic gets mirrored. You can also use <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ModifyTrafficMirrorFilterNetworkServices.html ModifyTrafficMirrorFilterNetworkServices> to mirror supported network services.
module Network.AWS.EC2.CreateTrafficMirrorFilter
    (
    -- * Creating a request
      CreateTrafficMirrorFilter (..)
    , mkCreateTrafficMirrorFilter
    -- ** Request lenses
    , ctmfClientToken
    , ctmfDescription
    , ctmfDryRun
    , ctmfTagSpecifications

    -- * Destructuring the response
    , CreateTrafficMirrorFilterResponse (..)
    , mkCreateTrafficMirrorFilterResponse
    -- ** Response lenses
    , ctmfrrsClientToken
    , ctmfrrsTrafficMirrorFilter
    , ctmfrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateTrafficMirrorFilter' smart constructor.
data CreateTrafficMirrorFilter = CreateTrafficMirrorFilter'
  { clientToken :: Core.Maybe Core.Text
    -- ^ Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
  , description :: Core.Maybe Core.Text
    -- ^ The description of the Traffic Mirror filter.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , tagSpecifications :: Core.Maybe [Types.TagSpecification]
    -- ^ The tags to assign to a Traffic Mirror filter.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTrafficMirrorFilter' value with any optional fields omitted.
mkCreateTrafficMirrorFilter
    :: CreateTrafficMirrorFilter
mkCreateTrafficMirrorFilter
  = CreateTrafficMirrorFilter'{clientToken = Core.Nothing,
                               description = Core.Nothing, dryRun = Core.Nothing,
                               tagSpecifications = Core.Nothing}

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmfClientToken :: Lens.Lens' CreateTrafficMirrorFilter (Core.Maybe Core.Text)
ctmfClientToken = Lens.field @"clientToken"
{-# INLINEABLE ctmfClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | The description of the Traffic Mirror filter.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmfDescription :: Lens.Lens' CreateTrafficMirrorFilter (Core.Maybe Core.Text)
ctmfDescription = Lens.field @"description"
{-# INLINEABLE ctmfDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmfDryRun :: Lens.Lens' CreateTrafficMirrorFilter (Core.Maybe Core.Bool)
ctmfDryRun = Lens.field @"dryRun"
{-# INLINEABLE ctmfDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The tags to assign to a Traffic Mirror filter.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmfTagSpecifications :: Lens.Lens' CreateTrafficMirrorFilter (Core.Maybe [Types.TagSpecification])
ctmfTagSpecifications = Lens.field @"tagSpecifications"
{-# INLINEABLE ctmfTagSpecifications #-}
{-# DEPRECATED tagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead"  #-}

instance Core.ToQuery CreateTrafficMirrorFilter where
        toQuery CreateTrafficMirrorFilter{..}
          = Core.toQueryPair "Action"
              ("CreateTrafficMirrorFilter" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ClientToken") clientToken
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Description") description
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "TagSpecification")
                tagSpecifications

instance Core.ToHeaders CreateTrafficMirrorFilter where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateTrafficMirrorFilter where
        type Rs CreateTrafficMirrorFilter =
             CreateTrafficMirrorFilterResponse
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
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 CreateTrafficMirrorFilterResponse' Core.<$>
                   (x Core..@? "clientToken") Core.<*>
                     x Core..@? "trafficMirrorFilter"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateTrafficMirrorFilterResponse' smart constructor.
data CreateTrafficMirrorFilterResponse = CreateTrafficMirrorFilterResponse'
  { clientToken :: Core.Maybe Core.Text
    -- ^ Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
  , trafficMirrorFilter :: Core.Maybe Types.TrafficMirrorFilter
    -- ^ Information about the Traffic Mirror filter.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTrafficMirrorFilterResponse' value with any optional fields omitted.
mkCreateTrafficMirrorFilterResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateTrafficMirrorFilterResponse
mkCreateTrafficMirrorFilterResponse responseStatus
  = CreateTrafficMirrorFilterResponse'{clientToken = Core.Nothing,
                                       trafficMirrorFilter = Core.Nothing, responseStatus}

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmfrrsClientToken :: Lens.Lens' CreateTrafficMirrorFilterResponse (Core.Maybe Core.Text)
ctmfrrsClientToken = Lens.field @"clientToken"
{-# INLINEABLE ctmfrrsClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | Information about the Traffic Mirror filter.
--
-- /Note:/ Consider using 'trafficMirrorFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmfrrsTrafficMirrorFilter :: Lens.Lens' CreateTrafficMirrorFilterResponse (Core.Maybe Types.TrafficMirrorFilter)
ctmfrrsTrafficMirrorFilter = Lens.field @"trafficMirrorFilter"
{-# INLINEABLE ctmfrrsTrafficMirrorFilter #-}
{-# DEPRECATED trafficMirrorFilter "Use generic-lens or generic-optics with 'trafficMirrorFilter' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmfrrsResponseStatus :: Lens.Lens' CreateTrafficMirrorFilterResponse Core.Int
ctmfrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ctmfrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
