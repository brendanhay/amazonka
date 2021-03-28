{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyInstanceCreditSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the credit option for CPU usage on a running or stopped burstable performance instance. The credit options are @standard@ and @unlimited@ .
--
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/burstable-performance-instances.html Burstable performance instances> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.ModifyInstanceCreditSpecification
    (
    -- * Creating a request
      ModifyInstanceCreditSpecification (..)
    , mkModifyInstanceCreditSpecification
    -- ** Request lenses
    , micsInstanceCreditSpecifications
    , micsClientToken
    , micsDryRun

    -- * Destructuring the response
    , ModifyInstanceCreditSpecificationResponse (..)
    , mkModifyInstanceCreditSpecificationResponse
    -- ** Response lenses
    , micsrrsSuccessfulInstanceCreditSpecifications
    , micsrrsUnsuccessfulInstanceCreditSpecifications
    , micsrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyInstanceCreditSpecification' smart constructor.
data ModifyInstanceCreditSpecification = ModifyInstanceCreditSpecification'
  { instanceCreditSpecifications :: [Types.InstanceCreditSpecificationRequest]
    -- ^ Information about the credit option for CPU usage.
  , clientToken :: Core.Maybe Core.Text
    -- ^ A unique, case-sensitive token that you provide to ensure idempotency of your modification request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyInstanceCreditSpecification' value with any optional fields omitted.
mkModifyInstanceCreditSpecification
    :: ModifyInstanceCreditSpecification
mkModifyInstanceCreditSpecification
  = ModifyInstanceCreditSpecification'{instanceCreditSpecifications =
                                         Core.mempty,
                                       clientToken = Core.Nothing, dryRun = Core.Nothing}

-- | Information about the credit option for CPU usage.
--
-- /Note:/ Consider using 'instanceCreditSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
micsInstanceCreditSpecifications :: Lens.Lens' ModifyInstanceCreditSpecification [Types.InstanceCreditSpecificationRequest]
micsInstanceCreditSpecifications = Lens.field @"instanceCreditSpecifications"
{-# INLINEABLE micsInstanceCreditSpecifications #-}
{-# DEPRECATED instanceCreditSpecifications "Use generic-lens or generic-optics with 'instanceCreditSpecifications' instead"  #-}

-- | A unique, case-sensitive token that you provide to ensure idempotency of your modification request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
micsClientToken :: Lens.Lens' ModifyInstanceCreditSpecification (Core.Maybe Core.Text)
micsClientToken = Lens.field @"clientToken"
{-# INLINEABLE micsClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
micsDryRun :: Lens.Lens' ModifyInstanceCreditSpecification (Core.Maybe Core.Bool)
micsDryRun = Lens.field @"dryRun"
{-# INLINEABLE micsDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery ModifyInstanceCreditSpecification where
        toQuery ModifyInstanceCreditSpecification{..}
          = Core.toQueryPair "Action"
              ("ModifyInstanceCreditSpecification" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.toQueryList "InstanceCreditSpecification"
                instanceCreditSpecifications
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ClientToken") clientToken
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders ModifyInstanceCreditSpecification where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyInstanceCreditSpecification where
        type Rs ModifyInstanceCreditSpecification =
             ModifyInstanceCreditSpecificationResponse
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
                 ModifyInstanceCreditSpecificationResponse' Core.<$>
                   (x Core..@? "successfulInstanceCreditSpecificationSet" Core..<@>
                      Core.parseXMLList "item")
                     Core.<*>
                     x Core..@? "unsuccessfulInstanceCreditSpecificationSet" Core..<@>
                       Core.parseXMLList "item"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyInstanceCreditSpecificationResponse' smart constructor.
data ModifyInstanceCreditSpecificationResponse = ModifyInstanceCreditSpecificationResponse'
  { successfulInstanceCreditSpecifications :: Core.Maybe [Types.SuccessfulInstanceCreditSpecificationItem]
    -- ^ Information about the instances whose credit option for CPU usage was successfully modified.
  , unsuccessfulInstanceCreditSpecifications :: Core.Maybe [Types.UnsuccessfulInstanceCreditSpecificationItem]
    -- ^ Information about the instances whose credit option for CPU usage was not modified.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyInstanceCreditSpecificationResponse' value with any optional fields omitted.
mkModifyInstanceCreditSpecificationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyInstanceCreditSpecificationResponse
mkModifyInstanceCreditSpecificationResponse responseStatus
  = ModifyInstanceCreditSpecificationResponse'{successfulInstanceCreditSpecifications
                                                 = Core.Nothing,
                                               unsuccessfulInstanceCreditSpecifications =
                                                 Core.Nothing,
                                               responseStatus}

-- | Information about the instances whose credit option for CPU usage was successfully modified.
--
-- /Note:/ Consider using 'successfulInstanceCreditSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
micsrrsSuccessfulInstanceCreditSpecifications :: Lens.Lens' ModifyInstanceCreditSpecificationResponse (Core.Maybe [Types.SuccessfulInstanceCreditSpecificationItem])
micsrrsSuccessfulInstanceCreditSpecifications = Lens.field @"successfulInstanceCreditSpecifications"
{-# INLINEABLE micsrrsSuccessfulInstanceCreditSpecifications #-}
{-# DEPRECATED successfulInstanceCreditSpecifications "Use generic-lens or generic-optics with 'successfulInstanceCreditSpecifications' instead"  #-}

-- | Information about the instances whose credit option for CPU usage was not modified.
--
-- /Note:/ Consider using 'unsuccessfulInstanceCreditSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
micsrrsUnsuccessfulInstanceCreditSpecifications :: Lens.Lens' ModifyInstanceCreditSpecificationResponse (Core.Maybe [Types.UnsuccessfulInstanceCreditSpecificationItem])
micsrrsUnsuccessfulInstanceCreditSpecifications = Lens.field @"unsuccessfulInstanceCreditSpecifications"
{-# INLINEABLE micsrrsUnsuccessfulInstanceCreditSpecifications #-}
{-# DEPRECATED unsuccessfulInstanceCreditSpecifications "Use generic-lens or generic-optics with 'unsuccessfulInstanceCreditSpecifications' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
micsrrsResponseStatus :: Lens.Lens' ModifyInstanceCreditSpecificationResponse Core.Int
micsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE micsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
