{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.CreateBillingGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a billing group.
module Network.AWS.IoT.CreateBillingGroup
    (
    -- * Creating a request
      CreateBillingGroup (..)
    , mkCreateBillingGroup
    -- ** Request lenses
    , cbgBillingGroupName
    , cbgBillingGroupProperties
    , cbgTags

    -- * Destructuring the response
    , CreateBillingGroupResponse (..)
    , mkCreateBillingGroupResponse
    -- ** Response lenses
    , cbgrrsBillingGroupArn
    , cbgrrsBillingGroupId
    , cbgrrsBillingGroupName
    , cbgrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateBillingGroup' smart constructor.
data CreateBillingGroup = CreateBillingGroup'
  { billingGroupName :: Types.BillingGroupName
    -- ^ The name you wish to give to the billing group.
  , billingGroupProperties :: Core.Maybe Types.BillingGroupProperties
    -- ^ The properties of the billing group.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Metadata which can be used to manage the billing group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateBillingGroup' value with any optional fields omitted.
mkCreateBillingGroup
    :: Types.BillingGroupName -- ^ 'billingGroupName'
    -> CreateBillingGroup
mkCreateBillingGroup billingGroupName
  = CreateBillingGroup'{billingGroupName,
                        billingGroupProperties = Core.Nothing, tags = Core.Nothing}

-- | The name you wish to give to the billing group.
--
-- /Note:/ Consider using 'billingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbgBillingGroupName :: Lens.Lens' CreateBillingGroup Types.BillingGroupName
cbgBillingGroupName = Lens.field @"billingGroupName"
{-# INLINEABLE cbgBillingGroupName #-}
{-# DEPRECATED billingGroupName "Use generic-lens or generic-optics with 'billingGroupName' instead"  #-}

-- | The properties of the billing group.
--
-- /Note:/ Consider using 'billingGroupProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbgBillingGroupProperties :: Lens.Lens' CreateBillingGroup (Core.Maybe Types.BillingGroupProperties)
cbgBillingGroupProperties = Lens.field @"billingGroupProperties"
{-# INLINEABLE cbgBillingGroupProperties #-}
{-# DEPRECATED billingGroupProperties "Use generic-lens or generic-optics with 'billingGroupProperties' instead"  #-}

-- | Metadata which can be used to manage the billing group.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbgTags :: Lens.Lens' CreateBillingGroup (Core.Maybe [Types.Tag])
cbgTags = Lens.field @"tags"
{-# INLINEABLE cbgTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateBillingGroup where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateBillingGroup where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON CreateBillingGroup where
        toJSON CreateBillingGroup{..}
          = Core.object
              (Core.catMaybes
                 [("billingGroupProperties" Core..=) Core.<$>
                    billingGroupProperties,
                  ("tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateBillingGroup where
        type Rs CreateBillingGroup = CreateBillingGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/billing-groups/" Core.<> Core.toText billingGroupName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateBillingGroupResponse' Core.<$>
                   (x Core..:? "billingGroupArn") Core.<*> x Core..:? "billingGroupId"
                     Core.<*> x Core..:? "billingGroupName"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateBillingGroupResponse' smart constructor.
data CreateBillingGroupResponse = CreateBillingGroupResponse'
  { billingGroupArn :: Core.Maybe Types.BillingGroupArn
    -- ^ The ARN of the billing group.
  , billingGroupId :: Core.Maybe Types.BillingGroupId
    -- ^ The ID of the billing group.
  , billingGroupName :: Core.Maybe Types.BillingGroupName
    -- ^ The name you gave to the billing group.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateBillingGroupResponse' value with any optional fields omitted.
mkCreateBillingGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateBillingGroupResponse
mkCreateBillingGroupResponse responseStatus
  = CreateBillingGroupResponse'{billingGroupArn = Core.Nothing,
                                billingGroupId = Core.Nothing, billingGroupName = Core.Nothing,
                                responseStatus}

-- | The ARN of the billing group.
--
-- /Note:/ Consider using 'billingGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbgrrsBillingGroupArn :: Lens.Lens' CreateBillingGroupResponse (Core.Maybe Types.BillingGroupArn)
cbgrrsBillingGroupArn = Lens.field @"billingGroupArn"
{-# INLINEABLE cbgrrsBillingGroupArn #-}
{-# DEPRECATED billingGroupArn "Use generic-lens or generic-optics with 'billingGroupArn' instead"  #-}

-- | The ID of the billing group.
--
-- /Note:/ Consider using 'billingGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbgrrsBillingGroupId :: Lens.Lens' CreateBillingGroupResponse (Core.Maybe Types.BillingGroupId)
cbgrrsBillingGroupId = Lens.field @"billingGroupId"
{-# INLINEABLE cbgrrsBillingGroupId #-}
{-# DEPRECATED billingGroupId "Use generic-lens or generic-optics with 'billingGroupId' instead"  #-}

-- | The name you gave to the billing group.
--
-- /Note:/ Consider using 'billingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbgrrsBillingGroupName :: Lens.Lens' CreateBillingGroupResponse (Core.Maybe Types.BillingGroupName)
cbgrrsBillingGroupName = Lens.field @"billingGroupName"
{-# INLINEABLE cbgrrsBillingGroupName #-}
{-# DEPRECATED billingGroupName "Use generic-lens or generic-optics with 'billingGroupName' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbgrrsResponseStatus :: Lens.Lens' CreateBillingGroupResponse Core.Int
cbgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cbgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
