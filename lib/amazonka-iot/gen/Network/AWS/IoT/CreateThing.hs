{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.CreateThing
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a thing record in the registry. If this call is made multiple times using the same thing name and configuration, the call will succeed. If this call is made with the same thing name but different configuration a @ResourceAlreadyExistsException@ is thrown.
module Network.AWS.IoT.CreateThing
    (
    -- * Creating a request
      CreateThing (..)
    , mkCreateThing
    -- ** Request lenses
    , ctThingName
    , ctAttributePayload
    , ctBillingGroupName
    , ctThingTypeName

    -- * Destructuring the response
    , CreateThingResponse (..)
    , mkCreateThingResponse
    -- ** Response lenses
    , ctrrsThingArn
    , ctrrsThingId
    , ctrrsThingName
    , ctrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the CreateThing operation.
--
-- /See:/ 'mkCreateThing' smart constructor.
data CreateThing = CreateThing'
  { thingName :: Types.ThingName
    -- ^ The name of the thing to create.
--
-- You can't change a thing's name after you create it. To change a thing's name, you must create a new thing, give it the new name, and then delete the old thing.
  , attributePayload :: Core.Maybe Types.AttributePayload
    -- ^ The attribute payload, which consists of up to three name/value pairs in a JSON document. For example:
--
-- @{\"attributes\":{\"string1\":\"string2\"}}@ 
  , billingGroupName :: Core.Maybe Types.BillingGroupName
    -- ^ The name of the billing group the thing will be added to.
  , thingTypeName :: Core.Maybe Types.ThingTypeName
    -- ^ The name of the thing type associated with the new thing.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateThing' value with any optional fields omitted.
mkCreateThing
    :: Types.ThingName -- ^ 'thingName'
    -> CreateThing
mkCreateThing thingName
  = CreateThing'{thingName, attributePayload = Core.Nothing,
                 billingGroupName = Core.Nothing, thingTypeName = Core.Nothing}

-- | The name of the thing to create.
--
-- You can't change a thing's name after you create it. To change a thing's name, you must create a new thing, give it the new name, and then delete the old thing.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctThingName :: Lens.Lens' CreateThing Types.ThingName
ctThingName = Lens.field @"thingName"
{-# INLINEABLE ctThingName #-}
{-# DEPRECATED thingName "Use generic-lens or generic-optics with 'thingName' instead"  #-}

-- | The attribute payload, which consists of up to three name/value pairs in a JSON document. For example:
--
-- @{\"attributes\":{\"string1\":\"string2\"}}@ 
--
-- /Note:/ Consider using 'attributePayload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctAttributePayload :: Lens.Lens' CreateThing (Core.Maybe Types.AttributePayload)
ctAttributePayload = Lens.field @"attributePayload"
{-# INLINEABLE ctAttributePayload #-}
{-# DEPRECATED attributePayload "Use generic-lens or generic-optics with 'attributePayload' instead"  #-}

-- | The name of the billing group the thing will be added to.
--
-- /Note:/ Consider using 'billingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctBillingGroupName :: Lens.Lens' CreateThing (Core.Maybe Types.BillingGroupName)
ctBillingGroupName = Lens.field @"billingGroupName"
{-# INLINEABLE ctBillingGroupName #-}
{-# DEPRECATED billingGroupName "Use generic-lens or generic-optics with 'billingGroupName' instead"  #-}

-- | The name of the thing type associated with the new thing.
--
-- /Note:/ Consider using 'thingTypeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctThingTypeName :: Lens.Lens' CreateThing (Core.Maybe Types.ThingTypeName)
ctThingTypeName = Lens.field @"thingTypeName"
{-# INLINEABLE ctThingTypeName #-}
{-# DEPRECATED thingTypeName "Use generic-lens or generic-optics with 'thingTypeName' instead"  #-}

instance Core.ToQuery CreateThing where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateThing where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON CreateThing where
        toJSON CreateThing{..}
          = Core.object
              (Core.catMaybes
                 [("attributePayload" Core..=) Core.<$> attributePayload,
                  ("billingGroupName" Core..=) Core.<$> billingGroupName,
                  ("thingTypeName" Core..=) Core.<$> thingTypeName])

instance Core.AWSRequest CreateThing where
        type Rs CreateThing = CreateThingResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/things/" Core.<> Core.toText thingName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateThingResponse' Core.<$>
                   (x Core..:? "thingArn") Core.<*> x Core..:? "thingId" Core.<*>
                     x Core..:? "thingName"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The output of the CreateThing operation.
--
-- /See:/ 'mkCreateThingResponse' smart constructor.
data CreateThingResponse = CreateThingResponse'
  { thingArn :: Core.Maybe Types.ThingArn
    -- ^ The ARN of the new thing.
  , thingId :: Core.Maybe Types.ThingId
    -- ^ The thing ID.
  , thingName :: Core.Maybe Types.ThingName
    -- ^ The name of the new thing.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateThingResponse' value with any optional fields omitted.
mkCreateThingResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateThingResponse
mkCreateThingResponse responseStatus
  = CreateThingResponse'{thingArn = Core.Nothing,
                         thingId = Core.Nothing, thingName = Core.Nothing, responseStatus}

-- | The ARN of the new thing.
--
-- /Note:/ Consider using 'thingArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrrsThingArn :: Lens.Lens' CreateThingResponse (Core.Maybe Types.ThingArn)
ctrrsThingArn = Lens.field @"thingArn"
{-# INLINEABLE ctrrsThingArn #-}
{-# DEPRECATED thingArn "Use generic-lens or generic-optics with 'thingArn' instead"  #-}

-- | The thing ID.
--
-- /Note:/ Consider using 'thingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrrsThingId :: Lens.Lens' CreateThingResponse (Core.Maybe Types.ThingId)
ctrrsThingId = Lens.field @"thingId"
{-# INLINEABLE ctrrsThingId #-}
{-# DEPRECATED thingId "Use generic-lens or generic-optics with 'thingId' instead"  #-}

-- | The name of the new thing.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrrsThingName :: Lens.Lens' CreateThingResponse (Core.Maybe Types.ThingName)
ctrrsThingName = Lens.field @"thingName"
{-# INLINEABLE ctrrsThingName #-}
{-# DEPRECATED thingName "Use generic-lens or generic-optics with 'thingName' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrrsResponseStatus :: Lens.Lens' CreateThingResponse Core.Int
ctrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ctrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
