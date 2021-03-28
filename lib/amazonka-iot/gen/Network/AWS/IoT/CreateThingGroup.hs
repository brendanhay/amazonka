{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.CreateThingGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a thing group.
module Network.AWS.IoT.CreateThingGroup
    (
    -- * Creating a request
      CreateThingGroup (..)
    , mkCreateThingGroup
    -- ** Request lenses
    , ctgThingGroupName
    , ctgParentGroupName
    , ctgTags
    , ctgThingGroupProperties

    -- * Destructuring the response
    , CreateThingGroupResponse (..)
    , mkCreateThingGroupResponse
    -- ** Response lenses
    , ctgrrsThingGroupArn
    , ctgrrsThingGroupId
    , ctgrrsThingGroupName
    , ctgrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateThingGroup' smart constructor.
data CreateThingGroup = CreateThingGroup'
  { thingGroupName :: Types.ThingGroupName
    -- ^ The thing group name to create.
  , parentGroupName :: Core.Maybe Types.ParentGroupName
    -- ^ The name of the parent thing group.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Metadata which can be used to manage the thing group.
  , thingGroupProperties :: Core.Maybe Types.ThingGroupProperties
    -- ^ The thing group properties.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateThingGroup' value with any optional fields omitted.
mkCreateThingGroup
    :: Types.ThingGroupName -- ^ 'thingGroupName'
    -> CreateThingGroup
mkCreateThingGroup thingGroupName
  = CreateThingGroup'{thingGroupName, parentGroupName = Core.Nothing,
                      tags = Core.Nothing, thingGroupProperties = Core.Nothing}

-- | The thing group name to create.
--
-- /Note:/ Consider using 'thingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgThingGroupName :: Lens.Lens' CreateThingGroup Types.ThingGroupName
ctgThingGroupName = Lens.field @"thingGroupName"
{-# INLINEABLE ctgThingGroupName #-}
{-# DEPRECATED thingGroupName "Use generic-lens or generic-optics with 'thingGroupName' instead"  #-}

-- | The name of the parent thing group.
--
-- /Note:/ Consider using 'parentGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgParentGroupName :: Lens.Lens' CreateThingGroup (Core.Maybe Types.ParentGroupName)
ctgParentGroupName = Lens.field @"parentGroupName"
{-# INLINEABLE ctgParentGroupName #-}
{-# DEPRECATED parentGroupName "Use generic-lens or generic-optics with 'parentGroupName' instead"  #-}

-- | Metadata which can be used to manage the thing group.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgTags :: Lens.Lens' CreateThingGroup (Core.Maybe [Types.Tag])
ctgTags = Lens.field @"tags"
{-# INLINEABLE ctgTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The thing group properties.
--
-- /Note:/ Consider using 'thingGroupProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgThingGroupProperties :: Lens.Lens' CreateThingGroup (Core.Maybe Types.ThingGroupProperties)
ctgThingGroupProperties = Lens.field @"thingGroupProperties"
{-# INLINEABLE ctgThingGroupProperties #-}
{-# DEPRECATED thingGroupProperties "Use generic-lens or generic-optics with 'thingGroupProperties' instead"  #-}

instance Core.ToQuery CreateThingGroup where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateThingGroup where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON CreateThingGroup where
        toJSON CreateThingGroup{..}
          = Core.object
              (Core.catMaybes
                 [("parentGroupName" Core..=) Core.<$> parentGroupName,
                  ("tags" Core..=) Core.<$> tags,
                  ("thingGroupProperties" Core..=) Core.<$> thingGroupProperties])

instance Core.AWSRequest CreateThingGroup where
        type Rs CreateThingGroup = CreateThingGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/thing-groups/" Core.<> Core.toText thingGroupName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateThingGroupResponse' Core.<$>
                   (x Core..:? "thingGroupArn") Core.<*> x Core..:? "thingGroupId"
                     Core.<*> x Core..:? "thingGroupName"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateThingGroupResponse' smart constructor.
data CreateThingGroupResponse = CreateThingGroupResponse'
  { thingGroupArn :: Core.Maybe Types.ThingGroupArn
    -- ^ The thing group ARN.
  , thingGroupId :: Core.Maybe Types.ThingGroupId
    -- ^ The thing group ID.
  , thingGroupName :: Core.Maybe Types.ThingGroupName
    -- ^ The thing group name.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateThingGroupResponse' value with any optional fields omitted.
mkCreateThingGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateThingGroupResponse
mkCreateThingGroupResponse responseStatus
  = CreateThingGroupResponse'{thingGroupArn = Core.Nothing,
                              thingGroupId = Core.Nothing, thingGroupName = Core.Nothing,
                              responseStatus}

-- | The thing group ARN.
--
-- /Note:/ Consider using 'thingGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgrrsThingGroupArn :: Lens.Lens' CreateThingGroupResponse (Core.Maybe Types.ThingGroupArn)
ctgrrsThingGroupArn = Lens.field @"thingGroupArn"
{-# INLINEABLE ctgrrsThingGroupArn #-}
{-# DEPRECATED thingGroupArn "Use generic-lens or generic-optics with 'thingGroupArn' instead"  #-}

-- | The thing group ID.
--
-- /Note:/ Consider using 'thingGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgrrsThingGroupId :: Lens.Lens' CreateThingGroupResponse (Core.Maybe Types.ThingGroupId)
ctgrrsThingGroupId = Lens.field @"thingGroupId"
{-# INLINEABLE ctgrrsThingGroupId #-}
{-# DEPRECATED thingGroupId "Use generic-lens or generic-optics with 'thingGroupId' instead"  #-}

-- | The thing group name.
--
-- /Note:/ Consider using 'thingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgrrsThingGroupName :: Lens.Lens' CreateThingGroupResponse (Core.Maybe Types.ThingGroupName)
ctgrrsThingGroupName = Lens.field @"thingGroupName"
{-# INLINEABLE ctgrrsThingGroupName #-}
{-# DEPRECATED thingGroupName "Use generic-lens or generic-optics with 'thingGroupName' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgrrsResponseStatus :: Lens.Lens' CreateThingGroupResponse Core.Int
ctgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ctgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
