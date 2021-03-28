{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.CreateSkillGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a skill group with a specified name and description.
module Network.AWS.AlexaBusiness.CreateSkillGroup
    (
    -- * Creating a request
      CreateSkillGroup (..)
    , mkCreateSkillGroup
    -- ** Request lenses
    , csgSkillGroupName
    , csgClientRequestToken
    , csgDescription
    , csgTags

    -- * Destructuring the response
    , CreateSkillGroupResponse (..)
    , mkCreateSkillGroupResponse
    -- ** Response lenses
    , csgrrsSkillGroupArn
    , csgrrsResponseStatus
    ) where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateSkillGroup' smart constructor.
data CreateSkillGroup = CreateSkillGroup'
  { skillGroupName :: Types.SkillGroupName
    -- ^ The name for the skill group.
  , clientRequestToken :: Core.Maybe Types.ClientRequestToken
    -- ^ A unique, user-specified identifier for this request that ensures idempotency. 
  , description :: Core.Maybe Types.SkillGroupDescription
    -- ^ The description for the skill group.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tags for the skill group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSkillGroup' value with any optional fields omitted.
mkCreateSkillGroup
    :: Types.SkillGroupName -- ^ 'skillGroupName'
    -> CreateSkillGroup
mkCreateSkillGroup skillGroupName
  = CreateSkillGroup'{skillGroupName,
                      clientRequestToken = Core.Nothing, description = Core.Nothing,
                      tags = Core.Nothing}

-- | The name for the skill group.
--
-- /Note:/ Consider using 'skillGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgSkillGroupName :: Lens.Lens' CreateSkillGroup Types.SkillGroupName
csgSkillGroupName = Lens.field @"skillGroupName"
{-# INLINEABLE csgSkillGroupName #-}
{-# DEPRECATED skillGroupName "Use generic-lens or generic-optics with 'skillGroupName' instead"  #-}

-- | A unique, user-specified identifier for this request that ensures idempotency. 
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgClientRequestToken :: Lens.Lens' CreateSkillGroup (Core.Maybe Types.ClientRequestToken)
csgClientRequestToken = Lens.field @"clientRequestToken"
{-# INLINEABLE csgClientRequestToken #-}
{-# DEPRECATED clientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead"  #-}

-- | The description for the skill group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgDescription :: Lens.Lens' CreateSkillGroup (Core.Maybe Types.SkillGroupDescription)
csgDescription = Lens.field @"description"
{-# INLINEABLE csgDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The tags for the skill group.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgTags :: Lens.Lens' CreateSkillGroup (Core.Maybe [Types.Tag])
csgTags = Lens.field @"tags"
{-# INLINEABLE csgTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateSkillGroup where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateSkillGroup where
        toHeaders CreateSkillGroup{..}
          = Core.pure ("X-Amz-Target", "AlexaForBusiness.CreateSkillGroup")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateSkillGroup where
        toJSON CreateSkillGroup{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("SkillGroupName" Core..= skillGroupName),
                  ("ClientRequestToken" Core..=) Core.<$> clientRequestToken,
                  ("Description" Core..=) Core.<$> description,
                  ("Tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateSkillGroup where
        type Rs CreateSkillGroup = CreateSkillGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateSkillGroupResponse' Core.<$>
                   (x Core..:? "SkillGroupArn") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateSkillGroupResponse' smart constructor.
data CreateSkillGroupResponse = CreateSkillGroupResponse'
  { skillGroupArn :: Core.Maybe Types.Arn
    -- ^ The ARN of the newly created skill group in the response.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSkillGroupResponse' value with any optional fields omitted.
mkCreateSkillGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateSkillGroupResponse
mkCreateSkillGroupResponse responseStatus
  = CreateSkillGroupResponse'{skillGroupArn = Core.Nothing,
                              responseStatus}

-- | The ARN of the newly created skill group in the response.
--
-- /Note:/ Consider using 'skillGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgrrsSkillGroupArn :: Lens.Lens' CreateSkillGroupResponse (Core.Maybe Types.Arn)
csgrrsSkillGroupArn = Lens.field @"skillGroupArn"
{-# INLINEABLE csgrrsSkillGroupArn #-}
{-# DEPRECATED skillGroupArn "Use generic-lens or generic-optics with 'skillGroupArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgrrsResponseStatus :: Lens.Lens' CreateSkillGroupResponse Core.Int
csgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE csgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
