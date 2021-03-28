{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.UpdateProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a project in AWS CodeStar.
module Network.AWS.CodeStar.UpdateProject
    (
    -- * Creating a request
      UpdateProject (..)
    , mkUpdateProject
    -- ** Request lenses
    , upId
    , upDescription
    , upName

    -- * Destructuring the response
    , UpdateProjectResponse (..)
    , mkUpdateProjectResponse
    -- ** Response lenses
    , uprrsResponseStatus
    ) where

import qualified Network.AWS.CodeStar.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateProject' smart constructor.
data UpdateProject = UpdateProject'
  { id :: Types.Id
    -- ^ The ID of the project you want to update.
  , description :: Core.Maybe Types.Description
    -- ^ The description of the project, if any.
  , name :: Core.Maybe Types.ProjectName
    -- ^ The name of the project you want to update.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateProject' value with any optional fields omitted.
mkUpdateProject
    :: Types.Id -- ^ 'id'
    -> UpdateProject
mkUpdateProject id
  = UpdateProject'{id, description = Core.Nothing,
                   name = Core.Nothing}

-- | The ID of the project you want to update.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upId :: Lens.Lens' UpdateProject Types.Id
upId = Lens.field @"id"
{-# INLINEABLE upId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The description of the project, if any.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upDescription :: Lens.Lens' UpdateProject (Core.Maybe Types.Description)
upDescription = Lens.field @"description"
{-# INLINEABLE upDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The name of the project you want to update.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upName :: Lens.Lens' UpdateProject (Core.Maybe Types.ProjectName)
upName = Lens.field @"name"
{-# INLINEABLE upName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery UpdateProject where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateProject where
        toHeaders UpdateProject{..}
          = Core.pure ("X-Amz-Target", "CodeStar_20170419.UpdateProject")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateProject where
        toJSON UpdateProject{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("id" Core..= id),
                  ("description" Core..=) Core.<$> description,
                  ("name" Core..=) Core.<$> name])

instance Core.AWSRequest UpdateProject where
        type Rs UpdateProject = UpdateProjectResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UpdateProjectResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateProjectResponse' smart constructor.
newtype UpdateProjectResponse = UpdateProjectResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateProjectResponse' value with any optional fields omitted.
mkUpdateProjectResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateProjectResponse
mkUpdateProjectResponse responseStatus
  = UpdateProjectResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprrsResponseStatus :: Lens.Lens' UpdateProjectResponse Core.Int
uprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
