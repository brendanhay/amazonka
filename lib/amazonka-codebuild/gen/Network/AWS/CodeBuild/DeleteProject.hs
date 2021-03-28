{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.DeleteProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a build project. When you delete a project, its builds are not deleted. 
module Network.AWS.CodeBuild.DeleteProject
    (
    -- * Creating a request
      DeleteProject (..)
    , mkDeleteProject
    -- ** Request lenses
    , dpName

    -- * Destructuring the response
    , DeleteProjectResponse (..)
    , mkDeleteProjectResponse
    -- ** Response lenses
    , dprrsResponseStatus
    ) where

import qualified Network.AWS.CodeBuild.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteProject' smart constructor.
newtype DeleteProject = DeleteProject'
  { name :: Types.NonEmptyString
    -- ^ The name of the build project.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteProject' value with any optional fields omitted.
mkDeleteProject
    :: Types.NonEmptyString -- ^ 'name'
    -> DeleteProject
mkDeleteProject name = DeleteProject'{name}

-- | The name of the build project.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpName :: Lens.Lens' DeleteProject Types.NonEmptyString
dpName = Lens.field @"name"
{-# INLINEABLE dpName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery DeleteProject where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteProject where
        toHeaders DeleteProject{..}
          = Core.pure ("X-Amz-Target", "CodeBuild_20161006.DeleteProject")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteProject where
        toJSON DeleteProject{..}
          = Core.object (Core.catMaybes [Core.Just ("name" Core..= name)])

instance Core.AWSRequest DeleteProject where
        type Rs DeleteProject = DeleteProjectResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteProjectResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteProjectResponse' smart constructor.
newtype DeleteProjectResponse = DeleteProjectResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteProjectResponse' value with any optional fields omitted.
mkDeleteProjectResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteProjectResponse
mkDeleteProjectResponse responseStatus
  = DeleteProjectResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprrsResponseStatus :: Lens.Lens' DeleteProjectResponse Core.Int
dprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
