{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Mobile.UpdateProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update an existing project. 
module Network.AWS.Mobile.UpdateProject
    (
    -- * Creating a request
      UpdateProject (..)
    , mkUpdateProject
    -- ** Request lenses
    , upProjectId
    , upContents

    -- * Destructuring the response
    , UpdateProjectResponse (..)
    , mkUpdateProjectResponse
    -- ** Response lenses
    , uprrsDetails
    , uprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Mobile.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request structure used for requests to update project configuration. 
--
-- /See:/ 'mkUpdateProject' smart constructor.
data UpdateProject = UpdateProject'
  { projectId :: Types.ProjectId
    -- ^ Unique project identifier. 
  , contents :: Core.Maybe Core.ByteString
    -- ^ ZIP or YAML file which contains project configuration to be updated. This should be the contents of the file downloaded from the URL provided in an export project operation. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateProject' value with any optional fields omitted.
mkUpdateProject
    :: Types.ProjectId -- ^ 'projectId'
    -> UpdateProject
mkUpdateProject projectId
  = UpdateProject'{projectId, contents = Core.Nothing}

-- | Unique project identifier. 
--
-- /Note:/ Consider using 'projectId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upProjectId :: Lens.Lens' UpdateProject Types.ProjectId
upProjectId = Lens.field @"projectId"
{-# INLINEABLE upProjectId #-}
{-# DEPRECATED projectId "Use generic-lens or generic-optics with 'projectId' instead"  #-}

-- | ZIP or YAML file which contains project configuration to be updated. This should be the contents of the file downloaded from the URL provided in an export project operation. 
--
-- /Note:/ Consider using 'contents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upContents :: Lens.Lens' UpdateProject (Core.Maybe Core.ByteString)
upContents = Lens.field @"contents"
{-# INLINEABLE upContents #-}
{-# DEPRECATED contents "Use generic-lens or generic-optics with 'contents' instead"  #-}

instance Core.ToQuery UpdateProject where
        toQuery UpdateProject{..} = Core.toQueryPair "projectId" projectId

instance Core.ToHeaders UpdateProject where
        toHeaders UpdateProject{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest UpdateProject where
        type Rs UpdateProject = UpdateProjectResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/update",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toBody contents}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateProjectResponse' Core.<$>
                   (x Core..:? "details") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Result structure used for requests to updated project configuration. 
--
-- /See:/ 'mkUpdateProjectResponse' smart constructor.
data UpdateProjectResponse = UpdateProjectResponse'
  { details :: Core.Maybe Types.ProjectDetails
    -- ^ Detailed information about the updated AWS Mobile Hub project. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UpdateProjectResponse' value with any optional fields omitted.
mkUpdateProjectResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateProjectResponse
mkUpdateProjectResponse responseStatus
  = UpdateProjectResponse'{details = Core.Nothing, responseStatus}

-- | Detailed information about the updated AWS Mobile Hub project. 
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprrsDetails :: Lens.Lens' UpdateProjectResponse (Core.Maybe Types.ProjectDetails)
uprrsDetails = Lens.field @"details"
{-# INLINEABLE uprrsDetails #-}
{-# DEPRECATED details "Use generic-lens or generic-optics with 'details' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprrsResponseStatus :: Lens.Lens' UpdateProjectResponse Core.Int
uprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
