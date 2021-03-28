{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.CreateProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amazon Rekognition Custom Labels project. A project is a logical grouping of resources (images, Labels, models) and operations (training, evaluation and detection). 
--
-- This operation requires permissions to perform the @rekognition:CreateProject@ action.
module Network.AWS.Rekognition.CreateProject
    (
    -- * Creating a request
      CreateProject (..)
    , mkCreateProject
    -- ** Request lenses
    , cpProjectName

    -- * Destructuring the response
    , CreateProjectResponse (..)
    , mkCreateProjectResponse
    -- ** Response lenses
    , cprrsProjectArn
    , cprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateProject' smart constructor.
newtype CreateProject = CreateProject'
  { projectName :: Types.ProjectName
    -- ^ The name of the project to create.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateProject' value with any optional fields omitted.
mkCreateProject
    :: Types.ProjectName -- ^ 'projectName'
    -> CreateProject
mkCreateProject projectName = CreateProject'{projectName}

-- | The name of the project to create.
--
-- /Note:/ Consider using 'projectName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpProjectName :: Lens.Lens' CreateProject Types.ProjectName
cpProjectName = Lens.field @"projectName"
{-# INLINEABLE cpProjectName #-}
{-# DEPRECATED projectName "Use generic-lens or generic-optics with 'projectName' instead"  #-}

instance Core.ToQuery CreateProject where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateProject where
        toHeaders CreateProject{..}
          = Core.pure ("X-Amz-Target", "RekognitionService.CreateProject")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateProject where
        toJSON CreateProject{..}
          = Core.object
              (Core.catMaybes [Core.Just ("ProjectName" Core..= projectName)])

instance Core.AWSRequest CreateProject where
        type Rs CreateProject = CreateProjectResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateProjectResponse' Core.<$>
                   (x Core..:? "ProjectArn") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateProjectResponse' smart constructor.
data CreateProjectResponse = CreateProjectResponse'
  { projectArn :: Core.Maybe Types.ProjectArn
    -- ^ The Amazon Resource Name (ARN) of the new project. You can use the ARN to configure IAM access to the project. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateProjectResponse' value with any optional fields omitted.
mkCreateProjectResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateProjectResponse
mkCreateProjectResponse responseStatus
  = CreateProjectResponse'{projectArn = Core.Nothing, responseStatus}

-- | The Amazon Resource Name (ARN) of the new project. You can use the ARN to configure IAM access to the project. 
--
-- /Note:/ Consider using 'projectArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrsProjectArn :: Lens.Lens' CreateProjectResponse (Core.Maybe Types.ProjectArn)
cprrsProjectArn = Lens.field @"projectArn"
{-# INLINEABLE cprrsProjectArn #-}
{-# DEPRECATED projectArn "Use generic-lens or generic-optics with 'projectArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrsResponseStatus :: Lens.Lens' CreateProjectResponse Core.Int
cprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
