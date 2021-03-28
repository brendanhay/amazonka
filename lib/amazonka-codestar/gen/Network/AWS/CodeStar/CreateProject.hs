{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.CreateProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a project, including project resources. This action creates a project based on a submitted project request. A set of source code files and a toolchain template file can be included with the project request. If these are not provided, an empty project is created.
module Network.AWS.CodeStar.CreateProject
    (
    -- * Creating a request
      CreateProject (..)
    , mkCreateProject
    -- ** Request lenses
    , cpName
    , cpId
    , cpClientRequestToken
    , cpDescription
    , cpSourceCode
    , cpTags
    , cpToolchain

    -- * Destructuring the response
    , CreateProjectResponse (..)
    , mkCreateProjectResponse
    -- ** Response lenses
    , cprrsId
    , cprrsArn
    , cprrsClientRequestToken
    , cprrsProjectTemplateId
    , cprrsResponseStatus
    ) where

import qualified Network.AWS.CodeStar.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateProject' smart constructor.
data CreateProject = CreateProject'
  { name :: Types.ProjectName
    -- ^ The display name for the project to be created in AWS CodeStar.
  , id :: Types.ProjectId
    -- ^ The ID of the project to be created in AWS CodeStar.
  , clientRequestToken :: Core.Maybe Types.ClientRequestToken
    -- ^ A user- or system-generated token that identifies the entity that requested project creation. This token can be used to repeat the request.
  , description :: Core.Maybe Types.Description
    -- ^ The description of the project, if any.
  , sourceCode :: Core.Maybe [Types.Code]
    -- ^ A list of the Code objects submitted with the project request. If this parameter is specified, the request must also include the toolchain parameter.
  , tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue)
    -- ^ The tags created for the project.
  , toolchain :: Core.Maybe Types.Toolchain
    -- ^ The name of the toolchain template file submitted with the project request. If this parameter is specified, the request must also include the sourceCode parameter.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateProject' value with any optional fields omitted.
mkCreateProject
    :: Types.ProjectName -- ^ 'name'
    -> Types.ProjectId -- ^ 'id'
    -> CreateProject
mkCreateProject name id
  = CreateProject'{name, id, clientRequestToken = Core.Nothing,
                   description = Core.Nothing, sourceCode = Core.Nothing,
                   tags = Core.Nothing, toolchain = Core.Nothing}

-- | The display name for the project to be created in AWS CodeStar.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpName :: Lens.Lens' CreateProject Types.ProjectName
cpName = Lens.field @"name"
{-# INLINEABLE cpName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The ID of the project to be created in AWS CodeStar.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpId :: Lens.Lens' CreateProject Types.ProjectId
cpId = Lens.field @"id"
{-# INLINEABLE cpId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | A user- or system-generated token that identifies the entity that requested project creation. This token can be used to repeat the request.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpClientRequestToken :: Lens.Lens' CreateProject (Core.Maybe Types.ClientRequestToken)
cpClientRequestToken = Lens.field @"clientRequestToken"
{-# INLINEABLE cpClientRequestToken #-}
{-# DEPRECATED clientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead"  #-}

-- | The description of the project, if any.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpDescription :: Lens.Lens' CreateProject (Core.Maybe Types.Description)
cpDescription = Lens.field @"description"
{-# INLINEABLE cpDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | A list of the Code objects submitted with the project request. If this parameter is specified, the request must also include the toolchain parameter.
--
-- /Note:/ Consider using 'sourceCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpSourceCode :: Lens.Lens' CreateProject (Core.Maybe [Types.Code])
cpSourceCode = Lens.field @"sourceCode"
{-# INLINEABLE cpSourceCode #-}
{-# DEPRECATED sourceCode "Use generic-lens or generic-optics with 'sourceCode' instead"  #-}

-- | The tags created for the project.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpTags :: Lens.Lens' CreateProject (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
cpTags = Lens.field @"tags"
{-# INLINEABLE cpTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The name of the toolchain template file submitted with the project request. If this parameter is specified, the request must also include the sourceCode parameter.
--
-- /Note:/ Consider using 'toolchain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpToolchain :: Lens.Lens' CreateProject (Core.Maybe Types.Toolchain)
cpToolchain = Lens.field @"toolchain"
{-# INLINEABLE cpToolchain #-}
{-# DEPRECATED toolchain "Use generic-lens or generic-optics with 'toolchain' instead"  #-}

instance Core.ToQuery CreateProject where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateProject where
        toHeaders CreateProject{..}
          = Core.pure ("X-Amz-Target", "CodeStar_20170419.CreateProject")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateProject where
        toJSON CreateProject{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("name" Core..= name), Core.Just ("id" Core..= id),
                  ("clientRequestToken" Core..=) Core.<$> clientRequestToken,
                  ("description" Core..=) Core.<$> description,
                  ("sourceCode" Core..=) Core.<$> sourceCode,
                  ("tags" Core..=) Core.<$> tags,
                  ("toolchain" Core..=) Core.<$> toolchain])

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
                   (x Core..: "id") Core.<*> x Core..: "arn" Core.<*>
                     x Core..:? "clientRequestToken"
                     Core.<*> x Core..:? "projectTemplateId"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateProjectResponse' smart constructor.
data CreateProjectResponse = CreateProjectResponse'
  { id :: Types.ProjectId
    -- ^ The ID of the project.
  , arn :: Types.ProjectArn
    -- ^ The Amazon Resource Name (ARN) of the created project.
  , clientRequestToken :: Core.Maybe Types.ClientRequestToken
    -- ^ A user- or system-generated token that identifies the entity that requested project creation.
  , projectTemplateId :: Core.Maybe Types.ProjectTemplateId
    -- ^ Reserved for future use.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateProjectResponse' value with any optional fields omitted.
mkCreateProjectResponse
    :: Types.ProjectId -- ^ 'id'
    -> Types.ProjectArn -- ^ 'arn'
    -> Core.Int -- ^ 'responseStatus'
    -> CreateProjectResponse
mkCreateProjectResponse id arn responseStatus
  = CreateProjectResponse'{id, arn,
                           clientRequestToken = Core.Nothing,
                           projectTemplateId = Core.Nothing, responseStatus}

-- | The ID of the project.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrsId :: Lens.Lens' CreateProjectResponse Types.ProjectId
cprrsId = Lens.field @"id"
{-# INLINEABLE cprrsId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The Amazon Resource Name (ARN) of the created project.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrsArn :: Lens.Lens' CreateProjectResponse Types.ProjectArn
cprrsArn = Lens.field @"arn"
{-# INLINEABLE cprrsArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | A user- or system-generated token that identifies the entity that requested project creation.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrsClientRequestToken :: Lens.Lens' CreateProjectResponse (Core.Maybe Types.ClientRequestToken)
cprrsClientRequestToken = Lens.field @"clientRequestToken"
{-# INLINEABLE cprrsClientRequestToken #-}
{-# DEPRECATED clientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead"  #-}

-- | Reserved for future use.
--
-- /Note:/ Consider using 'projectTemplateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrsProjectTemplateId :: Lens.Lens' CreateProjectResponse (Core.Maybe Types.ProjectTemplateId)
cprrsProjectTemplateId = Lens.field @"projectTemplateId"
{-# INLINEABLE cprrsProjectTemplateId #-}
{-# DEPRECATED projectTemplateId "Use generic-lens or generic-optics with 'projectTemplateId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrsResponseStatus :: Lens.Lens' CreateProjectResponse Core.Int
cprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
