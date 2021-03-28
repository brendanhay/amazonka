{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.DescribeProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a project and its resources.
module Network.AWS.CodeStar.DescribeProject
    (
    -- * Creating a request
      DescribeProject (..)
    , mkDescribeProject
    -- ** Request lenses
    , dId

    -- * Destructuring the response
    , DescribeProjectResponse (..)
    , mkDescribeProjectResponse
    -- ** Response lenses
    , drsArn
    , drsClientRequestToken
    , drsCreatedTimeStamp
    , drsDescription
    , drsId
    , drsName
    , drsProjectTemplateId
    , drsStackId
    , drsStatus
    , drsResponseStatus
    ) where

import qualified Network.AWS.CodeStar.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeProject' smart constructor.
newtype DescribeProject = DescribeProject'
  { id :: Types.ProjectId
    -- ^ The ID of the project.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeProject' value with any optional fields omitted.
mkDescribeProject
    :: Types.ProjectId -- ^ 'id'
    -> DescribeProject
mkDescribeProject id = DescribeProject'{id}

-- | The ID of the project.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dId :: Lens.Lens' DescribeProject Types.ProjectId
dId = Lens.field @"id"
{-# INLINEABLE dId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.ToQuery DescribeProject where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeProject where
        toHeaders DescribeProject{..}
          = Core.pure ("X-Amz-Target", "CodeStar_20170419.DescribeProject")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeProject where
        toJSON DescribeProject{..}
          = Core.object (Core.catMaybes [Core.Just ("id" Core..= id)])

instance Core.AWSRequest DescribeProject where
        type Rs DescribeProject = DescribeProjectResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeProjectResponse' Core.<$>
                   (x Core..:? "arn") Core.<*> x Core..:? "clientRequestToken"
                     Core.<*> x Core..:? "createdTimeStamp"
                     Core.<*> x Core..:? "description"
                     Core.<*> x Core..:? "id"
                     Core.<*> x Core..:? "name"
                     Core.<*> x Core..:? "projectTemplateId"
                     Core.<*> x Core..:? "stackId"
                     Core.<*> x Core..:? "status"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeProjectResponse' smart constructor.
data DescribeProjectResponse = DescribeProjectResponse'
  { arn :: Core.Maybe Types.Arn
    -- ^ The Amazon Resource Name (ARN) for the project.
  , clientRequestToken :: Core.Maybe Types.ClientRequestToken
    -- ^ A user- or system-generated token that identifies the entity that requested project creation. 
  , createdTimeStamp :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time the project was created, in timestamp format.
  , description :: Core.Maybe Types.Description
    -- ^ The description of the project, if any.
  , id :: Core.Maybe Types.Id
    -- ^ The ID of the project.
  , name :: Core.Maybe Types.Name
    -- ^ The display name for the project.
  , projectTemplateId :: Core.Maybe Types.ProjectTemplateId
    -- ^ The ID for the AWS CodeStar project template used to create the project.
  , stackId :: Core.Maybe Types.StackId
    -- ^ The ID of the primary stack in AWS CloudFormation used to generate resources for the project.
  , status :: Core.Maybe Types.ProjectStatus
    -- ^ The project creation or deletion status.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeProjectResponse' value with any optional fields omitted.
mkDescribeProjectResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeProjectResponse
mkDescribeProjectResponse responseStatus
  = DescribeProjectResponse'{arn = Core.Nothing,
                             clientRequestToken = Core.Nothing, createdTimeStamp = Core.Nothing,
                             description = Core.Nothing, id = Core.Nothing, name = Core.Nothing,
                             projectTemplateId = Core.Nothing, stackId = Core.Nothing,
                             status = Core.Nothing, responseStatus}

-- | The Amazon Resource Name (ARN) for the project.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsArn :: Lens.Lens' DescribeProjectResponse (Core.Maybe Types.Arn)
drsArn = Lens.field @"arn"
{-# INLINEABLE drsArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | A user- or system-generated token that identifies the entity that requested project creation. 
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsClientRequestToken :: Lens.Lens' DescribeProjectResponse (Core.Maybe Types.ClientRequestToken)
drsClientRequestToken = Lens.field @"clientRequestToken"
{-# INLINEABLE drsClientRequestToken #-}
{-# DEPRECATED clientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead"  #-}

-- | The date and time the project was created, in timestamp format.
--
-- /Note:/ Consider using 'createdTimeStamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsCreatedTimeStamp :: Lens.Lens' DescribeProjectResponse (Core.Maybe Core.NominalDiffTime)
drsCreatedTimeStamp = Lens.field @"createdTimeStamp"
{-# INLINEABLE drsCreatedTimeStamp #-}
{-# DEPRECATED createdTimeStamp "Use generic-lens or generic-optics with 'createdTimeStamp' instead"  #-}

-- | The description of the project, if any.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsDescription :: Lens.Lens' DescribeProjectResponse (Core.Maybe Types.Description)
drsDescription = Lens.field @"description"
{-# INLINEABLE drsDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The ID of the project.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsId :: Lens.Lens' DescribeProjectResponse (Core.Maybe Types.Id)
drsId = Lens.field @"id"
{-# INLINEABLE drsId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The display name for the project.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsName :: Lens.Lens' DescribeProjectResponse (Core.Maybe Types.Name)
drsName = Lens.field @"name"
{-# INLINEABLE drsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The ID for the AWS CodeStar project template used to create the project.
--
-- /Note:/ Consider using 'projectTemplateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsProjectTemplateId :: Lens.Lens' DescribeProjectResponse (Core.Maybe Types.ProjectTemplateId)
drsProjectTemplateId = Lens.field @"projectTemplateId"
{-# INLINEABLE drsProjectTemplateId #-}
{-# DEPRECATED projectTemplateId "Use generic-lens or generic-optics with 'projectTemplateId' instead"  #-}

-- | The ID of the primary stack in AWS CloudFormation used to generate resources for the project.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsStackId :: Lens.Lens' DescribeProjectResponse (Core.Maybe Types.StackId)
drsStackId = Lens.field @"stackId"
{-# INLINEABLE drsStackId #-}
{-# DEPRECATED stackId "Use generic-lens or generic-optics with 'stackId' instead"  #-}

-- | The project creation or deletion status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsStatus :: Lens.Lens' DescribeProjectResponse (Core.Maybe Types.ProjectStatus)
drsStatus = Lens.field @"status"
{-# INLINEABLE drsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeProjectResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
