{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Mobile.CreateProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS Mobile Hub project.
module Network.AWS.Mobile.CreateProject
  ( -- * Creating a request
    CreateProject (..),
    mkCreateProject,

    -- ** Request lenses
    cpContents,
    cpName,
    cpRegion,
    cpSnapshotId,

    -- * Destructuring the response
    CreateProjectResponse (..),
    mkCreateProjectResponse,

    -- ** Response lenses
    cprrsDetails,
    cprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Mobile.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request structure used to request a project be created.
--
-- /See:/ 'mkCreateProject' smart constructor.
data CreateProject = CreateProject'
  { -- | ZIP or YAML file which contains configuration settings to be used when creating the project. This may be the contents of the file downloaded from the URL provided in an export project operation.
    contents :: Core.Maybe Core.ByteString,
    -- | Name of the project.
    name :: Core.Maybe Types.ProjectName,
    -- | Default region where project resources should be created.
    region :: Core.Maybe Types.ProjectRegion,
    -- | Unique identifier for an exported snapshot of project configuration. This snapshot identifier is included in the share URL when a project is exported.
    snapshotId :: Core.Maybe Types.SnapshotId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateProject' value with any optional fields omitted.
mkCreateProject ::
  CreateProject
mkCreateProject =
  CreateProject'
    { contents = Core.Nothing,
      name = Core.Nothing,
      region = Core.Nothing,
      snapshotId = Core.Nothing
    }

-- | ZIP or YAML file which contains configuration settings to be used when creating the project. This may be the contents of the file downloaded from the URL provided in an export project operation.
--
-- /Note:/ Consider using 'contents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpContents :: Lens.Lens' CreateProject (Core.Maybe Core.ByteString)
cpContents = Lens.field @"contents"
{-# DEPRECATED cpContents "Use generic-lens or generic-optics with 'contents' instead." #-}

-- | Name of the project.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpName :: Lens.Lens' CreateProject (Core.Maybe Types.ProjectName)
cpName = Lens.field @"name"
{-# DEPRECATED cpName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Default region where project resources should be created.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpRegion :: Lens.Lens' CreateProject (Core.Maybe Types.ProjectRegion)
cpRegion = Lens.field @"region"
{-# DEPRECATED cpRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | Unique identifier for an exported snapshot of project configuration. This snapshot identifier is included in the share URL when a project is exported.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpSnapshotId :: Lens.Lens' CreateProject (Core.Maybe Types.SnapshotId)
cpSnapshotId = Lens.field @"snapshotId"
{-# DEPRECATED cpSnapshotId "Use generic-lens or generic-optics with 'snapshotId' instead." #-}

instance Core.AWSRequest CreateProject where
  type Rs CreateProject = CreateProjectResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/projects",
        Core._rqQuery =
          Core.toQueryValue "name" Core.<$> name
            Core.<> (Core.toQueryValue "region" Core.<$> region)
            Core.<> (Core.toQueryValue "snapshotId" Core.<$> snapshotId),
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toBody contents
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateProjectResponse'
            Core.<$> (x Core..:? "details") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Result structure used in response to a request to create a project.
--
-- /See:/ 'mkCreateProjectResponse' smart constructor.
data CreateProjectResponse = CreateProjectResponse'
  { -- | Detailed information about the created AWS Mobile Hub project.
    details :: Core.Maybe Types.ProjectDetails,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateProjectResponse' value with any optional fields omitted.
mkCreateProjectResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateProjectResponse
mkCreateProjectResponse responseStatus =
  CreateProjectResponse' {details = Core.Nothing, responseStatus}

-- | Detailed information about the created AWS Mobile Hub project.
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrsDetails :: Lens.Lens' CreateProjectResponse (Core.Maybe Types.ProjectDetails)
cprrsDetails = Lens.field @"details"
{-# DEPRECATED cprrsDetails "Use generic-lens or generic-optics with 'details' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrsResponseStatus :: Lens.Lens' CreateProjectResponse Core.Int
cprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
