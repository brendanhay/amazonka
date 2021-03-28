{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateProject (..)
    , mkCreateProject
    -- ** Request lenses
    , cpContents
    , cpName
    , cpRegion
    , cpSnapshotId

    -- * Destructuring the response
    , CreateProjectResponse (..)
    , mkCreateProjectResponse
    -- ** Response lenses
    , cprrsDetails
    , cprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Mobile.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request structure used to request a project be created. 
--
-- /See:/ 'mkCreateProject' smart constructor.
data CreateProject = CreateProject'
  { contents :: Core.Maybe Core.ByteString
    -- ^ ZIP or YAML file which contains configuration settings to be used when creating the project. This may be the contents of the file downloaded from the URL provided in an export project operation. 
  , name :: Core.Maybe Types.ProjectName
    -- ^ Name of the project. 
  , region :: Core.Maybe Types.ProjectRegion
    -- ^ Default region where project resources should be created. 
  , snapshotId :: Core.Maybe Types.SnapshotId
    -- ^ Unique identifier for an exported snapshot of project configuration. This snapshot identifier is included in the share URL when a project is exported. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateProject' value with any optional fields omitted.
mkCreateProject
    :: CreateProject
mkCreateProject
  = CreateProject'{contents = Core.Nothing, name = Core.Nothing,
                   region = Core.Nothing, snapshotId = Core.Nothing}

-- | ZIP or YAML file which contains configuration settings to be used when creating the project. This may be the contents of the file downloaded from the URL provided in an export project operation. 
--
-- /Note:/ Consider using 'contents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpContents :: Lens.Lens' CreateProject (Core.Maybe Core.ByteString)
cpContents = Lens.field @"contents"
{-# INLINEABLE cpContents #-}
{-# DEPRECATED contents "Use generic-lens or generic-optics with 'contents' instead"  #-}

-- | Name of the project. 
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpName :: Lens.Lens' CreateProject (Core.Maybe Types.ProjectName)
cpName = Lens.field @"name"
{-# INLINEABLE cpName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Default region where project resources should be created. 
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpRegion :: Lens.Lens' CreateProject (Core.Maybe Types.ProjectRegion)
cpRegion = Lens.field @"region"
{-# INLINEABLE cpRegion #-}
{-# DEPRECATED region "Use generic-lens or generic-optics with 'region' instead"  #-}

-- | Unique identifier for an exported snapshot of project configuration. This snapshot identifier is included in the share URL when a project is exported. 
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpSnapshotId :: Lens.Lens' CreateProject (Core.Maybe Types.SnapshotId)
cpSnapshotId = Lens.field @"snapshotId"
{-# INLINEABLE cpSnapshotId #-}
{-# DEPRECATED snapshotId "Use generic-lens or generic-optics with 'snapshotId' instead"  #-}

instance Core.ToQuery CreateProject where
        toQuery CreateProject{..}
          = Core.maybe Core.mempty (Core.toQueryPair "name") name Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "region") region
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "snapshotId") snapshotId

instance Core.ToHeaders CreateProject where
        toHeaders CreateProject{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest CreateProject where
        type Rs CreateProject = CreateProjectResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/projects",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toBody contents}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateProjectResponse' Core.<$>
                   (x Core..:? "details") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Result structure used in response to a request to create a project. 
--
-- /See:/ 'mkCreateProjectResponse' smart constructor.
data CreateProjectResponse = CreateProjectResponse'
  { details :: Core.Maybe Types.ProjectDetails
    -- ^ Detailed information about the created AWS Mobile Hub project. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateProjectResponse' value with any optional fields omitted.
mkCreateProjectResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateProjectResponse
mkCreateProjectResponse responseStatus
  = CreateProjectResponse'{details = Core.Nothing, responseStatus}

-- | Detailed information about the created AWS Mobile Hub project. 
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrsDetails :: Lens.Lens' CreateProjectResponse (Core.Maybe Types.ProjectDetails)
cprrsDetails = Lens.field @"details"
{-# INLINEABLE cprrsDetails #-}
{-# DEPRECATED details "Use generic-lens or generic-optics with 'details' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrsResponseStatus :: Lens.Lens' CreateProjectResponse Core.Int
cprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
