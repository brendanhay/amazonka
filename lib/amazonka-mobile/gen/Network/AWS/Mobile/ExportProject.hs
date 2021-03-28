{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Mobile.ExportProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Exports project configuration to a snapshot which can be downloaded and shared. Note that mobile app push credentials are encrypted in exported projects, so they can only be shared successfully within the same AWS account. 
module Network.AWS.Mobile.ExportProject
    (
    -- * Creating a request
      ExportProject (..)
    , mkExportProject
    -- ** Request lenses
    , epProjectId

    -- * Destructuring the response
    , ExportProjectResponse (..)
    , mkExportProjectResponse
    -- ** Response lenses
    , eprrsDownloadUrl
    , eprrsShareUrl
    , eprrsSnapshotId
    , eprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Mobile.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request structure used in requests to export project configuration details. 
--
-- /See:/ 'mkExportProject' smart constructor.
newtype ExportProject = ExportProject'
  { projectId :: Types.ProjectId
    -- ^ Unique project identifier. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ExportProject' value with any optional fields omitted.
mkExportProject
    :: Types.ProjectId -- ^ 'projectId'
    -> ExportProject
mkExportProject projectId = ExportProject'{projectId}

-- | Unique project identifier. 
--
-- /Note:/ Consider using 'projectId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epProjectId :: Lens.Lens' ExportProject Types.ProjectId
epProjectId = Lens.field @"projectId"
{-# INLINEABLE epProjectId #-}
{-# DEPRECATED projectId "Use generic-lens or generic-optics with 'projectId' instead"  #-}

instance Core.ToQuery ExportProject where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ExportProject where
        toHeaders ExportProject{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ExportProject where
        toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest ExportProject where
        type Rs ExportProject = ExportProjectResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/exports/" Core.<> Core.toText projectId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ExportProjectResponse' Core.<$>
                   (x Core..:? "downloadUrl") Core.<*> x Core..:? "shareUrl" Core.<*>
                     x Core..:? "snapshotId"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Result structure used for requests to export project configuration details. 
--
-- /See:/ 'mkExportProjectResponse' smart constructor.
data ExportProjectResponse = ExportProjectResponse'
  { downloadUrl :: Core.Maybe Types.DownloadUrl
    -- ^ URL which can be used to download the exported project configuation file(s). 
  , shareUrl :: Core.Maybe Types.ShareUrl
    -- ^ URL which can be shared to allow other AWS users to create their own project in AWS Mobile Hub with the same configuration as the specified project. This URL pertains to a snapshot in time of the project configuration that is created when this API is called. If you want to share additional changes to your project configuration, then you will need to create and share a new snapshot by calling this method again. 
  , snapshotId :: Core.Maybe Types.SnapshotId
    -- ^ Unique identifier for the exported snapshot of the project configuration. This snapshot identifier is included in the share URL. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExportProjectResponse' value with any optional fields omitted.
mkExportProjectResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ExportProjectResponse
mkExportProjectResponse responseStatus
  = ExportProjectResponse'{downloadUrl = Core.Nothing,
                           shareUrl = Core.Nothing, snapshotId = Core.Nothing, responseStatus}

-- | URL which can be used to download the exported project configuation file(s). 
--
-- /Note:/ Consider using 'downloadUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eprrsDownloadUrl :: Lens.Lens' ExportProjectResponse (Core.Maybe Types.DownloadUrl)
eprrsDownloadUrl = Lens.field @"downloadUrl"
{-# INLINEABLE eprrsDownloadUrl #-}
{-# DEPRECATED downloadUrl "Use generic-lens or generic-optics with 'downloadUrl' instead"  #-}

-- | URL which can be shared to allow other AWS users to create their own project in AWS Mobile Hub with the same configuration as the specified project. This URL pertains to a snapshot in time of the project configuration that is created when this API is called. If you want to share additional changes to your project configuration, then you will need to create and share a new snapshot by calling this method again. 
--
-- /Note:/ Consider using 'shareUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eprrsShareUrl :: Lens.Lens' ExportProjectResponse (Core.Maybe Types.ShareUrl)
eprrsShareUrl = Lens.field @"shareUrl"
{-# INLINEABLE eprrsShareUrl #-}
{-# DEPRECATED shareUrl "Use generic-lens or generic-optics with 'shareUrl' instead"  #-}

-- | Unique identifier for the exported snapshot of the project configuration. This snapshot identifier is included in the share URL. 
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eprrsSnapshotId :: Lens.Lens' ExportProjectResponse (Core.Maybe Types.SnapshotId)
eprrsSnapshotId = Lens.field @"snapshotId"
{-# INLINEABLE eprrsSnapshotId #-}
{-# DEPRECATED snapshotId "Use generic-lens or generic-optics with 'snapshotId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eprrsResponseStatus :: Lens.Lens' ExportProjectResponse Core.Int
eprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE eprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
