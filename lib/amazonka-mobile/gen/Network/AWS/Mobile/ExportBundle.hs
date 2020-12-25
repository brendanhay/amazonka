{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Mobile.ExportBundle
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates customized software development kit (SDK) and or tool packages used to integrate mobile web or mobile app clients with backend AWS resources.
module Network.AWS.Mobile.ExportBundle
  ( -- * Creating a request
    ExportBundle (..),
    mkExportBundle,

    -- ** Request lenses
    ebBundleId,
    ebPlatform,
    ebProjectId,

    -- * Destructuring the response
    ExportBundleResponse (..),
    mkExportBundleResponse,

    -- ** Response lenses
    ebrrsDownloadUrl,
    ebrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Mobile.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request structure used to request generation of custom SDK and tool packages required to integrate mobile web or app clients with backed AWS resources.
--
-- /See:/ 'mkExportBundle' smart constructor.
data ExportBundle = ExportBundle'
  { -- | Unique bundle identifier.
    bundleId :: Types.BundleId,
    -- | Developer desktop or target application platform.
    platform :: Core.Maybe Types.Platform,
    -- | Unique project identifier.
    projectId :: Core.Maybe Types.ProjectId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExportBundle' value with any optional fields omitted.
mkExportBundle ::
  -- | 'bundleId'
  Types.BundleId ->
  ExportBundle
mkExportBundle bundleId =
  ExportBundle'
    { bundleId,
      platform = Core.Nothing,
      projectId = Core.Nothing
    }

-- | Unique bundle identifier.
--
-- /Note:/ Consider using 'bundleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebBundleId :: Lens.Lens' ExportBundle Types.BundleId
ebBundleId = Lens.field @"bundleId"
{-# DEPRECATED ebBundleId "Use generic-lens or generic-optics with 'bundleId' instead." #-}

-- | Developer desktop or target application platform.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebPlatform :: Lens.Lens' ExportBundle (Core.Maybe Types.Platform)
ebPlatform = Lens.field @"platform"
{-# DEPRECATED ebPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

-- | Unique project identifier.
--
-- /Note:/ Consider using 'projectId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebProjectId :: Lens.Lens' ExportBundle (Core.Maybe Types.ProjectId)
ebProjectId = Lens.field @"projectId"
{-# DEPRECATED ebProjectId "Use generic-lens or generic-optics with 'projectId' instead." #-}

instance Core.FromJSON ExportBundle where
  toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest ExportBundle where
  type Rs ExportBundle = ExportBundleResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath ("/bundles/" Core.<> (Core.toText bundleId)),
        Core._rqQuery =
          Core.toQueryValue "platform" Core.<$> platform
            Core.<> (Core.toQueryValue "projectId" Core.<$> projectId),
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ExportBundleResponse'
            Core.<$> (x Core..:? "downloadUrl") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Result structure which contains link to download custom-generated SDK and tool packages used to integrate mobile web or app clients with backed AWS resources.
--
-- /See:/ 'mkExportBundleResponse' smart constructor.
data ExportBundleResponse = ExportBundleResponse'
  { -- | URL which contains the custom-generated SDK and tool packages used to integrate the client mobile app or web app with the AWS resources created by the AWS Mobile Hub project.
    downloadUrl :: Core.Maybe Types.DownloadUrl,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExportBundleResponse' value with any optional fields omitted.
mkExportBundleResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ExportBundleResponse
mkExportBundleResponse responseStatus =
  ExportBundleResponse' {downloadUrl = Core.Nothing, responseStatus}

-- | URL which contains the custom-generated SDK and tool packages used to integrate the client mobile app or web app with the AWS resources created by the AWS Mobile Hub project.
--
-- /Note:/ Consider using 'downloadUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebrrsDownloadUrl :: Lens.Lens' ExportBundleResponse (Core.Maybe Types.DownloadUrl)
ebrrsDownloadUrl = Lens.field @"downloadUrl"
{-# DEPRECATED ebrrsDownloadUrl "Use generic-lens or generic-optics with 'downloadUrl' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebrrsResponseStatus :: Lens.Lens' ExportBundleResponse Core.Int
ebrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ebrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
