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
    ebPlatform,
    ebBundleId,
    ebProjectId,

    -- * Destructuring the response
    ExportBundleResponse (..),
    mkExportBundleResponse,

    -- ** Response lenses
    ebrsDownloadURL,
    ebrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Mobile.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request structure used to request generation of custom SDK and tool packages required to integrate mobile web or app clients with backed AWS resources.
--
-- /See:/ 'mkExportBundle' smart constructor.
data ExportBundle = ExportBundle'
  { -- | Developer desktop or target application platform.
    platform :: Lude.Maybe Platform,
    -- | Unique bundle identifier.
    bundleId :: Lude.Text,
    -- | Unique project identifier.
    projectId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExportBundle' with the minimum fields required to make a request.
--
-- * 'platform' - Developer desktop or target application platform.
-- * 'bundleId' - Unique bundle identifier.
-- * 'projectId' - Unique project identifier.
mkExportBundle ::
  -- | 'bundleId'
  Lude.Text ->
  ExportBundle
mkExportBundle pBundleId_ =
  ExportBundle'
    { platform = Lude.Nothing,
      bundleId = pBundleId_,
      projectId = Lude.Nothing
    }

-- | Developer desktop or target application platform.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebPlatform :: Lens.Lens' ExportBundle (Lude.Maybe Platform)
ebPlatform = Lens.lens (platform :: ExportBundle -> Lude.Maybe Platform) (\s a -> s {platform = a} :: ExportBundle)
{-# DEPRECATED ebPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

-- | Unique bundle identifier.
--
-- /Note:/ Consider using 'bundleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebBundleId :: Lens.Lens' ExportBundle Lude.Text
ebBundleId = Lens.lens (bundleId :: ExportBundle -> Lude.Text) (\s a -> s {bundleId = a} :: ExportBundle)
{-# DEPRECATED ebBundleId "Use generic-lens or generic-optics with 'bundleId' instead." #-}

-- | Unique project identifier.
--
-- /Note:/ Consider using 'projectId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebProjectId :: Lens.Lens' ExportBundle (Lude.Maybe Lude.Text)
ebProjectId = Lens.lens (projectId :: ExportBundle -> Lude.Maybe Lude.Text) (\s a -> s {projectId = a} :: ExportBundle)
{-# DEPRECATED ebProjectId "Use generic-lens or generic-optics with 'projectId' instead." #-}

instance Lude.AWSRequest ExportBundle where
  type Rs ExportBundle = ExportBundleResponse
  request = Req.postJSON mobileService
  response =
    Res.receiveJSON
      ( \s h x ->
          ExportBundleResponse'
            Lude.<$> (x Lude..?> "downloadUrl") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ExportBundle where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ExportBundle where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath ExportBundle where
  toPath ExportBundle' {..} =
    Lude.mconcat ["/bundles/", Lude.toBS bundleId]

instance Lude.ToQuery ExportBundle where
  toQuery ExportBundle' {..} =
    Lude.mconcat
      ["platform" Lude.=: platform, "projectId" Lude.=: projectId]

-- | Result structure which contains link to download custom-generated SDK and tool packages used to integrate mobile web or app clients with backed AWS resources.
--
-- /See:/ 'mkExportBundleResponse' smart constructor.
data ExportBundleResponse = ExportBundleResponse'
  { -- | URL which contains the custom-generated SDK and tool packages used to integrate the client mobile app or web app with the AWS resources created by the AWS Mobile Hub project.
    downloadURL :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExportBundleResponse' with the minimum fields required to make a request.
--
-- * 'downloadURL' - URL which contains the custom-generated SDK and tool packages used to integrate the client mobile app or web app with the AWS resources created by the AWS Mobile Hub project.
-- * 'responseStatus' - The response status code.
mkExportBundleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ExportBundleResponse
mkExportBundleResponse pResponseStatus_ =
  ExportBundleResponse'
    { downloadURL = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | URL which contains the custom-generated SDK and tool packages used to integrate the client mobile app or web app with the AWS resources created by the AWS Mobile Hub project.
--
-- /Note:/ Consider using 'downloadURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebrsDownloadURL :: Lens.Lens' ExportBundleResponse (Lude.Maybe Lude.Text)
ebrsDownloadURL = Lens.lens (downloadURL :: ExportBundleResponse -> Lude.Maybe Lude.Text) (\s a -> s {downloadURL = a} :: ExportBundleResponse)
{-# DEPRECATED ebrsDownloadURL "Use generic-lens or generic-optics with 'downloadURL' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebrsResponseStatus :: Lens.Lens' ExportBundleResponse Lude.Int
ebrsResponseStatus = Lens.lens (responseStatus :: ExportBundleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ExportBundleResponse)
{-# DEPRECATED ebrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
