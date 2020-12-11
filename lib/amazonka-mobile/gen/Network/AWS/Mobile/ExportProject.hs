{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ExportProject (..),
    mkExportProject,

    -- ** Request lenses
    epProjectId,

    -- * Destructuring the response
    ExportProjectResponse (..),
    mkExportProjectResponse,

    -- ** Response lenses
    eprsShareURL,
    eprsDownloadURL,
    eprsSnapshotId,
    eprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Mobile.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request structure used in requests to export project configuration details.
--
-- /See:/ 'mkExportProject' smart constructor.
newtype ExportProject = ExportProject' {projectId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExportProject' with the minimum fields required to make a request.
--
-- * 'projectId' - Unique project identifier.
mkExportProject ::
  -- | 'projectId'
  Lude.Text ->
  ExportProject
mkExportProject pProjectId_ =
  ExportProject' {projectId = pProjectId_}

-- | Unique project identifier.
--
-- /Note:/ Consider using 'projectId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epProjectId :: Lens.Lens' ExportProject Lude.Text
epProjectId = Lens.lens (projectId :: ExportProject -> Lude.Text) (\s a -> s {projectId = a} :: ExportProject)
{-# DEPRECATED epProjectId "Use generic-lens or generic-optics with 'projectId' instead." #-}

instance Lude.AWSRequest ExportProject where
  type Rs ExportProject = ExportProjectResponse
  request = Req.postJSON mobileService
  response =
    Res.receiveJSON
      ( \s h x ->
          ExportProjectResponse'
            Lude.<$> (x Lude..?> "shareUrl")
            Lude.<*> (x Lude..?> "downloadUrl")
            Lude.<*> (x Lude..?> "snapshotId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ExportProject where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ExportProject where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath ExportProject where
  toPath ExportProject' {..} =
    Lude.mconcat ["/exports/", Lude.toBS projectId]

instance Lude.ToQuery ExportProject where
  toQuery = Lude.const Lude.mempty

-- | Result structure used for requests to export project configuration details.
--
-- /See:/ 'mkExportProjectResponse' smart constructor.
data ExportProjectResponse = ExportProjectResponse'
  { shareURL ::
      Lude.Maybe Lude.Text,
    downloadURL :: Lude.Maybe Lude.Text,
    snapshotId :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExportProjectResponse' with the minimum fields required to make a request.
--
-- * 'downloadURL' - URL which can be used to download the exported project configuation file(s).
-- * 'responseStatus' - The response status code.
-- * 'shareURL' - URL which can be shared to allow other AWS users to create their own project in AWS Mobile Hub with the same configuration as the specified project. This URL pertains to a snapshot in time of the project configuration that is created when this API is called. If you want to share additional changes to your project configuration, then you will need to create and share a new snapshot by calling this method again.
-- * 'snapshotId' - Unique identifier for the exported snapshot of the project configuration. This snapshot identifier is included in the share URL.
mkExportProjectResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ExportProjectResponse
mkExportProjectResponse pResponseStatus_ =
  ExportProjectResponse'
    { shareURL = Lude.Nothing,
      downloadURL = Lude.Nothing,
      snapshotId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | URL which can be shared to allow other AWS users to create their own project in AWS Mobile Hub with the same configuration as the specified project. This URL pertains to a snapshot in time of the project configuration that is created when this API is called. If you want to share additional changes to your project configuration, then you will need to create and share a new snapshot by calling this method again.
--
-- /Note:/ Consider using 'shareURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eprsShareURL :: Lens.Lens' ExportProjectResponse (Lude.Maybe Lude.Text)
eprsShareURL = Lens.lens (shareURL :: ExportProjectResponse -> Lude.Maybe Lude.Text) (\s a -> s {shareURL = a} :: ExportProjectResponse)
{-# DEPRECATED eprsShareURL "Use generic-lens or generic-optics with 'shareURL' instead." #-}

-- | URL which can be used to download the exported project configuation file(s).
--
-- /Note:/ Consider using 'downloadURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eprsDownloadURL :: Lens.Lens' ExportProjectResponse (Lude.Maybe Lude.Text)
eprsDownloadURL = Lens.lens (downloadURL :: ExportProjectResponse -> Lude.Maybe Lude.Text) (\s a -> s {downloadURL = a} :: ExportProjectResponse)
{-# DEPRECATED eprsDownloadURL "Use generic-lens or generic-optics with 'downloadURL' instead." #-}

-- | Unique identifier for the exported snapshot of the project configuration. This snapshot identifier is included in the share URL.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eprsSnapshotId :: Lens.Lens' ExportProjectResponse (Lude.Maybe Lude.Text)
eprsSnapshotId = Lens.lens (snapshotId :: ExportProjectResponse -> Lude.Maybe Lude.Text) (\s a -> s {snapshotId = a} :: ExportProjectResponse)
{-# DEPRECATED eprsSnapshotId "Use generic-lens or generic-optics with 'snapshotId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eprsResponseStatus :: Lens.Lens' ExportProjectResponse Lude.Int
eprsResponseStatus = Lens.lens (responseStatus :: ExportProjectResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ExportProjectResponse)
{-# DEPRECATED eprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
