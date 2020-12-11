{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.GetExport
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Exports the contents of a Amazon Lex resource in a specified format.
module Network.AWS.LexModels.GetExport
  ( -- * Creating a request
    GetExport (..),
    mkGetExport,

    -- ** Request lenses
    geName,
    geVersion,
    geResourceType,
    geExportType,

    -- * Destructuring the response
    GetExportResponse (..),
    mkGetExportResponse,

    -- ** Response lenses
    gersFailureReason,
    gersResourceType,
    gersExportStatus,
    gersUrl,
    gersExportType,
    gersName,
    gersVersion,
    gersResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetExport' smart constructor.
data GetExport = GetExport'
  { name :: Lude.Text,
    version :: Lude.Text,
    resourceType :: ResourceType,
    exportType :: ExportType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetExport' with the minimum fields required to make a request.
--
-- * 'exportType' - The format of the exported data.
-- * 'name' - The name of the bot to export.
-- * 'resourceType' - The type of resource to export.
-- * 'version' - The version of the bot to export.
mkGetExport ::
  -- | 'name'
  Lude.Text ->
  -- | 'version'
  Lude.Text ->
  -- | 'resourceType'
  ResourceType ->
  -- | 'exportType'
  ExportType ->
  GetExport
mkGetExport pName_ pVersion_ pResourceType_ pExportType_ =
  GetExport'
    { name = pName_,
      version = pVersion_,
      resourceType = pResourceType_,
      exportType = pExportType_
    }

-- | The name of the bot to export.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
geName :: Lens.Lens' GetExport Lude.Text
geName = Lens.lens (name :: GetExport -> Lude.Text) (\s a -> s {name = a} :: GetExport)
{-# DEPRECATED geName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The version of the bot to export.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
geVersion :: Lens.Lens' GetExport Lude.Text
geVersion = Lens.lens (version :: GetExport -> Lude.Text) (\s a -> s {version = a} :: GetExport)
{-# DEPRECATED geVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The type of resource to export.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
geResourceType :: Lens.Lens' GetExport ResourceType
geResourceType = Lens.lens (resourceType :: GetExport -> ResourceType) (\s a -> s {resourceType = a} :: GetExport)
{-# DEPRECATED geResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The format of the exported data.
--
-- /Note:/ Consider using 'exportType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
geExportType :: Lens.Lens' GetExport ExportType
geExportType = Lens.lens (exportType :: GetExport -> ExportType) (\s a -> s {exportType = a} :: GetExport)
{-# DEPRECATED geExportType "Use generic-lens or generic-optics with 'exportType' instead." #-}

instance Lude.AWSRequest GetExport where
  type Rs GetExport = GetExportResponse
  request = Req.get lexModelsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetExportResponse'
            Lude.<$> (x Lude..?> "failureReason")
            Lude.<*> (x Lude..?> "resourceType")
            Lude.<*> (x Lude..?> "exportStatus")
            Lude.<*> (x Lude..?> "url")
            Lude.<*> (x Lude..?> "exportType")
            Lude.<*> (x Lude..?> "name")
            Lude.<*> (x Lude..?> "version")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetExport where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetExport where
  toPath = Lude.const "/exports/"

instance Lude.ToQuery GetExport where
  toQuery GetExport' {..} =
    Lude.mconcat
      [ "name" Lude.=: name,
        "version" Lude.=: version,
        "resourceType" Lude.=: resourceType,
        "exportType" Lude.=: exportType
      ]

-- | /See:/ 'mkGetExportResponse' smart constructor.
data GetExportResponse = GetExportResponse'
  { failureReason ::
      Lude.Maybe Lude.Text,
    resourceType :: Lude.Maybe ResourceType,
    exportStatus :: Lude.Maybe ExportStatus,
    url :: Lude.Maybe Lude.Text,
    exportType :: Lude.Maybe ExportType,
    name :: Lude.Maybe Lude.Text,
    version :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'GetExportResponse' with the minimum fields required to make a request.
--
-- * 'exportStatus' - The status of the export.
--
--
--     * @IN_PROGRESS@ - The export is in progress.
--
--
--     * @READY@ - The export is complete.
--
--
--     * @FAILED@ - The export could not be completed.
--
--
-- * 'exportType' - The format of the exported data.
-- * 'failureReason' - If @status@ is @FAILED@ , Amazon Lex provides the reason that it failed to export the resource.
-- * 'name' - The name of the bot being exported.
-- * 'resourceType' - The type of the exported resource.
-- * 'responseStatus' - The response status code.
-- * 'url' - An S3 pre-signed URL that provides the location of the exported resource. The exported resource is a ZIP archive that contains the exported resource in JSON format. The structure of the archive may change. Your code should not rely on the archive structure.
-- * 'version' - The version of the bot being exported.
mkGetExportResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetExportResponse
mkGetExportResponse pResponseStatus_ =
  GetExportResponse'
    { failureReason = Lude.Nothing,
      resourceType = Lude.Nothing,
      exportStatus = Lude.Nothing,
      url = Lude.Nothing,
      exportType = Lude.Nothing,
      name = Lude.Nothing,
      version = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If @status@ is @FAILED@ , Amazon Lex provides the reason that it failed to export the resource.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gersFailureReason :: Lens.Lens' GetExportResponse (Lude.Maybe Lude.Text)
gersFailureReason = Lens.lens (failureReason :: GetExportResponse -> Lude.Maybe Lude.Text) (\s a -> s {failureReason = a} :: GetExportResponse)
{-# DEPRECATED gersFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | The type of the exported resource.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gersResourceType :: Lens.Lens' GetExportResponse (Lude.Maybe ResourceType)
gersResourceType = Lens.lens (resourceType :: GetExportResponse -> Lude.Maybe ResourceType) (\s a -> s {resourceType = a} :: GetExportResponse)
{-# DEPRECATED gersResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The status of the export.
--
--
--     * @IN_PROGRESS@ - The export is in progress.
--
--
--     * @READY@ - The export is complete.
--
--
--     * @FAILED@ - The export could not be completed.
--
--
--
-- /Note:/ Consider using 'exportStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gersExportStatus :: Lens.Lens' GetExportResponse (Lude.Maybe ExportStatus)
gersExportStatus = Lens.lens (exportStatus :: GetExportResponse -> Lude.Maybe ExportStatus) (\s a -> s {exportStatus = a} :: GetExportResponse)
{-# DEPRECATED gersExportStatus "Use generic-lens or generic-optics with 'exportStatus' instead." #-}

-- | An S3 pre-signed URL that provides the location of the exported resource. The exported resource is a ZIP archive that contains the exported resource in JSON format. The structure of the archive may change. Your code should not rely on the archive structure.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gersUrl :: Lens.Lens' GetExportResponse (Lude.Maybe Lude.Text)
gersUrl = Lens.lens (url :: GetExportResponse -> Lude.Maybe Lude.Text) (\s a -> s {url = a} :: GetExportResponse)
{-# DEPRECATED gersUrl "Use generic-lens or generic-optics with 'url' instead." #-}

-- | The format of the exported data.
--
-- /Note:/ Consider using 'exportType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gersExportType :: Lens.Lens' GetExportResponse (Lude.Maybe ExportType)
gersExportType = Lens.lens (exportType :: GetExportResponse -> Lude.Maybe ExportType) (\s a -> s {exportType = a} :: GetExportResponse)
{-# DEPRECATED gersExportType "Use generic-lens or generic-optics with 'exportType' instead." #-}

-- | The name of the bot being exported.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gersName :: Lens.Lens' GetExportResponse (Lude.Maybe Lude.Text)
gersName = Lens.lens (name :: GetExportResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: GetExportResponse)
{-# DEPRECATED gersName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The version of the bot being exported.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gersVersion :: Lens.Lens' GetExportResponse (Lude.Maybe Lude.Text)
gersVersion = Lens.lens (version :: GetExportResponse -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: GetExportResponse)
{-# DEPRECATED gersVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gersResponseStatus :: Lens.Lens' GetExportResponse Lude.Int
gersResponseStatus = Lens.lens (responseStatus :: GetExportResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetExportResponse)
{-# DEPRECATED gersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
