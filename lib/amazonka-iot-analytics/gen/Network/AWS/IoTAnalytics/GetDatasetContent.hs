{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.GetDatasetContent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the contents of a data set as presigned URIs.
module Network.AWS.IoTAnalytics.GetDatasetContent
  ( -- * Creating a request
    GetDatasetContent (..),
    mkGetDatasetContent,

    -- ** Request lenses
    gdcDatasetName,
    gdcVersionId,

    -- * Destructuring the response
    GetDatasetContentResponse (..),
    mkGetDatasetContentResponse,

    -- ** Response lenses
    gdcrrsEntries,
    gdcrrsStatus,
    gdcrrsTimestamp,
    gdcrrsResponseStatus,
  )
where

import qualified Network.AWS.IoTAnalytics.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetDatasetContent' smart constructor.
data GetDatasetContent = GetDatasetContent'
  { -- | The name of the data set whose contents are retrieved.
    datasetName :: Types.DatasetName,
    -- | The version of the data set whose contents are retrieved. You can also use the strings "$LATEST" or "$LATEST_SUCCEEDED" to retrieve the contents of the latest or latest successfully completed data set. If not specified, "$LATEST_SUCCEEDED" is the default.
    versionId :: Core.Maybe Types.DatasetContentVersion
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDatasetContent' value with any optional fields omitted.
mkGetDatasetContent ::
  -- | 'datasetName'
  Types.DatasetName ->
  GetDatasetContent
mkGetDatasetContent datasetName =
  GetDatasetContent' {datasetName, versionId = Core.Nothing}

-- | The name of the data set whose contents are retrieved.
--
-- /Note:/ Consider using 'datasetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdcDatasetName :: Lens.Lens' GetDatasetContent Types.DatasetName
gdcDatasetName = Lens.field @"datasetName"
{-# DEPRECATED gdcDatasetName "Use generic-lens or generic-optics with 'datasetName' instead." #-}

-- | The version of the data set whose contents are retrieved. You can also use the strings "$LATEST" or "$LATEST_SUCCEEDED" to retrieve the contents of the latest or latest successfully completed data set. If not specified, "$LATEST_SUCCEEDED" is the default.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdcVersionId :: Lens.Lens' GetDatasetContent (Core.Maybe Types.DatasetContentVersion)
gdcVersionId = Lens.field @"versionId"
{-# DEPRECATED gdcVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

instance Core.AWSRequest GetDatasetContent where
  type Rs GetDatasetContent = GetDatasetContentResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/datasets/" Core.<> (Core.toText datasetName)
                Core.<> ("/content")
            ),
        Core._rqQuery = Core.toQueryValue "versionId" Core.<$> versionId,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDatasetContentResponse'
            Core.<$> (x Core..:? "entries")
            Core.<*> (x Core..:? "status")
            Core.<*> (x Core..:? "timestamp")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetDatasetContentResponse' smart constructor.
data GetDatasetContentResponse = GetDatasetContentResponse'
  { -- | A list of @DatasetEntry@ objects.
    entries :: Core.Maybe [Types.DatasetEntry],
    -- | The status of the data set content.
    status :: Core.Maybe Types.DatasetContentStatus,
    -- | The time when the request was made.
    timestamp :: Core.Maybe Core.NominalDiffTime,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetDatasetContentResponse' value with any optional fields omitted.
mkGetDatasetContentResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetDatasetContentResponse
mkGetDatasetContentResponse responseStatus =
  GetDatasetContentResponse'
    { entries = Core.Nothing,
      status = Core.Nothing,
      timestamp = Core.Nothing,
      responseStatus
    }

-- | A list of @DatasetEntry@ objects.
--
-- /Note:/ Consider using 'entries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdcrrsEntries :: Lens.Lens' GetDatasetContentResponse (Core.Maybe [Types.DatasetEntry])
gdcrrsEntries = Lens.field @"entries"
{-# DEPRECATED gdcrrsEntries "Use generic-lens or generic-optics with 'entries' instead." #-}

-- | The status of the data set content.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdcrrsStatus :: Lens.Lens' GetDatasetContentResponse (Core.Maybe Types.DatasetContentStatus)
gdcrrsStatus = Lens.field @"status"
{-# DEPRECATED gdcrrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The time when the request was made.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdcrrsTimestamp :: Lens.Lens' GetDatasetContentResponse (Core.Maybe Core.NominalDiffTime)
gdcrrsTimestamp = Lens.field @"timestamp"
{-# DEPRECATED gdcrrsTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdcrrsResponseStatus :: Lens.Lens' GetDatasetContentResponse Core.Int
gdcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gdcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
