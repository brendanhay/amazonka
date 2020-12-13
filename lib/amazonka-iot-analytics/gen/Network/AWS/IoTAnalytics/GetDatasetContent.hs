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
    gdcVersionId,
    gdcDatasetName,

    -- * Destructuring the response
    GetDatasetContentResponse (..),
    mkGetDatasetContentResponse,

    -- ** Response lenses
    gdcrsStatus,
    gdcrsEntries,
    gdcrsTimestamp,
    gdcrsResponseStatus,
  )
where

import Network.AWS.IoTAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetDatasetContent' smart constructor.
data GetDatasetContent = GetDatasetContent'
  { -- | The version of the data set whose contents are retrieved. You can also use the strings "$LATEST" or "$LATEST_SUCCEEDED" to retrieve the contents of the latest or latest successfully completed data set. If not specified, "$LATEST_SUCCEEDED" is the default.
    versionId :: Lude.Maybe Lude.Text,
    -- | The name of the data set whose contents are retrieved.
    datasetName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDatasetContent' with the minimum fields required to make a request.
--
-- * 'versionId' - The version of the data set whose contents are retrieved. You can also use the strings "$LATEST" or "$LATEST_SUCCEEDED" to retrieve the contents of the latest or latest successfully completed data set. If not specified, "$LATEST_SUCCEEDED" is the default.
-- * 'datasetName' - The name of the data set whose contents are retrieved.
mkGetDatasetContent ::
  -- | 'datasetName'
  Lude.Text ->
  GetDatasetContent
mkGetDatasetContent pDatasetName_ =
  GetDatasetContent'
    { versionId = Lude.Nothing,
      datasetName = pDatasetName_
    }

-- | The version of the data set whose contents are retrieved. You can also use the strings "$LATEST" or "$LATEST_SUCCEEDED" to retrieve the contents of the latest or latest successfully completed data set. If not specified, "$LATEST_SUCCEEDED" is the default.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdcVersionId :: Lens.Lens' GetDatasetContent (Lude.Maybe Lude.Text)
gdcVersionId = Lens.lens (versionId :: GetDatasetContent -> Lude.Maybe Lude.Text) (\s a -> s {versionId = a} :: GetDatasetContent)
{-# DEPRECATED gdcVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | The name of the data set whose contents are retrieved.
--
-- /Note:/ Consider using 'datasetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdcDatasetName :: Lens.Lens' GetDatasetContent Lude.Text
gdcDatasetName = Lens.lens (datasetName :: GetDatasetContent -> Lude.Text) (\s a -> s {datasetName = a} :: GetDatasetContent)
{-# DEPRECATED gdcDatasetName "Use generic-lens or generic-optics with 'datasetName' instead." #-}

instance Lude.AWSRequest GetDatasetContent where
  type Rs GetDatasetContent = GetDatasetContentResponse
  request = Req.get ioTAnalyticsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetDatasetContentResponse'
            Lude.<$> (x Lude..?> "status")
            Lude.<*> (x Lude..?> "entries" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "timestamp")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetDatasetContent where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetDatasetContent where
  toPath GetDatasetContent' {..} =
    Lude.mconcat ["/datasets/", Lude.toBS datasetName, "/content"]

instance Lude.ToQuery GetDatasetContent where
  toQuery GetDatasetContent' {..} =
    Lude.mconcat ["versionId" Lude.=: versionId]

-- | /See:/ 'mkGetDatasetContentResponse' smart constructor.
data GetDatasetContentResponse = GetDatasetContentResponse'
  { -- | The status of the data set content.
    status :: Lude.Maybe DatasetContentStatus,
    -- | A list of @DatasetEntry@ objects.
    entries :: Lude.Maybe [DatasetEntry],
    -- | The time when the request was made.
    timestamp :: Lude.Maybe Lude.Timestamp,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDatasetContentResponse' with the minimum fields required to make a request.
--
-- * 'status' - The status of the data set content.
-- * 'entries' - A list of @DatasetEntry@ objects.
-- * 'timestamp' - The time when the request was made.
-- * 'responseStatus' - The response status code.
mkGetDatasetContentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetDatasetContentResponse
mkGetDatasetContentResponse pResponseStatus_ =
  GetDatasetContentResponse'
    { status = Lude.Nothing,
      entries = Lude.Nothing,
      timestamp = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The status of the data set content.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdcrsStatus :: Lens.Lens' GetDatasetContentResponse (Lude.Maybe DatasetContentStatus)
gdcrsStatus = Lens.lens (status :: GetDatasetContentResponse -> Lude.Maybe DatasetContentStatus) (\s a -> s {status = a} :: GetDatasetContentResponse)
{-# DEPRECATED gdcrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | A list of @DatasetEntry@ objects.
--
-- /Note:/ Consider using 'entries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdcrsEntries :: Lens.Lens' GetDatasetContentResponse (Lude.Maybe [DatasetEntry])
gdcrsEntries = Lens.lens (entries :: GetDatasetContentResponse -> Lude.Maybe [DatasetEntry]) (\s a -> s {entries = a} :: GetDatasetContentResponse)
{-# DEPRECATED gdcrsEntries "Use generic-lens or generic-optics with 'entries' instead." #-}

-- | The time when the request was made.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdcrsTimestamp :: Lens.Lens' GetDatasetContentResponse (Lude.Maybe Lude.Timestamp)
gdcrsTimestamp = Lens.lens (timestamp :: GetDatasetContentResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {timestamp = a} :: GetDatasetContentResponse)
{-# DEPRECATED gdcrsTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdcrsResponseStatus :: Lens.Lens' GetDatasetContentResponse Lude.Int
gdcrsResponseStatus = Lens.lens (responseStatus :: GetDatasetContentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetDatasetContentResponse)
{-# DEPRECATED gdcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
