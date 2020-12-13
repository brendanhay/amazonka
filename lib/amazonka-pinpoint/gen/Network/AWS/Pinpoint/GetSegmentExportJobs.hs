{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetSegmentExportJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of the export jobs for a segment.
module Network.AWS.Pinpoint.GetSegmentExportJobs
  ( -- * Creating a request
    GetSegmentExportJobs (..),
    mkGetSegmentExportJobs,

    -- ** Request lenses
    gsejToken,
    gsejApplicationId,
    gsejPageSize,
    gsejSegmentId,

    -- * Destructuring the response
    GetSegmentExportJobsResponse (..),
    mkGetSegmentExportJobsResponse,

    -- ** Response lenses
    gsejrsExportJobsResponse,
    gsejrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetSegmentExportJobs' smart constructor.
data GetSegmentExportJobs = GetSegmentExportJobs'
  { -- | The NextToken string that specifies which page of results to return in a paginated response.
    token :: Lude.Maybe Lude.Text,
    -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Lude.Text,
    -- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
    pageSize :: Lude.Maybe Lude.Text,
    -- | The unique identifier for the segment.
    segmentId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSegmentExportJobs' with the minimum fields required to make a request.
--
-- * 'token' - The NextToken string that specifies which page of results to return in a paginated response.
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
-- * 'pageSize' - The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
-- * 'segmentId' - The unique identifier for the segment.
mkGetSegmentExportJobs ::
  -- | 'applicationId'
  Lude.Text ->
  -- | 'segmentId'
  Lude.Text ->
  GetSegmentExportJobs
mkGetSegmentExportJobs pApplicationId_ pSegmentId_ =
  GetSegmentExportJobs'
    { token = Lude.Nothing,
      applicationId = pApplicationId_,
      pageSize = Lude.Nothing,
      segmentId = pSegmentId_
    }

-- | The NextToken string that specifies which page of results to return in a paginated response.
--
-- /Note:/ Consider using 'token' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsejToken :: Lens.Lens' GetSegmentExportJobs (Lude.Maybe Lude.Text)
gsejToken = Lens.lens (token :: GetSegmentExportJobs -> Lude.Maybe Lude.Text) (\s a -> s {token = a} :: GetSegmentExportJobs)
{-# DEPRECATED gsejToken "Use generic-lens or generic-optics with 'token' instead." #-}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsejApplicationId :: Lens.Lens' GetSegmentExportJobs Lude.Text
gsejApplicationId = Lens.lens (applicationId :: GetSegmentExportJobs -> Lude.Text) (\s a -> s {applicationId = a} :: GetSegmentExportJobs)
{-# DEPRECATED gsejApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsejPageSize :: Lens.Lens' GetSegmentExportJobs (Lude.Maybe Lude.Text)
gsejPageSize = Lens.lens (pageSize :: GetSegmentExportJobs -> Lude.Maybe Lude.Text) (\s a -> s {pageSize = a} :: GetSegmentExportJobs)
{-# DEPRECATED gsejPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

-- | The unique identifier for the segment.
--
-- /Note:/ Consider using 'segmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsejSegmentId :: Lens.Lens' GetSegmentExportJobs Lude.Text
gsejSegmentId = Lens.lens (segmentId :: GetSegmentExportJobs -> Lude.Text) (\s a -> s {segmentId = a} :: GetSegmentExportJobs)
{-# DEPRECATED gsejSegmentId "Use generic-lens or generic-optics with 'segmentId' instead." #-}

instance Lude.AWSRequest GetSegmentExportJobs where
  type Rs GetSegmentExportJobs = GetSegmentExportJobsResponse
  request = Req.get pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetSegmentExportJobsResponse'
            Lude.<$> (Lude.eitherParseJSON x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetSegmentExportJobs where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetSegmentExportJobs where
  toPath GetSegmentExportJobs' {..} =
    Lude.mconcat
      [ "/v1/apps/",
        Lude.toBS applicationId,
        "/segments/",
        Lude.toBS segmentId,
        "/jobs/export"
      ]

instance Lude.ToQuery GetSegmentExportJobs where
  toQuery GetSegmentExportJobs' {..} =
    Lude.mconcat
      ["token" Lude.=: token, "page-size" Lude.=: pageSize]

-- | /See:/ 'mkGetSegmentExportJobsResponse' smart constructor.
data GetSegmentExportJobsResponse = GetSegmentExportJobsResponse'
  { exportJobsResponse :: ExportJobsResponse,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSegmentExportJobsResponse' with the minimum fields required to make a request.
--
-- * 'exportJobsResponse' -
-- * 'responseStatus' - The response status code.
mkGetSegmentExportJobsResponse ::
  -- | 'exportJobsResponse'
  ExportJobsResponse ->
  -- | 'responseStatus'
  Lude.Int ->
  GetSegmentExportJobsResponse
mkGetSegmentExportJobsResponse
  pExportJobsResponse_
  pResponseStatus_ =
    GetSegmentExportJobsResponse'
      { exportJobsResponse =
          pExportJobsResponse_,
        responseStatus = pResponseStatus_
      }

-- | Undocumented field.
--
-- /Note:/ Consider using 'exportJobsResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsejrsExportJobsResponse :: Lens.Lens' GetSegmentExportJobsResponse ExportJobsResponse
gsejrsExportJobsResponse = Lens.lens (exportJobsResponse :: GetSegmentExportJobsResponse -> ExportJobsResponse) (\s a -> s {exportJobsResponse = a} :: GetSegmentExportJobsResponse)
{-# DEPRECATED gsejrsExportJobsResponse "Use generic-lens or generic-optics with 'exportJobsResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsejrsResponseStatus :: Lens.Lens' GetSegmentExportJobsResponse Lude.Int
gsejrsResponseStatus = Lens.lens (responseStatus :: GetSegmentExportJobsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetSegmentExportJobsResponse)
{-# DEPRECATED gsejrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
