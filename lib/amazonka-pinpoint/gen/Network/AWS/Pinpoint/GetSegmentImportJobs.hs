{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetSegmentImportJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of the import jobs for a segment.
module Network.AWS.Pinpoint.GetSegmentImportJobs
  ( -- * Creating a request
    GetSegmentImportJobs (..),
    mkGetSegmentImportJobs,

    -- ** Request lenses
    gsijToken,
    gsijApplicationId,
    gsijPageSize,
    gsijSegmentId,

    -- * Destructuring the response
    GetSegmentImportJobsResponse (..),
    mkGetSegmentImportJobsResponse,

    -- ** Response lenses
    gsijrsImportJobsResponse,
    gsijrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetSegmentImportJobs' smart constructor.
data GetSegmentImportJobs = GetSegmentImportJobs'
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

-- | Creates a value of 'GetSegmentImportJobs' with the minimum fields required to make a request.
--
-- * 'token' - The NextToken string that specifies which page of results to return in a paginated response.
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
-- * 'pageSize' - The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
-- * 'segmentId' - The unique identifier for the segment.
mkGetSegmentImportJobs ::
  -- | 'applicationId'
  Lude.Text ->
  -- | 'segmentId'
  Lude.Text ->
  GetSegmentImportJobs
mkGetSegmentImportJobs pApplicationId_ pSegmentId_ =
  GetSegmentImportJobs'
    { token = Lude.Nothing,
      applicationId = pApplicationId_,
      pageSize = Lude.Nothing,
      segmentId = pSegmentId_
    }

-- | The NextToken string that specifies which page of results to return in a paginated response.
--
-- /Note:/ Consider using 'token' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsijToken :: Lens.Lens' GetSegmentImportJobs (Lude.Maybe Lude.Text)
gsijToken = Lens.lens (token :: GetSegmentImportJobs -> Lude.Maybe Lude.Text) (\s a -> s {token = a} :: GetSegmentImportJobs)
{-# DEPRECATED gsijToken "Use generic-lens or generic-optics with 'token' instead." #-}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsijApplicationId :: Lens.Lens' GetSegmentImportJobs Lude.Text
gsijApplicationId = Lens.lens (applicationId :: GetSegmentImportJobs -> Lude.Text) (\s a -> s {applicationId = a} :: GetSegmentImportJobs)
{-# DEPRECATED gsijApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsijPageSize :: Lens.Lens' GetSegmentImportJobs (Lude.Maybe Lude.Text)
gsijPageSize = Lens.lens (pageSize :: GetSegmentImportJobs -> Lude.Maybe Lude.Text) (\s a -> s {pageSize = a} :: GetSegmentImportJobs)
{-# DEPRECATED gsijPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

-- | The unique identifier for the segment.
--
-- /Note:/ Consider using 'segmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsijSegmentId :: Lens.Lens' GetSegmentImportJobs Lude.Text
gsijSegmentId = Lens.lens (segmentId :: GetSegmentImportJobs -> Lude.Text) (\s a -> s {segmentId = a} :: GetSegmentImportJobs)
{-# DEPRECATED gsijSegmentId "Use generic-lens or generic-optics with 'segmentId' instead." #-}

instance Lude.AWSRequest GetSegmentImportJobs where
  type Rs GetSegmentImportJobs = GetSegmentImportJobsResponse
  request = Req.get pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetSegmentImportJobsResponse'
            Lude.<$> (Lude.eitherParseJSON x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetSegmentImportJobs where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetSegmentImportJobs where
  toPath GetSegmentImportJobs' {..} =
    Lude.mconcat
      [ "/v1/apps/",
        Lude.toBS applicationId,
        "/segments/",
        Lude.toBS segmentId,
        "/jobs/import"
      ]

instance Lude.ToQuery GetSegmentImportJobs where
  toQuery GetSegmentImportJobs' {..} =
    Lude.mconcat
      ["token" Lude.=: token, "page-size" Lude.=: pageSize]

-- | /See:/ 'mkGetSegmentImportJobsResponse' smart constructor.
data GetSegmentImportJobsResponse = GetSegmentImportJobsResponse'
  { importJobsResponse :: ImportJobsResponse,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSegmentImportJobsResponse' with the minimum fields required to make a request.
--
-- * 'importJobsResponse' -
-- * 'responseStatus' - The response status code.
mkGetSegmentImportJobsResponse ::
  -- | 'importJobsResponse'
  ImportJobsResponse ->
  -- | 'responseStatus'
  Lude.Int ->
  GetSegmentImportJobsResponse
mkGetSegmentImportJobsResponse
  pImportJobsResponse_
  pResponseStatus_ =
    GetSegmentImportJobsResponse'
      { importJobsResponse =
          pImportJobsResponse_,
        responseStatus = pResponseStatus_
      }

-- | Undocumented field.
--
-- /Note:/ Consider using 'importJobsResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsijrsImportJobsResponse :: Lens.Lens' GetSegmentImportJobsResponse ImportJobsResponse
gsijrsImportJobsResponse = Lens.lens (importJobsResponse :: GetSegmentImportJobsResponse -> ImportJobsResponse) (\s a -> s {importJobsResponse = a} :: GetSegmentImportJobsResponse)
{-# DEPRECATED gsijrsImportJobsResponse "Use generic-lens or generic-optics with 'importJobsResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsijrsResponseStatus :: Lens.Lens' GetSegmentImportJobsResponse Lude.Int
gsijrsResponseStatus = Lens.lens (responseStatus :: GetSegmentImportJobsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetSegmentImportJobsResponse)
{-# DEPRECATED gsijrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
