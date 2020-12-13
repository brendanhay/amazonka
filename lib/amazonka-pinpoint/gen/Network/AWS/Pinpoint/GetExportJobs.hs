{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetExportJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of all the export jobs for an application.
module Network.AWS.Pinpoint.GetExportJobs
  ( -- * Creating a request
    GetExportJobs (..),
    mkGetExportJobs,

    -- ** Request lenses
    gejsToken,
    gejsApplicationId,
    gejsPageSize,

    -- * Destructuring the response
    GetExportJobsResponse (..),
    mkGetExportJobsResponse,

    -- ** Response lenses
    gejrsExportJobsResponse,
    gejrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetExportJobs' smart constructor.
data GetExportJobs = GetExportJobs'
  { -- | The NextToken string that specifies which page of results to return in a paginated response.
    token :: Lude.Maybe Lude.Text,
    -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Lude.Text,
    -- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
    pageSize :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetExportJobs' with the minimum fields required to make a request.
--
-- * 'token' - The NextToken string that specifies which page of results to return in a paginated response.
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
-- * 'pageSize' - The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
mkGetExportJobs ::
  -- | 'applicationId'
  Lude.Text ->
  GetExportJobs
mkGetExportJobs pApplicationId_ =
  GetExportJobs'
    { token = Lude.Nothing,
      applicationId = pApplicationId_,
      pageSize = Lude.Nothing
    }

-- | The NextToken string that specifies which page of results to return in a paginated response.
--
-- /Note:/ Consider using 'token' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gejsToken :: Lens.Lens' GetExportJobs (Lude.Maybe Lude.Text)
gejsToken = Lens.lens (token :: GetExportJobs -> Lude.Maybe Lude.Text) (\s a -> s {token = a} :: GetExportJobs)
{-# DEPRECATED gejsToken "Use generic-lens or generic-optics with 'token' instead." #-}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gejsApplicationId :: Lens.Lens' GetExportJobs Lude.Text
gejsApplicationId = Lens.lens (applicationId :: GetExportJobs -> Lude.Text) (\s a -> s {applicationId = a} :: GetExportJobs)
{-# DEPRECATED gejsApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gejsPageSize :: Lens.Lens' GetExportJobs (Lude.Maybe Lude.Text)
gejsPageSize = Lens.lens (pageSize :: GetExportJobs -> Lude.Maybe Lude.Text) (\s a -> s {pageSize = a} :: GetExportJobs)
{-# DEPRECATED gejsPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

instance Lude.AWSRequest GetExportJobs where
  type Rs GetExportJobs = GetExportJobsResponse
  request = Req.get pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetExportJobsResponse'
            Lude.<$> (Lude.eitherParseJSON x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetExportJobs where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetExportJobs where
  toPath GetExportJobs' {..} =
    Lude.mconcat
      ["/v1/apps/", Lude.toBS applicationId, "/jobs/export"]

instance Lude.ToQuery GetExportJobs where
  toQuery GetExportJobs' {..} =
    Lude.mconcat
      ["token" Lude.=: token, "page-size" Lude.=: pageSize]

-- | /See:/ 'mkGetExportJobsResponse' smart constructor.
data GetExportJobsResponse = GetExportJobsResponse'
  { exportJobsResponse :: ExportJobsResponse,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetExportJobsResponse' with the minimum fields required to make a request.
--
-- * 'exportJobsResponse' -
-- * 'responseStatus' - The response status code.
mkGetExportJobsResponse ::
  -- | 'exportJobsResponse'
  ExportJobsResponse ->
  -- | 'responseStatus'
  Lude.Int ->
  GetExportJobsResponse
mkGetExportJobsResponse pExportJobsResponse_ pResponseStatus_ =
  GetExportJobsResponse'
    { exportJobsResponse = pExportJobsResponse_,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'exportJobsResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gejrsExportJobsResponse :: Lens.Lens' GetExportJobsResponse ExportJobsResponse
gejrsExportJobsResponse = Lens.lens (exportJobsResponse :: GetExportJobsResponse -> ExportJobsResponse) (\s a -> s {exportJobsResponse = a} :: GetExportJobsResponse)
{-# DEPRECATED gejrsExportJobsResponse "Use generic-lens or generic-optics with 'exportJobsResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gejrsResponseStatus :: Lens.Lens' GetExportJobsResponse Lude.Int
gejrsResponseStatus = Lens.lens (responseStatus :: GetExportJobsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetExportJobsResponse)
{-# DEPRECATED gejrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
