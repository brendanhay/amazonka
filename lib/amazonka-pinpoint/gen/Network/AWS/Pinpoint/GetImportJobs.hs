{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetImportJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of all the import jobs for an application.
module Network.AWS.Pinpoint.GetImportJobs
  ( -- * Creating a request
    GetImportJobs (..),
    mkGetImportJobs,

    -- ** Request lenses
    gijsToken,
    gijsPageSize,
    gijsApplicationId,

    -- * Destructuring the response
    GetImportJobsResponse (..),
    mkGetImportJobsResponse,

    -- ** Response lenses
    gijsrsResponseStatus,
    gijsrsImportJobsResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetImportJobs' smart constructor.
data GetImportJobs = GetImportJobs'
  { token :: Lude.Maybe Lude.Text,
    pageSize :: Lude.Maybe Lude.Text,
    applicationId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetImportJobs' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
-- * 'pageSize' - The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
-- * 'token' - The NextToken string that specifies which page of results to return in a paginated response.
mkGetImportJobs ::
  -- | 'applicationId'
  Lude.Text ->
  GetImportJobs
mkGetImportJobs pApplicationId_ =
  GetImportJobs'
    { token = Lude.Nothing,
      pageSize = Lude.Nothing,
      applicationId = pApplicationId_
    }

-- | The NextToken string that specifies which page of results to return in a paginated response.
--
-- /Note:/ Consider using 'token' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gijsToken :: Lens.Lens' GetImportJobs (Lude.Maybe Lude.Text)
gijsToken = Lens.lens (token :: GetImportJobs -> Lude.Maybe Lude.Text) (\s a -> s {token = a} :: GetImportJobs)
{-# DEPRECATED gijsToken "Use generic-lens or generic-optics with 'token' instead." #-}

-- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gijsPageSize :: Lens.Lens' GetImportJobs (Lude.Maybe Lude.Text)
gijsPageSize = Lens.lens (pageSize :: GetImportJobs -> Lude.Maybe Lude.Text) (\s a -> s {pageSize = a} :: GetImportJobs)
{-# DEPRECATED gijsPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gijsApplicationId :: Lens.Lens' GetImportJobs Lude.Text
gijsApplicationId = Lens.lens (applicationId :: GetImportJobs -> Lude.Text) (\s a -> s {applicationId = a} :: GetImportJobs)
{-# DEPRECATED gijsApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Lude.AWSRequest GetImportJobs where
  type Rs GetImportJobs = GetImportJobsResponse
  request = Req.get pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetImportJobsResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (Lude.eitherParseJSON x)
      )

instance Lude.ToHeaders GetImportJobs where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetImportJobs where
  toPath GetImportJobs' {..} =
    Lude.mconcat
      ["/v1/apps/", Lude.toBS applicationId, "/jobs/import"]

instance Lude.ToQuery GetImportJobs where
  toQuery GetImportJobs' {..} =
    Lude.mconcat
      ["token" Lude.=: token, "page-size" Lude.=: pageSize]

-- | /See:/ 'mkGetImportJobsResponse' smart constructor.
data GetImportJobsResponse = GetImportJobsResponse'
  { responseStatus ::
      Lude.Int,
    importJobsResponse :: ImportJobsResponse
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetImportJobsResponse' with the minimum fields required to make a request.
--
-- * 'importJobsResponse' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkGetImportJobsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'importJobsResponse'
  ImportJobsResponse ->
  GetImportJobsResponse
mkGetImportJobsResponse pResponseStatus_ pImportJobsResponse_ =
  GetImportJobsResponse'
    { responseStatus = pResponseStatus_,
      importJobsResponse = pImportJobsResponse_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gijsrsResponseStatus :: Lens.Lens' GetImportJobsResponse Lude.Int
gijsrsResponseStatus = Lens.lens (responseStatus :: GetImportJobsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetImportJobsResponse)
{-# DEPRECATED gijsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'importJobsResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gijsrsImportJobsResponse :: Lens.Lens' GetImportJobsResponse ImportJobsResponse
gijsrsImportJobsResponse = Lens.lens (importJobsResponse :: GetImportJobsResponse -> ImportJobsResponse) (\s a -> s {importJobsResponse = a} :: GetImportJobsResponse)
{-# DEPRECATED gijsrsImportJobsResponse "Use generic-lens or generic-optics with 'importJobsResponse' instead." #-}
