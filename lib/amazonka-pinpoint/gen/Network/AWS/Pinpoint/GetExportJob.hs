{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetExportJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of a specific export job for an application.
module Network.AWS.Pinpoint.GetExportJob
  ( -- * Creating a request
    GetExportJob (..),
    mkGetExportJob,

    -- ** Request lenses
    gejJobId,
    gejApplicationId,

    -- * Destructuring the response
    GetExportJobResponse (..),
    mkGetExportJobResponse,

    -- ** Response lenses
    gejfrsExportJobResponse,
    gejfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetExportJob' smart constructor.
data GetExportJob = GetExportJob'
  { -- | The unique identifier for the job.
    jobId :: Lude.Text,
    -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetExportJob' with the minimum fields required to make a request.
--
-- * 'jobId' - The unique identifier for the job.
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
mkGetExportJob ::
  -- | 'jobId'
  Lude.Text ->
  -- | 'applicationId'
  Lude.Text ->
  GetExportJob
mkGetExportJob pJobId_ pApplicationId_ =
  GetExportJob' {jobId = pJobId_, applicationId = pApplicationId_}

-- | The unique identifier for the job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gejJobId :: Lens.Lens' GetExportJob Lude.Text
gejJobId = Lens.lens (jobId :: GetExportJob -> Lude.Text) (\s a -> s {jobId = a} :: GetExportJob)
{-# DEPRECATED gejJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gejApplicationId :: Lens.Lens' GetExportJob Lude.Text
gejApplicationId = Lens.lens (applicationId :: GetExportJob -> Lude.Text) (\s a -> s {applicationId = a} :: GetExportJob)
{-# DEPRECATED gejApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Lude.AWSRequest GetExportJob where
  type Rs GetExportJob = GetExportJobResponse
  request = Req.get pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetExportJobResponse'
            Lude.<$> (Lude.eitherParseJSON x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetExportJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetExportJob where
  toPath GetExportJob' {..} =
    Lude.mconcat
      [ "/v1/apps/",
        Lude.toBS applicationId,
        "/jobs/export/",
        Lude.toBS jobId
      ]

instance Lude.ToQuery GetExportJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetExportJobResponse' smart constructor.
data GetExportJobResponse = GetExportJobResponse'
  { exportJobResponse :: ExportJobResponse,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetExportJobResponse' with the minimum fields required to make a request.
--
-- * 'exportJobResponse' -
-- * 'responseStatus' - The response status code.
mkGetExportJobResponse ::
  -- | 'exportJobResponse'
  ExportJobResponse ->
  -- | 'responseStatus'
  Lude.Int ->
  GetExportJobResponse
mkGetExportJobResponse pExportJobResponse_ pResponseStatus_ =
  GetExportJobResponse'
    { exportJobResponse = pExportJobResponse_,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'exportJobResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gejfrsExportJobResponse :: Lens.Lens' GetExportJobResponse ExportJobResponse
gejfrsExportJobResponse = Lens.lens (exportJobResponse :: GetExportJobResponse -> ExportJobResponse) (\s a -> s {exportJobResponse = a} :: GetExportJobResponse)
{-# DEPRECATED gejfrsExportJobResponse "Use generic-lens or generic-optics with 'exportJobResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gejfrsResponseStatus :: Lens.Lens' GetExportJobResponse Lude.Int
gejfrsResponseStatus = Lens.lens (responseStatus :: GetExportJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetExportJobResponse)
{-# DEPRECATED gejfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
