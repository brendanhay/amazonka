{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    gejApplicationId,
    gejJobId,

    -- * Destructuring the response
    GetExportJobResponse (..),
    mkGetExportJobResponse,

    -- ** Response lenses
    gejersResponseStatus,
    gejersExportJobResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetExportJob' smart constructor.
data GetExportJob = GetExportJob'
  { applicationId :: Lude.Text,
    jobId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetExportJob' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
-- * 'jobId' - The unique identifier for the job.
mkGetExportJob ::
  -- | 'applicationId'
  Lude.Text ->
  -- | 'jobId'
  Lude.Text ->
  GetExportJob
mkGetExportJob pApplicationId_ pJobId_ =
  GetExportJob' {applicationId = pApplicationId_, jobId = pJobId_}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gejApplicationId :: Lens.Lens' GetExportJob Lude.Text
gejApplicationId = Lens.lens (applicationId :: GetExportJob -> Lude.Text) (\s a -> s {applicationId = a} :: GetExportJob)
{-# DEPRECATED gejApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The unique identifier for the job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gejJobId :: Lens.Lens' GetExportJob Lude.Text
gejJobId = Lens.lens (jobId :: GetExportJob -> Lude.Text) (\s a -> s {jobId = a} :: GetExportJob)
{-# DEPRECATED gejJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

instance Lude.AWSRequest GetExportJob where
  type Rs GetExportJob = GetExportJobResponse
  request = Req.get pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetExportJobResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (Lude.eitherParseJSON x)
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
  { responseStatus ::
      Lude.Int,
    exportJobResponse :: ExportJobResponse
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetExportJobResponse' with the minimum fields required to make a request.
--
-- * 'exportJobResponse' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkGetExportJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'exportJobResponse'
  ExportJobResponse ->
  GetExportJobResponse
mkGetExportJobResponse pResponseStatus_ pExportJobResponse_ =
  GetExportJobResponse'
    { responseStatus = pResponseStatus_,
      exportJobResponse = pExportJobResponse_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gejersResponseStatus :: Lens.Lens' GetExportJobResponse Lude.Int
gejersResponseStatus = Lens.lens (responseStatus :: GetExportJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetExportJobResponse)
{-# DEPRECATED gejersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'exportJobResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gejersExportJobResponse :: Lens.Lens' GetExportJobResponse ExportJobResponse
gejersExportJobResponse = Lens.lens (exportJobResponse :: GetExportJobResponse -> ExportJobResponse) (\s a -> s {exportJobResponse = a} :: GetExportJobResponse)
{-# DEPRECATED gejersExportJobResponse "Use generic-lens or generic-optics with 'exportJobResponse' instead." #-}
