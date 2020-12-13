{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetImportJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of a specific import job for an application.
module Network.AWS.Pinpoint.GetImportJob
  ( -- * Creating a request
    GetImportJob (..),
    mkGetImportJob,

    -- ** Request lenses
    gijfJobId,
    gijfApplicationId,

    -- * Destructuring the response
    GetImportJobResponse (..),
    mkGetImportJobResponse,

    -- ** Response lenses
    gijrsImportJobResponse,
    gijrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetImportJob' smart constructor.
data GetImportJob = GetImportJob'
  { -- | The unique identifier for the job.
    jobId :: Lude.Text,
    -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetImportJob' with the minimum fields required to make a request.
--
-- * 'jobId' - The unique identifier for the job.
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
mkGetImportJob ::
  -- | 'jobId'
  Lude.Text ->
  -- | 'applicationId'
  Lude.Text ->
  GetImportJob
mkGetImportJob pJobId_ pApplicationId_ =
  GetImportJob' {jobId = pJobId_, applicationId = pApplicationId_}

-- | The unique identifier for the job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gijfJobId :: Lens.Lens' GetImportJob Lude.Text
gijfJobId = Lens.lens (jobId :: GetImportJob -> Lude.Text) (\s a -> s {jobId = a} :: GetImportJob)
{-# DEPRECATED gijfJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gijfApplicationId :: Lens.Lens' GetImportJob Lude.Text
gijfApplicationId = Lens.lens (applicationId :: GetImportJob -> Lude.Text) (\s a -> s {applicationId = a} :: GetImportJob)
{-# DEPRECATED gijfApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Lude.AWSRequest GetImportJob where
  type Rs GetImportJob = GetImportJobResponse
  request = Req.get pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetImportJobResponse'
            Lude.<$> (Lude.eitherParseJSON x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetImportJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetImportJob where
  toPath GetImportJob' {..} =
    Lude.mconcat
      [ "/v1/apps/",
        Lude.toBS applicationId,
        "/jobs/import/",
        Lude.toBS jobId
      ]

instance Lude.ToQuery GetImportJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetImportJobResponse' smart constructor.
data GetImportJobResponse = GetImportJobResponse'
  { importJobResponse :: ImportJobResponse,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetImportJobResponse' with the minimum fields required to make a request.
--
-- * 'importJobResponse' -
-- * 'responseStatus' - The response status code.
mkGetImportJobResponse ::
  -- | 'importJobResponse'
  ImportJobResponse ->
  -- | 'responseStatus'
  Lude.Int ->
  GetImportJobResponse
mkGetImportJobResponse pImportJobResponse_ pResponseStatus_ =
  GetImportJobResponse'
    { importJobResponse = pImportJobResponse_,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'importJobResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gijrsImportJobResponse :: Lens.Lens' GetImportJobResponse ImportJobResponse
gijrsImportJobResponse = Lens.lens (importJobResponse :: GetImportJobResponse -> ImportJobResponse) (\s a -> s {importJobResponse = a} :: GetImportJobResponse)
{-# DEPRECATED gijrsImportJobResponse "Use generic-lens or generic-optics with 'importJobResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gijrsResponseStatus :: Lens.Lens' GetImportJobResponse Lude.Int
gijrsResponseStatus = Lens.lens (responseStatus :: GetImportJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetImportJobResponse)
{-# DEPRECATED gijrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
