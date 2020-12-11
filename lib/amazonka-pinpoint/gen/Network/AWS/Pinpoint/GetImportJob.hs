{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    gijApplicationId,
    gijJobId,

    -- * Destructuring the response
    GetImportJobResponse (..),
    mkGetImportJobResponse,

    -- ** Response lenses
    gijrsResponseStatus,
    gijrsImportJobResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetImportJob' smart constructor.
data GetImportJob = GetImportJob'
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

-- | Creates a value of 'GetImportJob' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
-- * 'jobId' - The unique identifier for the job.
mkGetImportJob ::
  -- | 'applicationId'
  Lude.Text ->
  -- | 'jobId'
  Lude.Text ->
  GetImportJob
mkGetImportJob pApplicationId_ pJobId_ =
  GetImportJob' {applicationId = pApplicationId_, jobId = pJobId_}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gijApplicationId :: Lens.Lens' GetImportJob Lude.Text
gijApplicationId = Lens.lens (applicationId :: GetImportJob -> Lude.Text) (\s a -> s {applicationId = a} :: GetImportJob)
{-# DEPRECATED gijApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The unique identifier for the job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gijJobId :: Lens.Lens' GetImportJob Lude.Text
gijJobId = Lens.lens (jobId :: GetImportJob -> Lude.Text) (\s a -> s {jobId = a} :: GetImportJob)
{-# DEPRECATED gijJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

instance Lude.AWSRequest GetImportJob where
  type Rs GetImportJob = GetImportJobResponse
  request = Req.get pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetImportJobResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (Lude.eitherParseJSON x)
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
  { responseStatus ::
      Lude.Int,
    importJobResponse :: ImportJobResponse
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetImportJobResponse' with the minimum fields required to make a request.
--
-- * 'importJobResponse' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkGetImportJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'importJobResponse'
  ImportJobResponse ->
  GetImportJobResponse
mkGetImportJobResponse pResponseStatus_ pImportJobResponse_ =
  GetImportJobResponse'
    { responseStatus = pResponseStatus_,
      importJobResponse = pImportJobResponse_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gijrsResponseStatus :: Lens.Lens' GetImportJobResponse Lude.Int
gijrsResponseStatus = Lens.lens (responseStatus :: GetImportJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetImportJobResponse)
{-# DEPRECATED gijrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'importJobResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gijrsImportJobResponse :: Lens.Lens' GetImportJobResponse ImportJobResponse
gijrsImportJobResponse = Lens.lens (importJobResponse :: GetImportJobResponse -> ImportJobResponse) (\s a -> s {importJobResponse = a} :: GetImportJobResponse)
{-# DEPRECATED gijrsImportJobResponse "Use generic-lens or generic-optics with 'importJobResponse' instead." #-}
