{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.GetJobManifest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a link to an Amazon S3 presigned URL for the manifest file associated with the specified @JobId@ value. You can access the manifest file for up to 60 minutes after this request has been made. To access the manifest file after 60 minutes have passed, you'll have to make another call to the @GetJobManifest@ action.
--
-- The manifest is an encrypted file that you can download after your job enters the @WithCustomer@ status. The manifest is decrypted by using the @UnlockCode@ code value, when you pass both values to the Snow device through the Snowball client when the client is started for the first time.
-- As a best practice, we recommend that you don't save a copy of an @UnlockCode@ value in the same location as the manifest file for that job. Saving these separately helps prevent unauthorized parties from gaining access to the Snow device associated with that job.
-- The credentials of a given job, including its manifest file and unlock code, expire 90 days after the job is created.
module Network.AWS.Snowball.GetJobManifest
  ( -- * Creating a request
    GetJobManifest (..),
    mkGetJobManifest,

    -- ** Request lenses
    gjmJobId,

    -- * Destructuring the response
    GetJobManifestResponse (..),
    mkGetJobManifestResponse,

    -- ** Response lenses
    gjmrsManifestURI,
    gjmrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Snowball.Types

-- | /See:/ 'mkGetJobManifest' smart constructor.
newtype GetJobManifest = GetJobManifest' {jobId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetJobManifest' with the minimum fields required to make a request.
--
-- * 'jobId' - The ID for a job that you want to get the manifest file for, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
mkGetJobManifest ::
  -- | 'jobId'
  Lude.Text ->
  GetJobManifest
mkGetJobManifest pJobId_ = GetJobManifest' {jobId = pJobId_}

-- | The ID for a job that you want to get the manifest file for, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjmJobId :: Lens.Lens' GetJobManifest Lude.Text
gjmJobId = Lens.lens (jobId :: GetJobManifest -> Lude.Text) (\s a -> s {jobId = a} :: GetJobManifest)
{-# DEPRECATED gjmJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

instance Lude.AWSRequest GetJobManifest where
  type Rs GetJobManifest = GetJobManifestResponse
  request = Req.postJSON snowballService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetJobManifestResponse'
            Lude.<$> (x Lude..?> "ManifestURI") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetJobManifest where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSIESnowballJobManagementService.GetJobManifest" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetJobManifest where
  toJSON GetJobManifest' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("JobId" Lude..= jobId)])

instance Lude.ToPath GetJobManifest where
  toPath = Lude.const "/"

instance Lude.ToQuery GetJobManifest where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetJobManifestResponse' smart constructor.
data GetJobManifestResponse = GetJobManifestResponse'
  { manifestURI ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetJobManifestResponse' with the minimum fields required to make a request.
--
-- * 'manifestURI' - The Amazon S3 presigned URL for the manifest file associated with the specified @JobId@ value.
-- * 'responseStatus' - The response status code.
mkGetJobManifestResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetJobManifestResponse
mkGetJobManifestResponse pResponseStatus_ =
  GetJobManifestResponse'
    { manifestURI = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon S3 presigned URL for the manifest file associated with the specified @JobId@ value.
--
-- /Note:/ Consider using 'manifestURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjmrsManifestURI :: Lens.Lens' GetJobManifestResponse (Lude.Maybe Lude.Text)
gjmrsManifestURI = Lens.lens (manifestURI :: GetJobManifestResponse -> Lude.Maybe Lude.Text) (\s a -> s {manifestURI = a} :: GetJobManifestResponse)
{-# DEPRECATED gjmrsManifestURI "Use generic-lens or generic-optics with 'manifestURI' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjmrsResponseStatus :: Lens.Lens' GetJobManifestResponse Lude.Int
gjmrsResponseStatus = Lens.lens (responseStatus :: GetJobManifestResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetJobManifestResponse)
{-# DEPRECATED gjmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
