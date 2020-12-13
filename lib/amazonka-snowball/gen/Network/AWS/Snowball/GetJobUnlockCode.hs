{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.GetJobUnlockCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the @UnlockCode@ code value for the specified job. A particular @UnlockCode@ value can be accessed for up to 90 days after the associated job has been created.
--
-- The @UnlockCode@ value is a 29-character code with 25 alphanumeric characters and 4 hyphens. This code is used to decrypt the manifest file when it is passed along with the manifest to the Snow device through the Snowball client when the client is started for the first time.
-- As a best practice, we recommend that you don't save a copy of the @UnlockCode@ in the same location as the manifest file for that job. Saving these separately helps prevent unauthorized parties from gaining access to the Snow device associated with that job.
module Network.AWS.Snowball.GetJobUnlockCode
  ( -- * Creating a request
    GetJobUnlockCode (..),
    mkGetJobUnlockCode,

    -- ** Request lenses
    gjucJobId,

    -- * Destructuring the response
    GetJobUnlockCodeResponse (..),
    mkGetJobUnlockCodeResponse,

    -- ** Response lenses
    gjucrsUnlockCode,
    gjucrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Snowball.Types

-- | /See:/ 'mkGetJobUnlockCode' smart constructor.
newtype GetJobUnlockCode = GetJobUnlockCode'
  { -- | The ID for the job that you want to get the @UnlockCode@ value for, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
    jobId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetJobUnlockCode' with the minimum fields required to make a request.
--
-- * 'jobId' - The ID for the job that you want to get the @UnlockCode@ value for, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
mkGetJobUnlockCode ::
  -- | 'jobId'
  Lude.Text ->
  GetJobUnlockCode
mkGetJobUnlockCode pJobId_ = GetJobUnlockCode' {jobId = pJobId_}

-- | The ID for the job that you want to get the @UnlockCode@ value for, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjucJobId :: Lens.Lens' GetJobUnlockCode Lude.Text
gjucJobId = Lens.lens (jobId :: GetJobUnlockCode -> Lude.Text) (\s a -> s {jobId = a} :: GetJobUnlockCode)
{-# DEPRECATED gjucJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

instance Lude.AWSRequest GetJobUnlockCode where
  type Rs GetJobUnlockCode = GetJobUnlockCodeResponse
  request = Req.postJSON snowballService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetJobUnlockCodeResponse'
            Lude.<$> (x Lude..?> "UnlockCode") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetJobUnlockCode where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSIESnowballJobManagementService.GetJobUnlockCode" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetJobUnlockCode where
  toJSON GetJobUnlockCode' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("JobId" Lude..= jobId)])

instance Lude.ToPath GetJobUnlockCode where
  toPath = Lude.const "/"

instance Lude.ToQuery GetJobUnlockCode where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetJobUnlockCodeResponse' smart constructor.
data GetJobUnlockCodeResponse = GetJobUnlockCodeResponse'
  { -- | The @UnlockCode@ value for the specified job. The @UnlockCode@ value can be accessed for up to 90 days after the job has been created.
    unlockCode :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetJobUnlockCodeResponse' with the minimum fields required to make a request.
--
-- * 'unlockCode' - The @UnlockCode@ value for the specified job. The @UnlockCode@ value can be accessed for up to 90 days after the job has been created.
-- * 'responseStatus' - The response status code.
mkGetJobUnlockCodeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetJobUnlockCodeResponse
mkGetJobUnlockCodeResponse pResponseStatus_ =
  GetJobUnlockCodeResponse'
    { unlockCode = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @UnlockCode@ value for the specified job. The @UnlockCode@ value can be accessed for up to 90 days after the job has been created.
--
-- /Note:/ Consider using 'unlockCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjucrsUnlockCode :: Lens.Lens' GetJobUnlockCodeResponse (Lude.Maybe Lude.Text)
gjucrsUnlockCode = Lens.lens (unlockCode :: GetJobUnlockCodeResponse -> Lude.Maybe Lude.Text) (\s a -> s {unlockCode = a} :: GetJobUnlockCodeResponse)
{-# DEPRECATED gjucrsUnlockCode "Use generic-lens or generic-optics with 'unlockCode' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjucrsResponseStatus :: Lens.Lens' GetJobUnlockCodeResponse Lude.Int
gjucrsResponseStatus = Lens.lens (responseStatus :: GetJobUnlockCodeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetJobUnlockCodeResponse)
{-# DEPRECATED gjucrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
