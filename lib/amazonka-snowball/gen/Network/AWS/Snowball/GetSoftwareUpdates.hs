{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.GetSoftwareUpdates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an Amazon S3 presigned URL for an update file associated with a specified @JobId@ .
module Network.AWS.Snowball.GetSoftwareUpdates
  ( -- * Creating a request
    GetSoftwareUpdates (..),
    mkGetSoftwareUpdates,

    -- ** Request lenses
    gsuJobId,

    -- * Destructuring the response
    GetSoftwareUpdatesResponse (..),
    mkGetSoftwareUpdatesResponse,

    -- ** Response lenses
    grsUpdatesURI,
    grsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Snowball.Types

-- | /See:/ 'mkGetSoftwareUpdates' smart constructor.
newtype GetSoftwareUpdates = GetSoftwareUpdates'
  { -- | The ID for a job that you want to get the software update file for, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
    jobId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSoftwareUpdates' with the minimum fields required to make a request.
--
-- * 'jobId' - The ID for a job that you want to get the software update file for, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
mkGetSoftwareUpdates ::
  -- | 'jobId'
  Lude.Text ->
  GetSoftwareUpdates
mkGetSoftwareUpdates pJobId_ = GetSoftwareUpdates' {jobId = pJobId_}

-- | The ID for a job that you want to get the software update file for, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsuJobId :: Lens.Lens' GetSoftwareUpdates Lude.Text
gsuJobId = Lens.lens (jobId :: GetSoftwareUpdates -> Lude.Text) (\s a -> s {jobId = a} :: GetSoftwareUpdates)
{-# DEPRECATED gsuJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

instance Lude.AWSRequest GetSoftwareUpdates where
  type Rs GetSoftwareUpdates = GetSoftwareUpdatesResponse
  request = Req.postJSON snowballService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetSoftwareUpdatesResponse'
            Lude.<$> (x Lude..?> "UpdatesURI") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetSoftwareUpdates where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSIESnowballJobManagementService.GetSoftwareUpdates" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetSoftwareUpdates where
  toJSON GetSoftwareUpdates' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("JobId" Lude..= jobId)])

instance Lude.ToPath GetSoftwareUpdates where
  toPath = Lude.const "/"

instance Lude.ToQuery GetSoftwareUpdates where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetSoftwareUpdatesResponse' smart constructor.
data GetSoftwareUpdatesResponse = GetSoftwareUpdatesResponse'
  { -- | The Amazon S3 presigned URL for the update file associated with the specified @JobId@ value. The software update will be available for 2 days after this request is made. To access an update after the 2 days have passed, you'll have to make another call to @GetSoftwareUpdates@ .
    updatesURI :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSoftwareUpdatesResponse' with the minimum fields required to make a request.
--
-- * 'updatesURI' - The Amazon S3 presigned URL for the update file associated with the specified @JobId@ value. The software update will be available for 2 days after this request is made. To access an update after the 2 days have passed, you'll have to make another call to @GetSoftwareUpdates@ .
-- * 'responseStatus' - The response status code.
mkGetSoftwareUpdatesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetSoftwareUpdatesResponse
mkGetSoftwareUpdatesResponse pResponseStatus_ =
  GetSoftwareUpdatesResponse'
    { updatesURI = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon S3 presigned URL for the update file associated with the specified @JobId@ value. The software update will be available for 2 days after this request is made. To access an update after the 2 days have passed, you'll have to make another call to @GetSoftwareUpdates@ .
--
-- /Note:/ Consider using 'updatesURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsUpdatesURI :: Lens.Lens' GetSoftwareUpdatesResponse (Lude.Maybe Lude.Text)
grsUpdatesURI = Lens.lens (updatesURI :: GetSoftwareUpdatesResponse -> Lude.Maybe Lude.Text) (\s a -> s {updatesURI = a} :: GetSoftwareUpdatesResponse)
{-# DEPRECATED grsUpdatesURI "Use generic-lens or generic-optics with 'updatesURI' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsResponseStatus :: Lens.Lens' GetSoftwareUpdatesResponse Lude.Int
grsResponseStatus = Lens.lens (responseStatus :: GetSoftwareUpdatesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetSoftwareUpdatesResponse)
{-# DEPRECATED grsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
