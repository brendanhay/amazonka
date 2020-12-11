{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.GetJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve the JSON for a specific completed transcoding job.
module Network.AWS.MediaConvert.GetJob
  ( -- * Creating a request
    GetJob (..),
    mkGetJob,

    -- ** Request lenses
    gjId,

    -- * Destructuring the response
    GetJobResponse (..),
    mkGetJobResponse,

    -- ** Response lenses
    gjrsJob,
    gjrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetJob' smart constructor.
newtype GetJob = GetJob' {id :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetJob' with the minimum fields required to make a request.
--
-- * 'id' - the job ID of the job.
mkGetJob ::
  -- | 'id'
  Lude.Text ->
  GetJob
mkGetJob pId_ = GetJob' {id = pId_}

-- | the job ID of the job.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjId :: Lens.Lens' GetJob Lude.Text
gjId = Lens.lens (id :: GetJob -> Lude.Text) (\s a -> s {id = a} :: GetJob)
{-# DEPRECATED gjId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest GetJob where
  type Rs GetJob = GetJobResponse
  request = Req.get mediaConvertService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetJobResponse'
            Lude.<$> (x Lude..?> "job") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetJob where
  toPath GetJob' {..} =
    Lude.mconcat ["/2017-08-29/jobs/", Lude.toBS id]

instance Lude.ToQuery GetJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetJobResponse' smart constructor.
data GetJobResponse = GetJobResponse'
  { job :: Lude.Maybe Job,
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

-- | Creates a value of 'GetJobResponse' with the minimum fields required to make a request.
--
-- * 'job' - Each job converts an input file into an output file or files. For more information, see the User Guide at https://docs.aws.amazon.com/mediaconvert/latest/ug/what-is.html
-- * 'responseStatus' - The response status code.
mkGetJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetJobResponse
mkGetJobResponse pResponseStatus_ =
  GetJobResponse'
    { job = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Each job converts an input file into an output file or files. For more information, see the User Guide at https://docs.aws.amazon.com/mediaconvert/latest/ug/what-is.html
--
-- /Note:/ Consider using 'job' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjrsJob :: Lens.Lens' GetJobResponse (Lude.Maybe Job)
gjrsJob = Lens.lens (job :: GetJobResponse -> Lude.Maybe Job) (\s a -> s {job = a} :: GetJobResponse)
{-# DEPRECATED gjrsJob "Use generic-lens or generic-optics with 'job' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjrsResponseStatus :: Lens.Lens' GetJobResponse Lude.Int
gjrsResponseStatus = Lens.lens (responseStatus :: GetJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetJobResponse)
{-# DEPRECATED gjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
