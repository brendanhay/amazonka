{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.ReadJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The ReadJob operation returns detailed information about a job.
module Network.AWS.ElasticTranscoder.ReadJob
  ( -- * Creating a request
    ReadJob (..),
    mkReadJob,

    -- ** Request lenses
    rjId,

    -- * Destructuring the response
    ReadJobResponse (..),
    mkReadJobResponse,

    -- ** Response lenses
    rjrsResponseStatus,
    rjrsJob,
  )
where

import Network.AWS.ElasticTranscoder.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The @ReadJobRequest@ structure.
--
-- /See:/ 'mkReadJob' smart constructor.
newtype ReadJob = ReadJob' {id :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReadJob' with the minimum fields required to make a request.
--
-- * 'id' - The identifier of the job for which you want to get detailed information.
mkReadJob ::
  -- | 'id'
  Lude.Text ->
  ReadJob
mkReadJob pId_ = ReadJob' {id = pId_}

-- | The identifier of the job for which you want to get detailed information.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjId :: Lens.Lens' ReadJob Lude.Text
rjId = Lens.lens (id :: ReadJob -> Lude.Text) (\s a -> s {id = a} :: ReadJob)
{-# DEPRECATED rjId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest ReadJob where
  type Rs ReadJob = ReadJobResponse
  request = Req.get elasticTranscoderService
  response =
    Res.receiveJSON
      ( \s h x ->
          ReadJobResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (x Lude..:> "Job")
      )

instance Lude.ToHeaders ReadJob where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ReadJob where
  toPath ReadJob' {..} =
    Lude.mconcat ["/2012-09-25/jobs/", Lude.toBS id]

instance Lude.ToQuery ReadJob where
  toQuery = Lude.const Lude.mempty

-- | The @ReadJobResponse@ structure.
--
-- /See:/ 'mkReadJobResponse' smart constructor.
data ReadJobResponse = ReadJobResponse'
  { responseStatus :: Lude.Int,
    job :: Job'
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReadJobResponse' with the minimum fields required to make a request.
--
-- * 'job' - A section of the response body that provides information about the job.
-- * 'responseStatus' - The response status code.
mkReadJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'job'
  Job' ->
  ReadJobResponse
mkReadJobResponse pResponseStatus_ pJob_ =
  ReadJobResponse' {responseStatus = pResponseStatus_, job = pJob_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjrsResponseStatus :: Lens.Lens' ReadJobResponse Lude.Int
rjrsResponseStatus = Lens.lens (responseStatus :: ReadJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ReadJobResponse)
{-# DEPRECATED rjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A section of the response body that provides information about the job.
--
-- /Note:/ Consider using 'job' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjrsJob :: Lens.Lens' ReadJobResponse Job'
rjrsJob = Lens.lens (job :: ReadJobResponse -> Job') (\s a -> s {job = a} :: ReadJobResponse)
{-# DEPRECATED rjrsJob "Use generic-lens or generic-optics with 'job' instead." #-}
