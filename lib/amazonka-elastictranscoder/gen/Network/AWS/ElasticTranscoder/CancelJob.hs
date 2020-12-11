{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.CancelJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The CancelJob operation cancels an unfinished job.
module Network.AWS.ElasticTranscoder.CancelJob
  ( -- * Creating a request
    CancelJob (..),
    mkCancelJob,

    -- ** Request lenses
    cjId,

    -- * Destructuring the response
    CancelJobResponse (..),
    mkCancelJobResponse,

    -- ** Response lenses
    canrsResponseStatus,
  )
where

import Network.AWS.ElasticTranscoder.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The @CancelJobRequest@ structure.
--
-- /See:/ 'mkCancelJob' smart constructor.
newtype CancelJob = CancelJob' {id :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CancelJob' with the minimum fields required to make a request.
--
-- * 'id' - The identifier of the job that you want to cancel.
--
-- To get a list of the jobs (including their @jobId@ ) that have a status of @Submitted@ , use the 'ListJobsByStatus' API action.
mkCancelJob ::
  -- | 'id'
  Lude.Text ->
  CancelJob
mkCancelJob pId_ = CancelJob' {id = pId_}

-- | The identifier of the job that you want to cancel.
--
-- To get a list of the jobs (including their @jobId@ ) that have a status of @Submitted@ , use the 'ListJobsByStatus' API action.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjId :: Lens.Lens' CancelJob Lude.Text
cjId = Lens.lens (id :: CancelJob -> Lude.Text) (\s a -> s {id = a} :: CancelJob)
{-# DEPRECATED cjId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest CancelJob where
  type Rs CancelJob = CancelJobResponse
  request = Req.delete elasticTranscoderService
  response =
    Res.receiveEmpty
      ( \s h x ->
          CancelJobResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CancelJob where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CancelJob where
  toPath CancelJob' {..} =
    Lude.mconcat ["/2012-09-25/jobs/", Lude.toBS id]

instance Lude.ToQuery CancelJob where
  toQuery = Lude.const Lude.mempty

-- | The response body contains a JSON object. If the job is successfully canceled, the value of @Success@ is @true@ .
--
-- /See:/ 'mkCancelJobResponse' smart constructor.
newtype CancelJobResponse = CancelJobResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CancelJobResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkCancelJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CancelJobResponse
mkCancelJobResponse pResponseStatus_ =
  CancelJobResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
canrsResponseStatus :: Lens.Lens' CancelJobResponse Lude.Int
canrsResponseStatus = Lens.lens (responseStatus :: CancelJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CancelJobResponse)
{-# DEPRECATED canrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
