{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.CancelJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently cancel a job. Once you have canceled a job, you can't start it again.
module Network.AWS.MediaConvert.CancelJob
  ( -- * Creating a request
    CancelJob (..),
    mkCancelJob,

    -- ** Request lenses
    cjId,

    -- * Destructuring the response
    CancelJobResponse (..),
    mkCancelJobResponse,

    -- ** Response lenses
    crsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCancelJob' smart constructor.
newtype CancelJob = CancelJob'
  { -- | The Job ID of the job to be cancelled.
    id :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CancelJob' with the minimum fields required to make a request.
--
-- * 'id' - The Job ID of the job to be cancelled.
mkCancelJob ::
  -- | 'id'
  Lude.Text ->
  CancelJob
mkCancelJob pId_ = CancelJob' {id = pId_}

-- | The Job ID of the job to be cancelled.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjId :: Lens.Lens' CancelJob Lude.Text
cjId = Lens.lens (id :: CancelJob -> Lude.Text) (\s a -> s {id = a} :: CancelJob)
{-# DEPRECATED cjId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest CancelJob where
  type Rs CancelJob = CancelJobResponse
  request = Req.delete mediaConvertService
  response =
    Res.receiveEmpty
      ( \s h x ->
          CancelJobResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CancelJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath CancelJob where
  toPath CancelJob' {..} =
    Lude.mconcat ["/2017-08-29/jobs/", Lude.toBS id]

instance Lude.ToQuery CancelJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCancelJobResponse' smart constructor.
newtype CancelJobResponse = CancelJobResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
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
crsResponseStatus :: Lens.Lens' CancelJobResponse Lude.Int
crsResponseStatus = Lens.lens (responseStatus :: CancelJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CancelJobResponse)
{-# DEPRECATED crsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
