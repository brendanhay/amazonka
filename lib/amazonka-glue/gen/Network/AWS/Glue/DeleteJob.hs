{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.DeleteJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified job definition. If the job definition is not found, no exception is thrown.
module Network.AWS.Glue.DeleteJob
  ( -- * Creating a request
    DeleteJob (..),
    mkDeleteJob,

    -- ** Request lenses
    djJobName,

    -- * Destructuring the response
    DeleteJobResponse (..),
    mkDeleteJobResponse,

    -- ** Response lenses
    djrsJobName,
    djrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteJob' smart constructor.
newtype DeleteJob = DeleteJob'
  { -- | The name of the job definition to delete.
    jobName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteJob' with the minimum fields required to make a request.
--
-- * 'jobName' - The name of the job definition to delete.
mkDeleteJob ::
  -- | 'jobName'
  Lude.Text ->
  DeleteJob
mkDeleteJob pJobName_ = DeleteJob' {jobName = pJobName_}

-- | The name of the job definition to delete.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djJobName :: Lens.Lens' DeleteJob Lude.Text
djJobName = Lens.lens (jobName :: DeleteJob -> Lude.Text) (\s a -> s {jobName = a} :: DeleteJob)
{-# DEPRECATED djJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

instance Lude.AWSRequest DeleteJob where
  type Rs DeleteJob = DeleteJobResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteJobResponse'
            Lude.<$> (x Lude..?> "JobName") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target" Lude.=# ("AWSGlue.DeleteJob" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteJob where
  toJSON DeleteJob' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("JobName" Lude..= jobName)])

instance Lude.ToPath DeleteJob where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteJobResponse' smart constructor.
data DeleteJobResponse = DeleteJobResponse'
  { -- | The name of the job definition that was deleted.
    jobName :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteJobResponse' with the minimum fields required to make a request.
--
-- * 'jobName' - The name of the job definition that was deleted.
-- * 'responseStatus' - The response status code.
mkDeleteJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteJobResponse
mkDeleteJobResponse pResponseStatus_ =
  DeleteJobResponse'
    { jobName = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The name of the job definition that was deleted.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djrsJobName :: Lens.Lens' DeleteJobResponse (Lude.Maybe Lude.Text)
djrsJobName = Lens.lens (jobName :: DeleteJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {jobName = a} :: DeleteJobResponse)
{-# DEPRECATED djrsJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djrsResponseStatus :: Lens.Lens' DeleteJobResponse Lude.Int
djrsResponseStatus = Lens.lens (responseStatus :: DeleteJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteJobResponse)
{-# DEPRECATED djrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
