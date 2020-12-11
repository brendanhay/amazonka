{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.DeleteReplicationJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified replication job.
--
-- After you delete a replication job, there are no further replication runs. AWS deletes the contents of the Amazon S3 bucket used to store AWS SMS artifacts. The AMIs created by the replication runs are not deleted.
module Network.AWS.SMS.DeleteReplicationJob
  ( -- * Creating a request
    DeleteReplicationJob (..),
    mkDeleteReplicationJob,

    -- ** Request lenses
    drjReplicationJobId,

    -- * Destructuring the response
    DeleteReplicationJobResponse (..),
    mkDeleteReplicationJobResponse,

    -- ** Response lenses
    drjrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SMS.Types

-- | /See:/ 'mkDeleteReplicationJob' smart constructor.
newtype DeleteReplicationJob = DeleteReplicationJob'
  { replicationJobId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteReplicationJob' with the minimum fields required to make a request.
--
-- * 'replicationJobId' - The ID of the replication job.
mkDeleteReplicationJob ::
  -- | 'replicationJobId'
  Lude.Text ->
  DeleteReplicationJob
mkDeleteReplicationJob pReplicationJobId_ =
  DeleteReplicationJob' {replicationJobId = pReplicationJobId_}

-- | The ID of the replication job.
--
-- /Note:/ Consider using 'replicationJobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drjReplicationJobId :: Lens.Lens' DeleteReplicationJob Lude.Text
drjReplicationJobId = Lens.lens (replicationJobId :: DeleteReplicationJob -> Lude.Text) (\s a -> s {replicationJobId = a} :: DeleteReplicationJob)
{-# DEPRECATED drjReplicationJobId "Use generic-lens or generic-optics with 'replicationJobId' instead." #-}

instance Lude.AWSRequest DeleteReplicationJob where
  type Rs DeleteReplicationJob = DeleteReplicationJobResponse
  request = Req.postJSON smsService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteReplicationJobResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteReplicationJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSServerMigrationService_V2016_10_24.DeleteReplicationJob" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteReplicationJob where
  toJSON DeleteReplicationJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("replicationJobId" Lude..= replicationJobId)]
      )

instance Lude.ToPath DeleteReplicationJob where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteReplicationJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteReplicationJobResponse' smart constructor.
newtype DeleteReplicationJobResponse = DeleteReplicationJobResponse'
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

-- | Creates a value of 'DeleteReplicationJobResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteReplicationJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteReplicationJobResponse
mkDeleteReplicationJobResponse pResponseStatus_ =
  DeleteReplicationJobResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drjrsResponseStatus :: Lens.Lens' DeleteReplicationJobResponse Lude.Int
drjrsResponseStatus = Lens.lens (responseStatus :: DeleteReplicationJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteReplicationJobResponse)
{-# DEPRECATED drjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
