{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.DeleteReplicationTaskAssessmentRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the record of a single premigration assessment run.
--
-- This operation removes all metadata that AWS DMS maintains about this assessment run. However, the operation leaves untouched all information about this assessment run that is stored in your Amazon S3 bucket.
module Network.AWS.DMS.DeleteReplicationTaskAssessmentRun
  ( -- * Creating a request
    DeleteReplicationTaskAssessmentRun (..),
    mkDeleteReplicationTaskAssessmentRun,

    -- ** Request lenses
    drtarReplicationTaskAssessmentRunARN,

    -- * Destructuring the response
    DeleteReplicationTaskAssessmentRunResponse (..),
    mkDeleteReplicationTaskAssessmentRunResponse,

    -- ** Response lenses
    drtarrsReplicationTaskAssessmentRun,
    drtarrsResponseStatus,
  )
where

import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDeleteReplicationTaskAssessmentRun' smart constructor.
newtype DeleteReplicationTaskAssessmentRun = DeleteReplicationTaskAssessmentRun'
  { replicationTaskAssessmentRunARN ::
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

-- | Creates a value of 'DeleteReplicationTaskAssessmentRun' with the minimum fields required to make a request.
--
-- * 'replicationTaskAssessmentRunARN' - Amazon Resource Name (ARN) of the premigration assessment run to be deleted.
mkDeleteReplicationTaskAssessmentRun ::
  -- | 'replicationTaskAssessmentRunARN'
  Lude.Text ->
  DeleteReplicationTaskAssessmentRun
mkDeleteReplicationTaskAssessmentRun
  pReplicationTaskAssessmentRunARN_ =
    DeleteReplicationTaskAssessmentRun'
      { replicationTaskAssessmentRunARN =
          pReplicationTaskAssessmentRunARN_
      }

-- | Amazon Resource Name (ARN) of the premigration assessment run to be deleted.
--
-- /Note:/ Consider using 'replicationTaskAssessmentRunARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtarReplicationTaskAssessmentRunARN :: Lens.Lens' DeleteReplicationTaskAssessmentRun Lude.Text
drtarReplicationTaskAssessmentRunARN = Lens.lens (replicationTaskAssessmentRunARN :: DeleteReplicationTaskAssessmentRun -> Lude.Text) (\s a -> s {replicationTaskAssessmentRunARN = a} :: DeleteReplicationTaskAssessmentRun)
{-# DEPRECATED drtarReplicationTaskAssessmentRunARN "Use generic-lens or generic-optics with 'replicationTaskAssessmentRunARN' instead." #-}

instance Lude.AWSRequest DeleteReplicationTaskAssessmentRun where
  type
    Rs DeleteReplicationTaskAssessmentRun =
      DeleteReplicationTaskAssessmentRunResponse
  request = Req.postJSON dmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteReplicationTaskAssessmentRunResponse'
            Lude.<$> (x Lude..?> "ReplicationTaskAssessmentRun")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteReplicationTaskAssessmentRun where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonDMSv20160101.DeleteReplicationTaskAssessmentRun" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteReplicationTaskAssessmentRun where
  toJSON DeleteReplicationTaskAssessmentRun' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ( "ReplicationTaskAssessmentRunArn"
                  Lude..= replicationTaskAssessmentRunARN
              )
          ]
      )

instance Lude.ToPath DeleteReplicationTaskAssessmentRun where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteReplicationTaskAssessmentRun where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkDeleteReplicationTaskAssessmentRunResponse' smart constructor.
data DeleteReplicationTaskAssessmentRunResponse = DeleteReplicationTaskAssessmentRunResponse'
  { replicationTaskAssessmentRun ::
      Lude.Maybe
        ReplicationTaskAssessmentRun,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteReplicationTaskAssessmentRunResponse' with the minimum fields required to make a request.
--
-- * 'replicationTaskAssessmentRun' - The @ReplicationTaskAssessmentRun@ object for the deleted assessment run.
-- * 'responseStatus' - The response status code.
mkDeleteReplicationTaskAssessmentRunResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteReplicationTaskAssessmentRunResponse
mkDeleteReplicationTaskAssessmentRunResponse pResponseStatus_ =
  DeleteReplicationTaskAssessmentRunResponse'
    { replicationTaskAssessmentRun =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @ReplicationTaskAssessmentRun@ object for the deleted assessment run.
--
-- /Note:/ Consider using 'replicationTaskAssessmentRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtarrsReplicationTaskAssessmentRun :: Lens.Lens' DeleteReplicationTaskAssessmentRunResponse (Lude.Maybe ReplicationTaskAssessmentRun)
drtarrsReplicationTaskAssessmentRun = Lens.lens (replicationTaskAssessmentRun :: DeleteReplicationTaskAssessmentRunResponse -> Lude.Maybe ReplicationTaskAssessmentRun) (\s a -> s {replicationTaskAssessmentRun = a} :: DeleteReplicationTaskAssessmentRunResponse)
{-# DEPRECATED drtarrsReplicationTaskAssessmentRun "Use generic-lens or generic-optics with 'replicationTaskAssessmentRun' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtarrsResponseStatus :: Lens.Lens' DeleteReplicationTaskAssessmentRunResponse Lude.Int
drtarrsResponseStatus = Lens.lens (responseStatus :: DeleteReplicationTaskAssessmentRunResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteReplicationTaskAssessmentRunResponse)
{-# DEPRECATED drtarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
