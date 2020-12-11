{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.CancelReplicationTaskAssessmentRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a single premigration assessment run.
--
-- This operation prevents any individual assessments from running if they haven't started running. It also attempts to cancel any individual assessments that are currently running.
module Network.AWS.DMS.CancelReplicationTaskAssessmentRun
  ( -- * Creating a request
    CancelReplicationTaskAssessmentRun (..),
    mkCancelReplicationTaskAssessmentRun,

    -- ** Request lenses
    crtarReplicationTaskAssessmentRunARN,

    -- * Destructuring the response
    CancelReplicationTaskAssessmentRunResponse (..),
    mkCancelReplicationTaskAssessmentRunResponse,

    -- ** Response lenses
    crtarrsReplicationTaskAssessmentRun,
    crtarrsResponseStatus,
  )
where

import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkCancelReplicationTaskAssessmentRun' smart constructor.
newtype CancelReplicationTaskAssessmentRun = CancelReplicationTaskAssessmentRun'
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

-- | Creates a value of 'CancelReplicationTaskAssessmentRun' with the minimum fields required to make a request.
--
-- * 'replicationTaskAssessmentRunARN' - Amazon Resource Name (ARN) of the premigration assessment run to be canceled.
mkCancelReplicationTaskAssessmentRun ::
  -- | 'replicationTaskAssessmentRunARN'
  Lude.Text ->
  CancelReplicationTaskAssessmentRun
mkCancelReplicationTaskAssessmentRun
  pReplicationTaskAssessmentRunARN_ =
    CancelReplicationTaskAssessmentRun'
      { replicationTaskAssessmentRunARN =
          pReplicationTaskAssessmentRunARN_
      }

-- | Amazon Resource Name (ARN) of the premigration assessment run to be canceled.
--
-- /Note:/ Consider using 'replicationTaskAssessmentRunARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtarReplicationTaskAssessmentRunARN :: Lens.Lens' CancelReplicationTaskAssessmentRun Lude.Text
crtarReplicationTaskAssessmentRunARN = Lens.lens (replicationTaskAssessmentRunARN :: CancelReplicationTaskAssessmentRun -> Lude.Text) (\s a -> s {replicationTaskAssessmentRunARN = a} :: CancelReplicationTaskAssessmentRun)
{-# DEPRECATED crtarReplicationTaskAssessmentRunARN "Use generic-lens or generic-optics with 'replicationTaskAssessmentRunARN' instead." #-}

instance Lude.AWSRequest CancelReplicationTaskAssessmentRun where
  type
    Rs CancelReplicationTaskAssessmentRun =
      CancelReplicationTaskAssessmentRunResponse
  request = Req.postJSON dmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          CancelReplicationTaskAssessmentRunResponse'
            Lude.<$> (x Lude..?> "ReplicationTaskAssessmentRun")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CancelReplicationTaskAssessmentRun where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonDMSv20160101.CancelReplicationTaskAssessmentRun" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CancelReplicationTaskAssessmentRun where
  toJSON CancelReplicationTaskAssessmentRun' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ( "ReplicationTaskAssessmentRunArn"
                  Lude..= replicationTaskAssessmentRunARN
              )
          ]
      )

instance Lude.ToPath CancelReplicationTaskAssessmentRun where
  toPath = Lude.const "/"

instance Lude.ToQuery CancelReplicationTaskAssessmentRun where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkCancelReplicationTaskAssessmentRunResponse' smart constructor.
data CancelReplicationTaskAssessmentRunResponse = CancelReplicationTaskAssessmentRunResponse'
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

-- | Creates a value of 'CancelReplicationTaskAssessmentRunResponse' with the minimum fields required to make a request.
--
-- * 'replicationTaskAssessmentRun' - The @ReplicationTaskAssessmentRun@ object for the canceled assessment run.
-- * 'responseStatus' - The response status code.
mkCancelReplicationTaskAssessmentRunResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CancelReplicationTaskAssessmentRunResponse
mkCancelReplicationTaskAssessmentRunResponse pResponseStatus_ =
  CancelReplicationTaskAssessmentRunResponse'
    { replicationTaskAssessmentRun =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @ReplicationTaskAssessmentRun@ object for the canceled assessment run.
--
-- /Note:/ Consider using 'replicationTaskAssessmentRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtarrsReplicationTaskAssessmentRun :: Lens.Lens' CancelReplicationTaskAssessmentRunResponse (Lude.Maybe ReplicationTaskAssessmentRun)
crtarrsReplicationTaskAssessmentRun = Lens.lens (replicationTaskAssessmentRun :: CancelReplicationTaskAssessmentRunResponse -> Lude.Maybe ReplicationTaskAssessmentRun) (\s a -> s {replicationTaskAssessmentRun = a} :: CancelReplicationTaskAssessmentRunResponse)
{-# DEPRECATED crtarrsReplicationTaskAssessmentRun "Use generic-lens or generic-optics with 'replicationTaskAssessmentRun' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtarrsResponseStatus :: Lens.Lens' CancelReplicationTaskAssessmentRunResponse Lude.Int
crtarrsResponseStatus = Lens.lens (responseStatus :: CancelReplicationTaskAssessmentRunResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CancelReplicationTaskAssessmentRunResponse)
{-# DEPRECATED crtarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
