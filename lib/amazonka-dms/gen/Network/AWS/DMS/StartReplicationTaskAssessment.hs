{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.StartReplicationTaskAssessment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the replication task assessment for unsupported data types in the source database.
module Network.AWS.DMS.StartReplicationTaskAssessment
  ( -- * Creating a request
    StartReplicationTaskAssessment (..),
    mkStartReplicationTaskAssessment,

    -- ** Request lenses
    srtaReplicationTaskARN,

    -- * Destructuring the response
    StartReplicationTaskAssessmentResponse (..),
    mkStartReplicationTaskAssessmentResponse,

    -- ** Response lenses
    srtarsReplicationTask,
    srtarsResponseStatus,
  )
where

import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkStartReplicationTaskAssessment' smart constructor.
newtype StartReplicationTaskAssessment = StartReplicationTaskAssessment'
  { -- | The Amazon Resource Name (ARN) of the replication task.
    replicationTaskARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartReplicationTaskAssessment' with the minimum fields required to make a request.
--
-- * 'replicationTaskARN' - The Amazon Resource Name (ARN) of the replication task.
mkStartReplicationTaskAssessment ::
  -- | 'replicationTaskARN'
  Lude.Text ->
  StartReplicationTaskAssessment
mkStartReplicationTaskAssessment pReplicationTaskARN_ =
  StartReplicationTaskAssessment'
    { replicationTaskARN =
        pReplicationTaskARN_
    }

-- | The Amazon Resource Name (ARN) of the replication task.
--
-- /Note:/ Consider using 'replicationTaskARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srtaReplicationTaskARN :: Lens.Lens' StartReplicationTaskAssessment Lude.Text
srtaReplicationTaskARN = Lens.lens (replicationTaskARN :: StartReplicationTaskAssessment -> Lude.Text) (\s a -> s {replicationTaskARN = a} :: StartReplicationTaskAssessment)
{-# DEPRECATED srtaReplicationTaskARN "Use generic-lens or generic-optics with 'replicationTaskARN' instead." #-}

instance Lude.AWSRequest StartReplicationTaskAssessment where
  type
    Rs StartReplicationTaskAssessment =
      StartReplicationTaskAssessmentResponse
  request = Req.postJSON dmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartReplicationTaskAssessmentResponse'
            Lude.<$> (x Lude..?> "ReplicationTask")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartReplicationTaskAssessment where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonDMSv20160101.StartReplicationTaskAssessment" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartReplicationTaskAssessment where
  toJSON StartReplicationTaskAssessment' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("ReplicationTaskArn" Lude..= replicationTaskARN)]
      )

instance Lude.ToPath StartReplicationTaskAssessment where
  toPath = Lude.const "/"

instance Lude.ToQuery StartReplicationTaskAssessment where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkStartReplicationTaskAssessmentResponse' smart constructor.
data StartReplicationTaskAssessmentResponse = StartReplicationTaskAssessmentResponse'
  { -- | The assessed replication task.
    replicationTask :: Lude.Maybe ReplicationTask,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartReplicationTaskAssessmentResponse' with the minimum fields required to make a request.
--
-- * 'replicationTask' - The assessed replication task.
-- * 'responseStatus' - The response status code.
mkStartReplicationTaskAssessmentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartReplicationTaskAssessmentResponse
mkStartReplicationTaskAssessmentResponse pResponseStatus_ =
  StartReplicationTaskAssessmentResponse'
    { replicationTask =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The assessed replication task.
--
-- /Note:/ Consider using 'replicationTask' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srtarsReplicationTask :: Lens.Lens' StartReplicationTaskAssessmentResponse (Lude.Maybe ReplicationTask)
srtarsReplicationTask = Lens.lens (replicationTask :: StartReplicationTaskAssessmentResponse -> Lude.Maybe ReplicationTask) (\s a -> s {replicationTask = a} :: StartReplicationTaskAssessmentResponse)
{-# DEPRECATED srtarsReplicationTask "Use generic-lens or generic-optics with 'replicationTask' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srtarsResponseStatus :: Lens.Lens' StartReplicationTaskAssessmentResponse Lude.Int
srtarsResponseStatus = Lens.lens (responseStatus :: StartReplicationTaskAssessmentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartReplicationTaskAssessmentResponse)
{-# DEPRECATED srtarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
