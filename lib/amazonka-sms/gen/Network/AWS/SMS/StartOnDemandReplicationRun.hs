{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.StartOnDemandReplicationRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an on-demand replication run for the specified replication job. This replication run starts immediately. This replication run is in addition to the ones already scheduled.
--
-- There is a limit on the number of on-demand replications runs that you can request in a 24-hour period.
module Network.AWS.SMS.StartOnDemandReplicationRun
  ( -- * Creating a request
    StartOnDemandReplicationRun (..),
    mkStartOnDemandReplicationRun,

    -- ** Request lenses
    sodrrReplicationJobId,
    sodrrDescription,

    -- * Destructuring the response
    StartOnDemandReplicationRunResponse (..),
    mkStartOnDemandReplicationRunResponse,

    -- ** Response lenses
    sodrrrsReplicationRunId,
    sodrrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SMS.Types

-- | /See:/ 'mkStartOnDemandReplicationRun' smart constructor.
data StartOnDemandReplicationRun = StartOnDemandReplicationRun'
  { -- | The ID of the replication job.
    replicationJobId :: Lude.Text,
    -- | The description of the replication run.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartOnDemandReplicationRun' with the minimum fields required to make a request.
--
-- * 'replicationJobId' - The ID of the replication job.
-- * 'description' - The description of the replication run.
mkStartOnDemandReplicationRun ::
  -- | 'replicationJobId'
  Lude.Text ->
  StartOnDemandReplicationRun
mkStartOnDemandReplicationRun pReplicationJobId_ =
  StartOnDemandReplicationRun'
    { replicationJobId =
        pReplicationJobId_,
      description = Lude.Nothing
    }

-- | The ID of the replication job.
--
-- /Note:/ Consider using 'replicationJobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sodrrReplicationJobId :: Lens.Lens' StartOnDemandReplicationRun Lude.Text
sodrrReplicationJobId = Lens.lens (replicationJobId :: StartOnDemandReplicationRun -> Lude.Text) (\s a -> s {replicationJobId = a} :: StartOnDemandReplicationRun)
{-# DEPRECATED sodrrReplicationJobId "Use generic-lens or generic-optics with 'replicationJobId' instead." #-}

-- | The description of the replication run.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sodrrDescription :: Lens.Lens' StartOnDemandReplicationRun (Lude.Maybe Lude.Text)
sodrrDescription = Lens.lens (description :: StartOnDemandReplicationRun -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: StartOnDemandReplicationRun)
{-# DEPRECATED sodrrDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.AWSRequest StartOnDemandReplicationRun where
  type
    Rs StartOnDemandReplicationRun =
      StartOnDemandReplicationRunResponse
  request = Req.postJSON smsService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartOnDemandReplicationRunResponse'
            Lude.<$> (x Lude..?> "replicationRunId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartOnDemandReplicationRun where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSServerMigrationService_V2016_10_24.StartOnDemandReplicationRun" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartOnDemandReplicationRun where
  toJSON StartOnDemandReplicationRun' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("replicationJobId" Lude..= replicationJobId),
            ("description" Lude..=) Lude.<$> description
          ]
      )

instance Lude.ToPath StartOnDemandReplicationRun where
  toPath = Lude.const "/"

instance Lude.ToQuery StartOnDemandReplicationRun where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartOnDemandReplicationRunResponse' smart constructor.
data StartOnDemandReplicationRunResponse = StartOnDemandReplicationRunResponse'
  { -- | The ID of the replication run.
    replicationRunId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartOnDemandReplicationRunResponse' with the minimum fields required to make a request.
--
-- * 'replicationRunId' - The ID of the replication run.
-- * 'responseStatus' - The response status code.
mkStartOnDemandReplicationRunResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartOnDemandReplicationRunResponse
mkStartOnDemandReplicationRunResponse pResponseStatus_ =
  StartOnDemandReplicationRunResponse'
    { replicationRunId =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the replication run.
--
-- /Note:/ Consider using 'replicationRunId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sodrrrsReplicationRunId :: Lens.Lens' StartOnDemandReplicationRunResponse (Lude.Maybe Lude.Text)
sodrrrsReplicationRunId = Lens.lens (replicationRunId :: StartOnDemandReplicationRunResponse -> Lude.Maybe Lude.Text) (\s a -> s {replicationRunId = a} :: StartOnDemandReplicationRunResponse)
{-# DEPRECATED sodrrrsReplicationRunId "Use generic-lens or generic-optics with 'replicationRunId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sodrrrsResponseStatus :: Lens.Lens' StartOnDemandReplicationRunResponse Lude.Int
sodrrrsResponseStatus = Lens.lens (responseStatus :: StartOnDemandReplicationRunResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartOnDemandReplicationRunResponse)
{-# DEPRECATED sodrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
