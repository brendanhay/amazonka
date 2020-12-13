{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.StartOnDemandAppReplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an on-demand replication run for the specified application.
module Network.AWS.SMS.StartOnDemandAppReplication
  ( -- * Creating a request
    StartOnDemandAppReplication (..),
    mkStartOnDemandAppReplication,

    -- ** Request lenses
    sodarAppId,
    sodarDescription,

    -- * Destructuring the response
    StartOnDemandAppReplicationResponse (..),
    mkStartOnDemandAppReplicationResponse,

    -- ** Response lenses
    sodarrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SMS.Types

-- | /See:/ 'mkStartOnDemandAppReplication' smart constructor.
data StartOnDemandAppReplication = StartOnDemandAppReplication'
  { -- | The ID of the application.
    appId :: Lude.Text,
    -- | The description of the replication run.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartOnDemandAppReplication' with the minimum fields required to make a request.
--
-- * 'appId' - The ID of the application.
-- * 'description' - The description of the replication run.
mkStartOnDemandAppReplication ::
  -- | 'appId'
  Lude.Text ->
  StartOnDemandAppReplication
mkStartOnDemandAppReplication pAppId_ =
  StartOnDemandAppReplication'
    { appId = pAppId_,
      description = Lude.Nothing
    }

-- | The ID of the application.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sodarAppId :: Lens.Lens' StartOnDemandAppReplication Lude.Text
sodarAppId = Lens.lens (appId :: StartOnDemandAppReplication -> Lude.Text) (\s a -> s {appId = a} :: StartOnDemandAppReplication)
{-# DEPRECATED sodarAppId "Use generic-lens or generic-optics with 'appId' instead." #-}

-- | The description of the replication run.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sodarDescription :: Lens.Lens' StartOnDemandAppReplication (Lude.Maybe Lude.Text)
sodarDescription = Lens.lens (description :: StartOnDemandAppReplication -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: StartOnDemandAppReplication)
{-# DEPRECATED sodarDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.AWSRequest StartOnDemandAppReplication where
  type
    Rs StartOnDemandAppReplication =
      StartOnDemandAppReplicationResponse
  request = Req.postJSON smsService
  response =
    Res.receiveEmpty
      ( \s h x ->
          StartOnDemandAppReplicationResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartOnDemandAppReplication where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSServerMigrationService_V2016_10_24.StartOnDemandAppReplication" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartOnDemandAppReplication where
  toJSON StartOnDemandAppReplication' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("appId" Lude..= appId),
            ("description" Lude..=) Lude.<$> description
          ]
      )

instance Lude.ToPath StartOnDemandAppReplication where
  toPath = Lude.const "/"

instance Lude.ToQuery StartOnDemandAppReplication where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartOnDemandAppReplicationResponse' smart constructor.
newtype StartOnDemandAppReplicationResponse = StartOnDemandAppReplicationResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartOnDemandAppReplicationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkStartOnDemandAppReplicationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartOnDemandAppReplicationResponse
mkStartOnDemandAppReplicationResponse pResponseStatus_ =
  StartOnDemandAppReplicationResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sodarrsResponseStatus :: Lens.Lens' StartOnDemandAppReplicationResponse Lude.Int
sodarrsResponseStatus = Lens.lens (responseStatus :: StartOnDemandAppReplicationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartOnDemandAppReplicationResponse)
{-# DEPRECATED sodarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
