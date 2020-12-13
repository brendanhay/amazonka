{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.StartAppReplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts replicating the specified application by creating replication jobs for each server in the application.
module Network.AWS.SMS.StartAppReplication
  ( -- * Creating a request
    StartAppReplication (..),
    mkStartAppReplication,

    -- ** Request lenses
    sarAppId,

    -- * Destructuring the response
    StartAppReplicationResponse (..),
    mkStartAppReplicationResponse,

    -- ** Response lenses
    srsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SMS.Types

-- | /See:/ 'mkStartAppReplication' smart constructor.
newtype StartAppReplication = StartAppReplication'
  { -- | The ID of the application.
    appId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartAppReplication' with the minimum fields required to make a request.
--
-- * 'appId' - The ID of the application.
mkStartAppReplication ::
  StartAppReplication
mkStartAppReplication = StartAppReplication' {appId = Lude.Nothing}

-- | The ID of the application.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sarAppId :: Lens.Lens' StartAppReplication (Lude.Maybe Lude.Text)
sarAppId = Lens.lens (appId :: StartAppReplication -> Lude.Maybe Lude.Text) (\s a -> s {appId = a} :: StartAppReplication)
{-# DEPRECATED sarAppId "Use generic-lens or generic-optics with 'appId' instead." #-}

instance Lude.AWSRequest StartAppReplication where
  type Rs StartAppReplication = StartAppReplicationResponse
  request = Req.postJSON smsService
  response =
    Res.receiveEmpty
      ( \s h x ->
          StartAppReplicationResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartAppReplication where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSServerMigrationService_V2016_10_24.StartAppReplication" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartAppReplication where
  toJSON StartAppReplication' {..} =
    Lude.object (Lude.catMaybes [("appId" Lude..=) Lude.<$> appId])

instance Lude.ToPath StartAppReplication where
  toPath = Lude.const "/"

instance Lude.ToQuery StartAppReplication where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartAppReplicationResponse' smart constructor.
newtype StartAppReplicationResponse = StartAppReplicationResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartAppReplicationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkStartAppReplicationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartAppReplicationResponse
mkStartAppReplicationResponse pResponseStatus_ =
  StartAppReplicationResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' StartAppReplicationResponse Lude.Int
srsResponseStatus = Lens.lens (responseStatus :: StartAppReplicationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartAppReplicationResponse)
{-# DEPRECATED srsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
