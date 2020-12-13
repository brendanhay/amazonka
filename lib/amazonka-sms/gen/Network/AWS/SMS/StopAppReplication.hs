{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.StopAppReplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops replicating the specified application by deleting the replication job for each server in the application.
module Network.AWS.SMS.StopAppReplication
  ( -- * Creating a request
    StopAppReplication (..),
    mkStopAppReplication,

    -- ** Request lenses
    sAppId,

    -- * Destructuring the response
    StopAppReplicationResponse (..),
    mkStopAppReplicationResponse,

    -- ** Response lenses
    sarrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SMS.Types

-- | /See:/ 'mkStopAppReplication' smart constructor.
newtype StopAppReplication = StopAppReplication'
  { -- | The ID of the application.
    appId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopAppReplication' with the minimum fields required to make a request.
--
-- * 'appId' - The ID of the application.
mkStopAppReplication ::
  StopAppReplication
mkStopAppReplication = StopAppReplication' {appId = Lude.Nothing}

-- | The ID of the application.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAppId :: Lens.Lens' StopAppReplication (Lude.Maybe Lude.Text)
sAppId = Lens.lens (appId :: StopAppReplication -> Lude.Maybe Lude.Text) (\s a -> s {appId = a} :: StopAppReplication)
{-# DEPRECATED sAppId "Use generic-lens or generic-optics with 'appId' instead." #-}

instance Lude.AWSRequest StopAppReplication where
  type Rs StopAppReplication = StopAppReplicationResponse
  request = Req.postJSON smsService
  response =
    Res.receiveEmpty
      ( \s h x ->
          StopAppReplicationResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StopAppReplication where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSServerMigrationService_V2016_10_24.StopAppReplication" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopAppReplication where
  toJSON StopAppReplication' {..} =
    Lude.object (Lude.catMaybes [("appId" Lude..=) Lude.<$> appId])

instance Lude.ToPath StopAppReplication where
  toPath = Lude.const "/"

instance Lude.ToQuery StopAppReplication where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStopAppReplicationResponse' smart constructor.
newtype StopAppReplicationResponse = StopAppReplicationResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopAppReplicationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkStopAppReplicationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StopAppReplicationResponse
mkStopAppReplicationResponse pResponseStatus_ =
  StopAppReplicationResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sarrsResponseStatus :: Lens.Lens' StopAppReplicationResponse Lude.Int
sarrsResponseStatus = Lens.lens (responseStatus :: StopAppReplicationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StopAppReplicationResponse)
{-# DEPRECATED sarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
