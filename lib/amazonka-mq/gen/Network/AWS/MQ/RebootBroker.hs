{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.RebootBroker
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reboots a broker. Note: This API is asynchronous.
module Network.AWS.MQ.RebootBroker
  ( -- * Creating a request
    RebootBroker (..),
    mkRebootBroker,

    -- ** Request lenses
    rbBrokerId,

    -- * Destructuring the response
    RebootBrokerResponse (..),
    mkRebootBrokerResponse,

    -- ** Response lenses
    rbrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRebootBroker' smart constructor.
newtype RebootBroker = RebootBroker'
  { -- | The unique ID that Amazon MQ generates for the broker.
    brokerId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RebootBroker' with the minimum fields required to make a request.
--
-- * 'brokerId' - The unique ID that Amazon MQ generates for the broker.
mkRebootBroker ::
  -- | 'brokerId'
  Lude.Text ->
  RebootBroker
mkRebootBroker pBrokerId_ = RebootBroker' {brokerId = pBrokerId_}

-- | The unique ID that Amazon MQ generates for the broker.
--
-- /Note:/ Consider using 'brokerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rbBrokerId :: Lens.Lens' RebootBroker Lude.Text
rbBrokerId = Lens.lens (brokerId :: RebootBroker -> Lude.Text) (\s a -> s {brokerId = a} :: RebootBroker)
{-# DEPRECATED rbBrokerId "Use generic-lens or generic-optics with 'brokerId' instead." #-}

instance Lude.AWSRequest RebootBroker where
  type Rs RebootBroker = RebootBrokerResponse
  request = Req.postJSON mqService
  response =
    Res.receiveEmpty
      ( \s h x ->
          RebootBrokerResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RebootBroker where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RebootBroker where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath RebootBroker where
  toPath RebootBroker' {..} =
    Lude.mconcat ["/v1/brokers/", Lude.toBS brokerId, "/reboot"]

instance Lude.ToQuery RebootBroker where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRebootBrokerResponse' smart constructor.
newtype RebootBrokerResponse = RebootBrokerResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RebootBrokerResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkRebootBrokerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RebootBrokerResponse
mkRebootBrokerResponse pResponseStatus_ =
  RebootBrokerResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rbrsResponseStatus :: Lens.Lens' RebootBrokerResponse Lude.Int
rbrsResponseStatus = Lens.lens (responseStatus :: RebootBrokerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RebootBrokerResponse)
{-# DEPRECATED rbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
