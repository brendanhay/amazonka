{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.DeleteBroker
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a broker. Note: This API is asynchronous.
module Network.AWS.MQ.DeleteBroker
  ( -- * Creating a request
    DeleteBroker (..),
    mkDeleteBroker,

    -- ** Request lenses
    dbfBrokerId,

    -- * Destructuring the response
    DeleteBrokerResponse (..),
    mkDeleteBrokerResponse,

    -- ** Response lenses
    drsBrokerId,
    drsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteBroker' smart constructor.
newtype DeleteBroker = DeleteBroker'
  { -- | The unique ID that Amazon MQ generates for the broker.
    brokerId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteBroker' with the minimum fields required to make a request.
--
-- * 'brokerId' - The unique ID that Amazon MQ generates for the broker.
mkDeleteBroker ::
  -- | 'brokerId'
  Lude.Text ->
  DeleteBroker
mkDeleteBroker pBrokerId_ = DeleteBroker' {brokerId = pBrokerId_}

-- | The unique ID that Amazon MQ generates for the broker.
--
-- /Note:/ Consider using 'brokerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbfBrokerId :: Lens.Lens' DeleteBroker Lude.Text
dbfBrokerId = Lens.lens (brokerId :: DeleteBroker -> Lude.Text) (\s a -> s {brokerId = a} :: DeleteBroker)
{-# DEPRECATED dbfBrokerId "Use generic-lens or generic-optics with 'brokerId' instead." #-}

instance Lude.AWSRequest DeleteBroker where
  type Rs DeleteBroker = DeleteBrokerResponse
  request = Req.delete mqService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteBrokerResponse'
            Lude.<$> (x Lude..?> "brokerId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteBroker where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteBroker where
  toPath DeleteBroker' {..} =
    Lude.mconcat ["/v1/brokers/", Lude.toBS brokerId]

instance Lude.ToQuery DeleteBroker where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteBrokerResponse' smart constructor.
data DeleteBrokerResponse = DeleteBrokerResponse'
  { -- | The unique ID that Amazon MQ generates for the broker.
    brokerId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteBrokerResponse' with the minimum fields required to make a request.
--
-- * 'brokerId' - The unique ID that Amazon MQ generates for the broker.
-- * 'responseStatus' - The response status code.
mkDeleteBrokerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteBrokerResponse
mkDeleteBrokerResponse pResponseStatus_ =
  DeleteBrokerResponse'
    { brokerId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The unique ID that Amazon MQ generates for the broker.
--
-- /Note:/ Consider using 'brokerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsBrokerId :: Lens.Lens' DeleteBrokerResponse (Lude.Maybe Lude.Text)
drsBrokerId = Lens.lens (brokerId :: DeleteBrokerResponse -> Lude.Maybe Lude.Text) (\s a -> s {brokerId = a} :: DeleteBrokerResponse)
{-# DEPRECATED drsBrokerId "Use generic-lens or generic-optics with 'brokerId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteBrokerResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DeleteBrokerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteBrokerResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
