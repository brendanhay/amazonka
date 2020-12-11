{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.DisassociateConnector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the specified connector from AWS SMS.
--
-- After you disassociate a connector, it is no longer available to support replication jobs.
module Network.AWS.SMS.DisassociateConnector
  ( -- * Creating a request
    DisassociateConnector (..),
    mkDisassociateConnector,

    -- ** Request lenses
    dcConnectorId,

    -- * Destructuring the response
    DisassociateConnectorResponse (..),
    mkDisassociateConnectorResponse,

    -- ** Response lenses
    dcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SMS.Types

-- | /See:/ 'mkDisassociateConnector' smart constructor.
newtype DisassociateConnector = DisassociateConnector'
  { connectorId ::
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

-- | Creates a value of 'DisassociateConnector' with the minimum fields required to make a request.
--
-- * 'connectorId' - The ID of the connector.
mkDisassociateConnector ::
  -- | 'connectorId'
  Lude.Text ->
  DisassociateConnector
mkDisassociateConnector pConnectorId_ =
  DisassociateConnector' {connectorId = pConnectorId_}

-- | The ID of the connector.
--
-- /Note:/ Consider using 'connectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcConnectorId :: Lens.Lens' DisassociateConnector Lude.Text
dcConnectorId = Lens.lens (connectorId :: DisassociateConnector -> Lude.Text) (\s a -> s {connectorId = a} :: DisassociateConnector)
{-# DEPRECATED dcConnectorId "Use generic-lens or generic-optics with 'connectorId' instead." #-}

instance Lude.AWSRequest DisassociateConnector where
  type Rs DisassociateConnector = DisassociateConnectorResponse
  request = Req.postJSON smsService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DisassociateConnectorResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DisassociateConnector where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSServerMigrationService_V2016_10_24.DisassociateConnector" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DisassociateConnector where
  toJSON DisassociateConnector' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("connectorId" Lude..= connectorId)])

instance Lude.ToPath DisassociateConnector where
  toPath = Lude.const "/"

instance Lude.ToQuery DisassociateConnector where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDisassociateConnectorResponse' smart constructor.
newtype DisassociateConnectorResponse = DisassociateConnectorResponse'
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

-- | Creates a value of 'DisassociateConnectorResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDisassociateConnectorResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DisassociateConnectorResponse
mkDisassociateConnectorResponse pResponseStatus_ =
  DisassociateConnectorResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsResponseStatus :: Lens.Lens' DisassociateConnectorResponse Lude.Int
dcrsResponseStatus = Lens.lens (responseStatus :: DisassociateConnectorResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisassociateConnectorResponse)
{-# DEPRECATED dcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
