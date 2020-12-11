{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGatewayManagementAPI.DeleteConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete the connection with the provided id.
module Network.AWS.APIGatewayManagementAPI.DeleteConnection
  ( -- * Creating a request
    DeleteConnection (..),
    mkDeleteConnection,

    -- ** Request lenses
    dcConnectionId,

    -- * Destructuring the response
    DeleteConnectionResponse (..),
    mkDeleteConnectionResponse,
  )
where

import Network.AWS.APIGatewayManagementAPI.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteConnection' smart constructor.
newtype DeleteConnection = DeleteConnection'
  { connectionId ::
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

-- | Creates a value of 'DeleteConnection' with the minimum fields required to make a request.
--
-- * 'connectionId' - Undocumented field.
mkDeleteConnection ::
  -- | 'connectionId'
  Lude.Text ->
  DeleteConnection
mkDeleteConnection pConnectionId_ =
  DeleteConnection' {connectionId = pConnectionId_}

-- | Undocumented field.
--
-- /Note:/ Consider using 'connectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcConnectionId :: Lens.Lens' DeleteConnection Lude.Text
dcConnectionId = Lens.lens (connectionId :: DeleteConnection -> Lude.Text) (\s a -> s {connectionId = a} :: DeleteConnection)
{-# DEPRECATED dcConnectionId "Use generic-lens or generic-optics with 'connectionId' instead." #-}

instance Lude.AWSRequest DeleteConnection where
  type Rs DeleteConnection = DeleteConnectionResponse
  request = Req.delete apiGatewayManagementAPIService
  response = Res.receiveNull DeleteConnectionResponse'

instance Lude.ToHeaders DeleteConnection where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteConnection where
  toPath DeleteConnection' {..} =
    Lude.mconcat ["/@connections/", Lude.toBS connectionId]

instance Lude.ToQuery DeleteConnection where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteConnectionResponse' smart constructor.
data DeleteConnectionResponse = DeleteConnectionResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteConnectionResponse' with the minimum fields required to make a request.
mkDeleteConnectionResponse ::
  DeleteConnectionResponse
mkDeleteConnectionResponse = DeleteConnectionResponse'
