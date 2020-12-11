{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorksCM.DeleteServer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the server and the underlying AWS CloudFormation stacks (including the server's EC2 instance). When you run this command, the server state is updated to @DELETING@ . After the server is deleted, it is no longer returned by @DescribeServer@ requests. If the AWS CloudFormation stack cannot be deleted, the server cannot be deleted.
--
-- This operation is asynchronous.
-- An @InvalidStateException@ is thrown when a server deletion is already in progress. A @ResourceNotFoundException@ is thrown when the server does not exist. A @ValidationException@ is raised when parameters of the request are not valid.
module Network.AWS.OpsWorksCM.DeleteServer
  ( -- * Creating a request
    DeleteServer (..),
    mkDeleteServer,

    -- ** Request lenses
    dsServerName,

    -- * Destructuring the response
    DeleteServerResponse (..),
    mkDeleteServerResponse,

    -- ** Response lenses
    dsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorksCM.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteServer' smart constructor.
newtype DeleteServer = DeleteServer' {serverName :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteServer' with the minimum fields required to make a request.
--
-- * 'serverName' - The ID of the server to delete.
mkDeleteServer ::
  -- | 'serverName'
  Lude.Text ->
  DeleteServer
mkDeleteServer pServerName_ =
  DeleteServer' {serverName = pServerName_}

-- | The ID of the server to delete.
--
-- /Note:/ Consider using 'serverName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsServerName :: Lens.Lens' DeleteServer Lude.Text
dsServerName = Lens.lens (serverName :: DeleteServer -> Lude.Text) (\s a -> s {serverName = a} :: DeleteServer)
{-# DEPRECATED dsServerName "Use generic-lens or generic-optics with 'serverName' instead." #-}

instance Lude.AWSRequest DeleteServer where
  type Rs DeleteServer = DeleteServerResponse
  request = Req.postJSON opsWorksCMService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteServerResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteServer where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorksCM_V2016_11_01.DeleteServer" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteServer where
  toJSON DeleteServer' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("ServerName" Lude..= serverName)])

instance Lude.ToPath DeleteServer where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteServer where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteServerResponse' smart constructor.
newtype DeleteServerResponse = DeleteServerResponse'
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

-- | Creates a value of 'DeleteServerResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteServerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteServerResponse
mkDeleteServerResponse pResponseStatus_ =
  DeleteServerResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsResponseStatus :: Lens.Lens' DeleteServerResponse Lude.Int
dsrsResponseStatus = Lens.lens (responseStatus :: DeleteServerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteServerResponse)
{-# DEPRECATED dsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
