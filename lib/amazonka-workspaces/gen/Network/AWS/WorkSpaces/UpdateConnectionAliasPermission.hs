{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.UpdateConnectionAliasPermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Shares or unshares a connection alias with one account by specifying whether that account has permission to associate the connection alias with a directory. If the association permission is granted, the connection alias is shared with that account. If the association permission is revoked, the connection alias is unshared with the account. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/cross-region-redirection.html Cross-Region Redirection for Amazon WorkSpaces> .
module Network.AWS.WorkSpaces.UpdateConnectionAliasPermission
  ( -- * Creating a request
    UpdateConnectionAliasPermission (..),
    mkUpdateConnectionAliasPermission,

    -- ** Request lenses
    ucapAliasId,
    ucapConnectionAliasPermission,

    -- * Destructuring the response
    UpdateConnectionAliasPermissionResponse (..),
    mkUpdateConnectionAliasPermissionResponse,

    -- ** Response lenses
    ucaprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'mkUpdateConnectionAliasPermission' smart constructor.
data UpdateConnectionAliasPermission = UpdateConnectionAliasPermission'
  { aliasId ::
      Lude.Text,
    connectionAliasPermission ::
      ConnectionAliasPermission
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateConnectionAliasPermission' with the minimum fields required to make a request.
--
-- * 'aliasId' - The identifier of the connection alias that you want to update permissions for.
-- * 'connectionAliasPermission' - Indicates whether to share or unshare the connection alias with the specified AWS account.
mkUpdateConnectionAliasPermission ::
  -- | 'aliasId'
  Lude.Text ->
  -- | 'connectionAliasPermission'
  ConnectionAliasPermission ->
  UpdateConnectionAliasPermission
mkUpdateConnectionAliasPermission
  pAliasId_
  pConnectionAliasPermission_ =
    UpdateConnectionAliasPermission'
      { aliasId = pAliasId_,
        connectionAliasPermission = pConnectionAliasPermission_
      }

-- | The identifier of the connection alias that you want to update permissions for.
--
-- /Note:/ Consider using 'aliasId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucapAliasId :: Lens.Lens' UpdateConnectionAliasPermission Lude.Text
ucapAliasId = Lens.lens (aliasId :: UpdateConnectionAliasPermission -> Lude.Text) (\s a -> s {aliasId = a} :: UpdateConnectionAliasPermission)
{-# DEPRECATED ucapAliasId "Use generic-lens or generic-optics with 'aliasId' instead." #-}

-- | Indicates whether to share or unshare the connection alias with the specified AWS account.
--
-- /Note:/ Consider using 'connectionAliasPermission' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucapConnectionAliasPermission :: Lens.Lens' UpdateConnectionAliasPermission ConnectionAliasPermission
ucapConnectionAliasPermission = Lens.lens (connectionAliasPermission :: UpdateConnectionAliasPermission -> ConnectionAliasPermission) (\s a -> s {connectionAliasPermission = a} :: UpdateConnectionAliasPermission)
{-# DEPRECATED ucapConnectionAliasPermission "Use generic-lens or generic-optics with 'connectionAliasPermission' instead." #-}

instance Lude.AWSRequest UpdateConnectionAliasPermission where
  type
    Rs UpdateConnectionAliasPermission =
      UpdateConnectionAliasPermissionResponse
  request = Req.postJSON workSpacesService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateConnectionAliasPermissionResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateConnectionAliasPermission where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "WorkspacesService.UpdateConnectionAliasPermission" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateConnectionAliasPermission where
  toJSON UpdateConnectionAliasPermission' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("AliasId" Lude..= aliasId),
            Lude.Just
              ("ConnectionAliasPermission" Lude..= connectionAliasPermission)
          ]
      )

instance Lude.ToPath UpdateConnectionAliasPermission where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateConnectionAliasPermission where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateConnectionAliasPermissionResponse' smart constructor.
newtype UpdateConnectionAliasPermissionResponse = UpdateConnectionAliasPermissionResponse'
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

-- | Creates a value of 'UpdateConnectionAliasPermissionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateConnectionAliasPermissionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateConnectionAliasPermissionResponse
mkUpdateConnectionAliasPermissionResponse pResponseStatus_ =
  UpdateConnectionAliasPermissionResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucaprsResponseStatus :: Lens.Lens' UpdateConnectionAliasPermissionResponse Lude.Int
ucaprsResponseStatus = Lens.lens (responseStatus :: UpdateConnectionAliasPermissionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateConnectionAliasPermissionResponse)
{-# DEPRECATED ucaprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
