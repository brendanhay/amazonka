{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.DeleteConnectionAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified connection alias. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/cross-region-redirection.html Cross-Region Redirection for Amazon WorkSpaces> .
--
-- /Important:/ __If you will no longer be using a fully qualified domain name (FQDN) as the registration code for your WorkSpaces users, you must take certain precautions to prevent potential security issues.__ For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/cross-region-redirection.html#cross-region-redirection-security-considerations Security Considerations if You Stop Using Cross-Region Redirection> .
module Network.AWS.WorkSpaces.DeleteConnectionAlias
  ( -- * Creating a request
    DeleteConnectionAlias (..),
    mkDeleteConnectionAlias,

    -- ** Request lenses
    dAliasId,

    -- * Destructuring the response
    DeleteConnectionAliasResponse (..),
    mkDeleteConnectionAliasResponse,

    -- ** Response lenses
    dcarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'mkDeleteConnectionAlias' smart constructor.
newtype DeleteConnectionAlias = DeleteConnectionAlias'
  { aliasId ::
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

-- | Creates a value of 'DeleteConnectionAlias' with the minimum fields required to make a request.
--
-- * 'aliasId' - The identifier of the connection alias to delete.
mkDeleteConnectionAlias ::
  -- | 'aliasId'
  Lude.Text ->
  DeleteConnectionAlias
mkDeleteConnectionAlias pAliasId_ =
  DeleteConnectionAlias' {aliasId = pAliasId_}

-- | The identifier of the connection alias to delete.
--
-- /Note:/ Consider using 'aliasId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAliasId :: Lens.Lens' DeleteConnectionAlias Lude.Text
dAliasId = Lens.lens (aliasId :: DeleteConnectionAlias -> Lude.Text) (\s a -> s {aliasId = a} :: DeleteConnectionAlias)
{-# DEPRECATED dAliasId "Use generic-lens or generic-optics with 'aliasId' instead." #-}

instance Lude.AWSRequest DeleteConnectionAlias where
  type Rs DeleteConnectionAlias = DeleteConnectionAliasResponse
  request = Req.postJSON workSpacesService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteConnectionAliasResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteConnectionAlias where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkspacesService.DeleteConnectionAlias" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteConnectionAlias where
  toJSON DeleteConnectionAlias' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("AliasId" Lude..= aliasId)])

instance Lude.ToPath DeleteConnectionAlias where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteConnectionAlias where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteConnectionAliasResponse' smart constructor.
newtype DeleteConnectionAliasResponse = DeleteConnectionAliasResponse'
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

-- | Creates a value of 'DeleteConnectionAliasResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteConnectionAliasResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteConnectionAliasResponse
mkDeleteConnectionAliasResponse pResponseStatus_ =
  DeleteConnectionAliasResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcarsResponseStatus :: Lens.Lens' DeleteConnectionAliasResponse Lude.Int
dcarsResponseStatus = Lens.lens (responseStatus :: DeleteConnectionAliasResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteConnectionAliasResponse)
{-# DEPRECATED dcarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
