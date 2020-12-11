{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DeleteRoleAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a role alias
module Network.AWS.IoT.DeleteRoleAlias
  ( -- * Creating a request
    DeleteRoleAlias (..),
    mkDeleteRoleAlias,

    -- ** Request lenses
    dRoleAlias,

    -- * Destructuring the response
    DeleteRoleAliasResponse (..),
    mkDeleteRoleAliasResponse,

    -- ** Response lenses
    drsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteRoleAlias' smart constructor.
newtype DeleteRoleAlias = DeleteRoleAlias' {roleAlias :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteRoleAlias' with the minimum fields required to make a request.
--
-- * 'roleAlias' - The role alias to delete.
mkDeleteRoleAlias ::
  -- | 'roleAlias'
  Lude.Text ->
  DeleteRoleAlias
mkDeleteRoleAlias pRoleAlias_ =
  DeleteRoleAlias' {roleAlias = pRoleAlias_}

-- | The role alias to delete.
--
-- /Note:/ Consider using 'roleAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dRoleAlias :: Lens.Lens' DeleteRoleAlias Lude.Text
dRoleAlias = Lens.lens (roleAlias :: DeleteRoleAlias -> Lude.Text) (\s a -> s {roleAlias = a} :: DeleteRoleAlias)
{-# DEPRECATED dRoleAlias "Use generic-lens or generic-optics with 'roleAlias' instead." #-}

instance Lude.AWSRequest DeleteRoleAlias where
  type Rs DeleteRoleAlias = DeleteRoleAliasResponse
  request = Req.delete ioTService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteRoleAliasResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteRoleAlias where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteRoleAlias where
  toPath DeleteRoleAlias' {..} =
    Lude.mconcat ["/role-aliases/", Lude.toBS roleAlias]

instance Lude.ToQuery DeleteRoleAlias where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteRoleAliasResponse' smart constructor.
newtype DeleteRoleAliasResponse = DeleteRoleAliasResponse'
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

-- | Creates a value of 'DeleteRoleAliasResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteRoleAliasResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteRoleAliasResponse
mkDeleteRoleAliasResponse pResponseStatus_ =
  DeleteRoleAliasResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteRoleAliasResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DeleteRoleAliasResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteRoleAliasResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
