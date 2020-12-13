{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DeleteAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an alias. This operation removes all record of the alias. Game clients attempting to access a server process using the deleted alias receive an error. To delete an alias, specify the alias ID to be deleted.
--
--
--     * 'CreateAlias'
--
--
--     * 'ListAliases'
--
--
--     * 'DescribeAlias'
--
--
--     * 'UpdateAlias'
--
--
--     * 'DeleteAlias'
--
--
--     * 'ResolveAlias'
module Network.AWS.GameLift.DeleteAlias
  ( -- * Creating a request
    DeleteAlias (..),
    mkDeleteAlias,

    -- ** Request lenses
    dAliasId,

    -- * Destructuring the response
    DeleteAliasResponse (..),
    mkDeleteAliasResponse,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for a request operation.
--
-- /See:/ 'mkDeleteAlias' smart constructor.
newtype DeleteAlias = DeleteAlias'
  { -- | A unique identifier of the alias that you want to delete. You can use either the alias ID or ARN value.
    aliasId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAlias' with the minimum fields required to make a request.
--
-- * 'aliasId' - A unique identifier of the alias that you want to delete. You can use either the alias ID or ARN value.
mkDeleteAlias ::
  -- | 'aliasId'
  Lude.Text ->
  DeleteAlias
mkDeleteAlias pAliasId_ = DeleteAlias' {aliasId = pAliasId_}

-- | A unique identifier of the alias that you want to delete. You can use either the alias ID or ARN value.
--
-- /Note:/ Consider using 'aliasId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAliasId :: Lens.Lens' DeleteAlias Lude.Text
dAliasId = Lens.lens (aliasId :: DeleteAlias -> Lude.Text) (\s a -> s {aliasId = a} :: DeleteAlias)
{-# DEPRECATED dAliasId "Use generic-lens or generic-optics with 'aliasId' instead." #-}

instance Lude.AWSRequest DeleteAlias where
  type Rs DeleteAlias = DeleteAliasResponse
  request = Req.postJSON gameLiftService
  response = Res.receiveNull DeleteAliasResponse'

instance Lude.ToHeaders DeleteAlias where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.DeleteAlias" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteAlias where
  toJSON DeleteAlias' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("AliasId" Lude..= aliasId)])

instance Lude.ToPath DeleteAlias where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteAlias where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteAliasResponse' smart constructor.
data DeleteAliasResponse = DeleteAliasResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAliasResponse' with the minimum fields required to make a request.
mkDeleteAliasResponse ::
  DeleteAliasResponse
mkDeleteAliasResponse = DeleteAliasResponse'
