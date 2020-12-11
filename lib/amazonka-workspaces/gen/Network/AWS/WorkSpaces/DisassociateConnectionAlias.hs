{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.DisassociateConnectionAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a connection alias from a directory. Disassociating a connection alias disables cross-Region redirection between two directories in different AWS Regions. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/cross-region-redirection.html Cross-Region Redirection for Amazon WorkSpaces> .
module Network.AWS.WorkSpaces.DisassociateConnectionAlias
  ( -- * Creating a request
    DisassociateConnectionAlias (..),
    mkDisassociateConnectionAlias,

    -- ** Request lenses
    dcaAliasId,

    -- * Destructuring the response
    DisassociateConnectionAliasResponse (..),
    mkDisassociateConnectionAliasResponse,

    -- ** Response lenses
    dcasrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'mkDisassociateConnectionAlias' smart constructor.
newtype DisassociateConnectionAlias = DisassociateConnectionAlias'
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

-- | Creates a value of 'DisassociateConnectionAlias' with the minimum fields required to make a request.
--
-- * 'aliasId' - The identifier of the connection alias to disassociate.
mkDisassociateConnectionAlias ::
  -- | 'aliasId'
  Lude.Text ->
  DisassociateConnectionAlias
mkDisassociateConnectionAlias pAliasId_ =
  DisassociateConnectionAlias' {aliasId = pAliasId_}

-- | The identifier of the connection alias to disassociate.
--
-- /Note:/ Consider using 'aliasId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaAliasId :: Lens.Lens' DisassociateConnectionAlias Lude.Text
dcaAliasId = Lens.lens (aliasId :: DisassociateConnectionAlias -> Lude.Text) (\s a -> s {aliasId = a} :: DisassociateConnectionAlias)
{-# DEPRECATED dcaAliasId "Use generic-lens or generic-optics with 'aliasId' instead." #-}

instance Lude.AWSRequest DisassociateConnectionAlias where
  type
    Rs DisassociateConnectionAlias =
      DisassociateConnectionAliasResponse
  request = Req.postJSON workSpacesService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DisassociateConnectionAliasResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DisassociateConnectionAlias where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "WorkspacesService.DisassociateConnectionAlias" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DisassociateConnectionAlias where
  toJSON DisassociateConnectionAlias' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("AliasId" Lude..= aliasId)])

instance Lude.ToPath DisassociateConnectionAlias where
  toPath = Lude.const "/"

instance Lude.ToQuery DisassociateConnectionAlias where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDisassociateConnectionAliasResponse' smart constructor.
newtype DisassociateConnectionAliasResponse = DisassociateConnectionAliasResponse'
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

-- | Creates a value of 'DisassociateConnectionAliasResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDisassociateConnectionAliasResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DisassociateConnectionAliasResponse
mkDisassociateConnectionAliasResponse pResponseStatus_ =
  DisassociateConnectionAliasResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcasrsResponseStatus :: Lens.Lens' DisassociateConnectionAliasResponse Lude.Int
dcasrsResponseStatus = Lens.lens (responseStatus :: DisassociateConnectionAliasResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisassociateConnectionAliasResponse)
{-# DEPRECATED dcasrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
