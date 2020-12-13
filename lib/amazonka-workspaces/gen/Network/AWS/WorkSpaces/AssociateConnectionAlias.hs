{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.AssociateConnectionAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified connection alias with the specified directory to enable cross-Region redirection. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/cross-region-redirection.html Cross-Region Redirection for Amazon WorkSpaces> .
module Network.AWS.WorkSpaces.AssociateConnectionAlias
  ( -- * Creating a request
    AssociateConnectionAlias (..),
    mkAssociateConnectionAlias,

    -- ** Request lenses
    acaResourceId,
    acaAliasId,

    -- * Destructuring the response
    AssociateConnectionAliasResponse (..),
    mkAssociateConnectionAliasResponse,

    -- ** Response lenses
    acarsConnectionIdentifier,
    acarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'mkAssociateConnectionAlias' smart constructor.
data AssociateConnectionAlias = AssociateConnectionAlias'
  { -- | The identifier of the directory to associate the connection alias with.
    resourceId :: Lude.Text,
    -- | The identifier of the connection alias.
    aliasId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateConnectionAlias' with the minimum fields required to make a request.
--
-- * 'resourceId' - The identifier of the directory to associate the connection alias with.
-- * 'aliasId' - The identifier of the connection alias.
mkAssociateConnectionAlias ::
  -- | 'resourceId'
  Lude.Text ->
  -- | 'aliasId'
  Lude.Text ->
  AssociateConnectionAlias
mkAssociateConnectionAlias pResourceId_ pAliasId_ =
  AssociateConnectionAlias'
    { resourceId = pResourceId_,
      aliasId = pAliasId_
    }

-- | The identifier of the directory to associate the connection alias with.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acaResourceId :: Lens.Lens' AssociateConnectionAlias Lude.Text
acaResourceId = Lens.lens (resourceId :: AssociateConnectionAlias -> Lude.Text) (\s a -> s {resourceId = a} :: AssociateConnectionAlias)
{-# DEPRECATED acaResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The identifier of the connection alias.
--
-- /Note:/ Consider using 'aliasId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acaAliasId :: Lens.Lens' AssociateConnectionAlias Lude.Text
acaAliasId = Lens.lens (aliasId :: AssociateConnectionAlias -> Lude.Text) (\s a -> s {aliasId = a} :: AssociateConnectionAlias)
{-# DEPRECATED acaAliasId "Use generic-lens or generic-optics with 'aliasId' instead." #-}

instance Lude.AWSRequest AssociateConnectionAlias where
  type Rs AssociateConnectionAlias = AssociateConnectionAliasResponse
  request = Req.postJSON workSpacesService
  response =
    Res.receiveJSON
      ( \s h x ->
          AssociateConnectionAliasResponse'
            Lude.<$> (x Lude..?> "ConnectionIdentifier")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AssociateConnectionAlias where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkspacesService.AssociateConnectionAlias" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AssociateConnectionAlias where
  toJSON AssociateConnectionAlias' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ResourceId" Lude..= resourceId),
            Lude.Just ("AliasId" Lude..= aliasId)
          ]
      )

instance Lude.ToPath AssociateConnectionAlias where
  toPath = Lude.const "/"

instance Lude.ToQuery AssociateConnectionAlias where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAssociateConnectionAliasResponse' smart constructor.
data AssociateConnectionAliasResponse = AssociateConnectionAliasResponse'
  { -- | The identifier of the connection alias association. You use the connection identifier in the DNS TXT record when you're configuring your DNS routing policies.
    connectionIdentifier :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateConnectionAliasResponse' with the minimum fields required to make a request.
--
-- * 'connectionIdentifier' - The identifier of the connection alias association. You use the connection identifier in the DNS TXT record when you're configuring your DNS routing policies.
-- * 'responseStatus' - The response status code.
mkAssociateConnectionAliasResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AssociateConnectionAliasResponse
mkAssociateConnectionAliasResponse pResponseStatus_ =
  AssociateConnectionAliasResponse'
    { connectionIdentifier =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The identifier of the connection alias association. You use the connection identifier in the DNS TXT record when you're configuring your DNS routing policies.
--
-- /Note:/ Consider using 'connectionIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acarsConnectionIdentifier :: Lens.Lens' AssociateConnectionAliasResponse (Lude.Maybe Lude.Text)
acarsConnectionIdentifier = Lens.lens (connectionIdentifier :: AssociateConnectionAliasResponse -> Lude.Maybe Lude.Text) (\s a -> s {connectionIdentifier = a} :: AssociateConnectionAliasResponse)
{-# DEPRECATED acarsConnectionIdentifier "Use generic-lens or generic-optics with 'connectionIdentifier' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acarsResponseStatus :: Lens.Lens' AssociateConnectionAliasResponse Lude.Int
acarsResponseStatus = Lens.lens (responseStatus :: AssociateConnectionAliasResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AssociateConnectionAliasResponse)
{-# DEPRECATED acarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
