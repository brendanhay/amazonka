{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.CreateConnectionAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates the specified connection alias for use with cross-Region redirection. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/cross-region-redirection.html Cross-Region Redirection for Amazon WorkSpaces> .
module Network.AWS.WorkSpaces.CreateConnectionAlias
  ( -- * Creating a request
    CreateConnectionAlias (..),
    mkCreateConnectionAlias,

    -- ** Request lenses
    ccaTags,
    ccaConnectionString,

    -- * Destructuring the response
    CreateConnectionAliasResponse (..),
    mkCreateConnectionAliasResponse,

    -- ** Response lenses
    ccarsAliasId,
    ccarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'mkCreateConnectionAlias' smart constructor.
data CreateConnectionAlias = CreateConnectionAlias'
  { tags ::
      Lude.Maybe [Tag],
    connectionString :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateConnectionAlias' with the minimum fields required to make a request.
--
-- * 'connectionString' - A connection string in the form of a fully qualified domain name (FQDN), such as @www.example.com@ .
--
-- /Important:/ After you create a connection string, it is always associated to your AWS account. You cannot recreate the same connection string with a different account, even if you delete all instances of it from the original account. The connection string is globally reserved for your account.
-- * 'tags' - The tags to associate with the connection alias.
mkCreateConnectionAlias ::
  -- | 'connectionString'
  Lude.Text ->
  CreateConnectionAlias
mkCreateConnectionAlias pConnectionString_ =
  CreateConnectionAlias'
    { tags = Lude.Nothing,
      connectionString = pConnectionString_
    }

-- | The tags to associate with the connection alias.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccaTags :: Lens.Lens' CreateConnectionAlias (Lude.Maybe [Tag])
ccaTags = Lens.lens (tags :: CreateConnectionAlias -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateConnectionAlias)
{-# DEPRECATED ccaTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | A connection string in the form of a fully qualified domain name (FQDN), such as @www.example.com@ .
--
-- /Important:/ After you create a connection string, it is always associated to your AWS account. You cannot recreate the same connection string with a different account, even if you delete all instances of it from the original account. The connection string is globally reserved for your account.
--
-- /Note:/ Consider using 'connectionString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccaConnectionString :: Lens.Lens' CreateConnectionAlias Lude.Text
ccaConnectionString = Lens.lens (connectionString :: CreateConnectionAlias -> Lude.Text) (\s a -> s {connectionString = a} :: CreateConnectionAlias)
{-# DEPRECATED ccaConnectionString "Use generic-lens or generic-optics with 'connectionString' instead." #-}

instance Lude.AWSRequest CreateConnectionAlias where
  type Rs CreateConnectionAlias = CreateConnectionAliasResponse
  request = Req.postJSON workSpacesService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateConnectionAliasResponse'
            Lude.<$> (x Lude..?> "AliasId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateConnectionAlias where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkspacesService.CreateConnectionAlias" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateConnectionAlias where
  toJSON CreateConnectionAlias' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("ConnectionString" Lude..= connectionString)
          ]
      )

instance Lude.ToPath CreateConnectionAlias where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateConnectionAlias where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateConnectionAliasResponse' smart constructor.
data CreateConnectionAliasResponse = CreateConnectionAliasResponse'
  { aliasId ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateConnectionAliasResponse' with the minimum fields required to make a request.
--
-- * 'aliasId' - The identifier of the connection alias.
-- * 'responseStatus' - The response status code.
mkCreateConnectionAliasResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateConnectionAliasResponse
mkCreateConnectionAliasResponse pResponseStatus_ =
  CreateConnectionAliasResponse'
    { aliasId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The identifier of the connection alias.
--
-- /Note:/ Consider using 'aliasId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccarsAliasId :: Lens.Lens' CreateConnectionAliasResponse (Lude.Maybe Lude.Text)
ccarsAliasId = Lens.lens (aliasId :: CreateConnectionAliasResponse -> Lude.Maybe Lude.Text) (\s a -> s {aliasId = a} :: CreateConnectionAliasResponse)
{-# DEPRECATED ccarsAliasId "Use generic-lens or generic-optics with 'aliasId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccarsResponseStatus :: Lens.Lens' CreateConnectionAliasResponse Lude.Int
ccarsResponseStatus = Lens.lens (responseStatus :: CreateConnectionAliasResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateConnectionAliasResponse)
{-# DEPRECATED ccarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
