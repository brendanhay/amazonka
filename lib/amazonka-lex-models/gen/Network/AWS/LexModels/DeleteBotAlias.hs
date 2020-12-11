{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.DeleteBotAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an alias for the specified bot.
--
-- You can't delete an alias that is used in the association between a bot and a messaging channel. If an alias is used in a channel association, the @DeleteBot@ operation returns a @ResourceInUseException@ exception that includes a reference to the channel association that refers to the bot. You can remove the reference to the alias by deleting the channel association. If you get the same exception again, delete the referring association until the @DeleteBotAlias@ operation is successful.
module Network.AWS.LexModels.DeleteBotAlias
  ( -- * Creating a request
    DeleteBotAlias (..),
    mkDeleteBotAlias,

    -- ** Request lenses
    dbaName,
    dbaBotName,

    -- * Destructuring the response
    DeleteBotAliasResponse (..),
    mkDeleteBotAliasResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteBotAlias' smart constructor.
data DeleteBotAlias = DeleteBotAlias'
  { name :: Lude.Text,
    botName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteBotAlias' with the minimum fields required to make a request.
--
-- * 'botName' - The name of the bot that the alias points to.
-- * 'name' - The name of the alias to delete. The name is case sensitive.
mkDeleteBotAlias ::
  -- | 'name'
  Lude.Text ->
  -- | 'botName'
  Lude.Text ->
  DeleteBotAlias
mkDeleteBotAlias pName_ pBotName_ =
  DeleteBotAlias' {name = pName_, botName = pBotName_}

-- | The name of the alias to delete. The name is case sensitive.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbaName :: Lens.Lens' DeleteBotAlias Lude.Text
dbaName = Lens.lens (name :: DeleteBotAlias -> Lude.Text) (\s a -> s {name = a} :: DeleteBotAlias)
{-# DEPRECATED dbaName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The name of the bot that the alias points to.
--
-- /Note:/ Consider using 'botName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbaBotName :: Lens.Lens' DeleteBotAlias Lude.Text
dbaBotName = Lens.lens (botName :: DeleteBotAlias -> Lude.Text) (\s a -> s {botName = a} :: DeleteBotAlias)
{-# DEPRECATED dbaBotName "Use generic-lens or generic-optics with 'botName' instead." #-}

instance Lude.AWSRequest DeleteBotAlias where
  type Rs DeleteBotAlias = DeleteBotAliasResponse
  request = Req.delete lexModelsService
  response = Res.receiveNull DeleteBotAliasResponse'

instance Lude.ToHeaders DeleteBotAlias where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteBotAlias where
  toPath DeleteBotAlias' {..} =
    Lude.mconcat
      ["/bots/", Lude.toBS botName, "/aliases/", Lude.toBS name]

instance Lude.ToQuery DeleteBotAlias where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteBotAliasResponse' smart constructor.
data DeleteBotAliasResponse = DeleteBotAliasResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteBotAliasResponse' with the minimum fields required to make a request.
mkDeleteBotAliasResponse ::
  DeleteBotAliasResponse
mkDeleteBotAliasResponse = DeleteBotAliasResponse'
