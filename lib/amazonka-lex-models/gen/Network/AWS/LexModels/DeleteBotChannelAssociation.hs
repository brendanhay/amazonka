{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.DeleteBotChannelAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the association between an Amazon Lex bot and a messaging platform.
--
-- This operation requires permission for the @lex:DeleteBotChannelAssociation@ action.
module Network.AWS.LexModels.DeleteBotChannelAssociation
  ( -- * Creating a request
    DeleteBotChannelAssociation (..),
    mkDeleteBotChannelAssociation,

    -- ** Request lenses
    dbcaName,
    dbcaBotName,
    dbcaBotAlias,

    -- * Destructuring the response
    DeleteBotChannelAssociationResponse (..),
    mkDeleteBotChannelAssociationResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteBotChannelAssociation' smart constructor.
data DeleteBotChannelAssociation = DeleteBotChannelAssociation'
  { name ::
      Lude.Text,
    botName :: Lude.Text,
    botAlias :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteBotChannelAssociation' with the minimum fields required to make a request.
--
-- * 'botAlias' - An alias that points to the specific version of the Amazon Lex bot to which this association is being made.
-- * 'botName' - The name of the Amazon Lex bot.
-- * 'name' - The name of the association. The name is case sensitive.
mkDeleteBotChannelAssociation ::
  -- | 'name'
  Lude.Text ->
  -- | 'botName'
  Lude.Text ->
  -- | 'botAlias'
  Lude.Text ->
  DeleteBotChannelAssociation
mkDeleteBotChannelAssociation pName_ pBotName_ pBotAlias_ =
  DeleteBotChannelAssociation'
    { name = pName_,
      botName = pBotName_,
      botAlias = pBotAlias_
    }

-- | The name of the association. The name is case sensitive.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcaName :: Lens.Lens' DeleteBotChannelAssociation Lude.Text
dbcaName = Lens.lens (name :: DeleteBotChannelAssociation -> Lude.Text) (\s a -> s {name = a} :: DeleteBotChannelAssociation)
{-# DEPRECATED dbcaName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The name of the Amazon Lex bot.
--
-- /Note:/ Consider using 'botName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcaBotName :: Lens.Lens' DeleteBotChannelAssociation Lude.Text
dbcaBotName = Lens.lens (botName :: DeleteBotChannelAssociation -> Lude.Text) (\s a -> s {botName = a} :: DeleteBotChannelAssociation)
{-# DEPRECATED dbcaBotName "Use generic-lens or generic-optics with 'botName' instead." #-}

-- | An alias that points to the specific version of the Amazon Lex bot to which this association is being made.
--
-- /Note:/ Consider using 'botAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcaBotAlias :: Lens.Lens' DeleteBotChannelAssociation Lude.Text
dbcaBotAlias = Lens.lens (botAlias :: DeleteBotChannelAssociation -> Lude.Text) (\s a -> s {botAlias = a} :: DeleteBotChannelAssociation)
{-# DEPRECATED dbcaBotAlias "Use generic-lens or generic-optics with 'botAlias' instead." #-}

instance Lude.AWSRequest DeleteBotChannelAssociation where
  type
    Rs DeleteBotChannelAssociation =
      DeleteBotChannelAssociationResponse
  request = Req.delete lexModelsService
  response = Res.receiveNull DeleteBotChannelAssociationResponse'

instance Lude.ToHeaders DeleteBotChannelAssociation where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteBotChannelAssociation where
  toPath DeleteBotChannelAssociation' {..} =
    Lude.mconcat
      [ "/bots/",
        Lude.toBS botName,
        "/aliases/",
        Lude.toBS botAlias,
        "/channels/",
        Lude.toBS name
      ]

instance Lude.ToQuery DeleteBotChannelAssociation where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteBotChannelAssociationResponse' smart constructor.
data DeleteBotChannelAssociationResponse = DeleteBotChannelAssociationResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteBotChannelAssociationResponse' with the minimum fields required to make a request.
mkDeleteBotChannelAssociationResponse ::
  DeleteBotChannelAssociationResponse
mkDeleteBotChannelAssociationResponse =
  DeleteBotChannelAssociationResponse'
