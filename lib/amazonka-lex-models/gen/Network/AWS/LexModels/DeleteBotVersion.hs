{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.DeleteBotVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specific version of a bot. To delete all versions of a bot, use the 'DeleteBot' operation.
--
-- This operation requires permissions for the @lex:DeleteBotVersion@ action.
module Network.AWS.LexModels.DeleteBotVersion
  ( -- * Creating a request
    DeleteBotVersion (..),
    mkDeleteBotVersion,

    -- ** Request lenses
    dbvName,
    dbvVersion,

    -- * Destructuring the response
    DeleteBotVersionResponse (..),
    mkDeleteBotVersionResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteBotVersion' smart constructor.
data DeleteBotVersion = DeleteBotVersion'
  { name :: Lude.Text,
    version :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteBotVersion' with the minimum fields required to make a request.
--
-- * 'name' - The name of the bot.
-- * 'version' - The version of the bot to delete. You cannot delete the @> LATEST@ version of the bot. To delete the @> LATEST@ version, use the 'DeleteBot' operation.
mkDeleteBotVersion ::
  -- | 'name'
  Lude.Text ->
  -- | 'version'
  Lude.Text ->
  DeleteBotVersion
mkDeleteBotVersion pName_ pVersion_ =
  DeleteBotVersion' {name = pName_, version = pVersion_}

-- | The name of the bot.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbvName :: Lens.Lens' DeleteBotVersion Lude.Text
dbvName = Lens.lens (name :: DeleteBotVersion -> Lude.Text) (\s a -> s {name = a} :: DeleteBotVersion)
{-# DEPRECATED dbvName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The version of the bot to delete. You cannot delete the @> LATEST@ version of the bot. To delete the @> LATEST@ version, use the 'DeleteBot' operation.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbvVersion :: Lens.Lens' DeleteBotVersion Lude.Text
dbvVersion = Lens.lens (version :: DeleteBotVersion -> Lude.Text) (\s a -> s {version = a} :: DeleteBotVersion)
{-# DEPRECATED dbvVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Lude.AWSRequest DeleteBotVersion where
  type Rs DeleteBotVersion = DeleteBotVersionResponse
  request = Req.delete lexModelsService
  response = Res.receiveNull DeleteBotVersionResponse'

instance Lude.ToHeaders DeleteBotVersion where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteBotVersion where
  toPath DeleteBotVersion' {..} =
    Lude.mconcat
      ["/bots/", Lude.toBS name, "/versions/", Lude.toBS version]

instance Lude.ToQuery DeleteBotVersion where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteBotVersionResponse' smart constructor.
data DeleteBotVersionResponse = DeleteBotVersionResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteBotVersionResponse' with the minimum fields required to make a request.
mkDeleteBotVersionResponse ::
  DeleteBotVersionResponse
mkDeleteBotVersionResponse = DeleteBotVersionResponse'
