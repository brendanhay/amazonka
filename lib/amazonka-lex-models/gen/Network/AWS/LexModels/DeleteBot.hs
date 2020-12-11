{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.DeleteBot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes all versions of the bot, including the @> LATEST@ version. To delete a specific version of the bot, use the 'DeleteBotVersion' operation. The @DeleteBot@ operation doesn't immediately remove the bot schema. Instead, it is marked for deletion and removed later.
--
-- Amazon Lex stores utterances indefinitely for improving the ability of your bot to respond to user inputs. These utterances are not removed when the bot is deleted. To remove the utterances, use the 'DeleteUtterances' operation.
-- If a bot has an alias, you can't delete it. Instead, the @DeleteBot@ operation returns a @ResourceInUseException@ exception that includes a reference to the alias that refers to the bot. To remove the reference to the bot, delete the alias. If you get the same exception again, delete the referring alias until the @DeleteBot@ operation is successful.
-- This operation requires permissions for the @lex:DeleteBot@ action.
module Network.AWS.LexModels.DeleteBot
  ( -- * Creating a request
    DeleteBot (..),
    mkDeleteBot,

    -- ** Request lenses
    dbName,

    -- * Destructuring the response
    DeleteBotResponse (..),
    mkDeleteBotResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteBot' smart constructor.
newtype DeleteBot = DeleteBot' {name :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteBot' with the minimum fields required to make a request.
--
-- * 'name' - The name of the bot. The name is case sensitive.
mkDeleteBot ::
  -- | 'name'
  Lude.Text ->
  DeleteBot
mkDeleteBot pName_ = DeleteBot' {name = pName_}

-- | The name of the bot. The name is case sensitive.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbName :: Lens.Lens' DeleteBot Lude.Text
dbName = Lens.lens (name :: DeleteBot -> Lude.Text) (\s a -> s {name = a} :: DeleteBot)
{-# DEPRECATED dbName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest DeleteBot where
  type Rs DeleteBot = DeleteBotResponse
  request = Req.delete lexModelsService
  response = Res.receiveNull DeleteBotResponse'

instance Lude.ToHeaders DeleteBot where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteBot where
  toPath DeleteBot' {..} = Lude.mconcat ["/bots/", Lude.toBS name]

instance Lude.ToQuery DeleteBot where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteBotResponse' smart constructor.
data DeleteBotResponse = DeleteBotResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteBotResponse' with the minimum fields required to make a request.
mkDeleteBotResponse ::
  DeleteBotResponse
mkDeleteBotResponse = DeleteBotResponse'
