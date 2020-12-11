{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.DeleteUtterances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes stored utterances.
--
-- Amazon Lex stores the utterances that users send to your bot. Utterances are stored for 15 days for use with the 'GetUtterancesView' operation, and then stored indefinitely for use in improving the ability of your bot to respond to user input.
-- Use the @DeleteUtterances@ operation to manually delete stored utterances for a specific user. When you use the @DeleteUtterances@ operation, utterances stored for improving your bot's ability to respond to user input are deleted immediately. Utterances stored for use with the @GetUtterancesView@ operation are deleted after 15 days.
-- This operation requires permissions for the @lex:DeleteUtterances@ action.
module Network.AWS.LexModels.DeleteUtterances
  ( -- * Creating a request
    DeleteUtterances (..),
    mkDeleteUtterances,

    -- ** Request lenses
    duBotName,
    duUserId,

    -- * Destructuring the response
    DeleteUtterancesResponse (..),
    mkDeleteUtterancesResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteUtterances' smart constructor.
data DeleteUtterances = DeleteUtterances'
  { botName :: Lude.Text,
    userId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteUtterances' with the minimum fields required to make a request.
--
-- * 'botName' - The name of the bot that stored the utterances.
-- * 'userId' - The unique identifier for the user that made the utterances. This is the user ID that was sent in the <http://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostContent.html PostContent> or <http://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostText.html PostText> operation request that contained the utterance.
mkDeleteUtterances ::
  -- | 'botName'
  Lude.Text ->
  -- | 'userId'
  Lude.Text ->
  DeleteUtterances
mkDeleteUtterances pBotName_ pUserId_ =
  DeleteUtterances' {botName = pBotName_, userId = pUserId_}

-- | The name of the bot that stored the utterances.
--
-- /Note:/ Consider using 'botName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duBotName :: Lens.Lens' DeleteUtterances Lude.Text
duBotName = Lens.lens (botName :: DeleteUtterances -> Lude.Text) (\s a -> s {botName = a} :: DeleteUtterances)
{-# DEPRECATED duBotName "Use generic-lens or generic-optics with 'botName' instead." #-}

-- | The unique identifier for the user that made the utterances. This is the user ID that was sent in the <http://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostContent.html PostContent> or <http://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostText.html PostText> operation request that contained the utterance.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duUserId :: Lens.Lens' DeleteUtterances Lude.Text
duUserId = Lens.lens (userId :: DeleteUtterances -> Lude.Text) (\s a -> s {userId = a} :: DeleteUtterances)
{-# DEPRECATED duUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

instance Lude.AWSRequest DeleteUtterances where
  type Rs DeleteUtterances = DeleteUtterancesResponse
  request = Req.delete lexModelsService
  response = Res.receiveNull DeleteUtterancesResponse'

instance Lude.ToHeaders DeleteUtterances where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteUtterances where
  toPath DeleteUtterances' {..} =
    Lude.mconcat
      ["/bots/", Lude.toBS botName, "/utterances/", Lude.toBS userId]

instance Lude.ToQuery DeleteUtterances where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteUtterancesResponse' smart constructor.
data DeleteUtterancesResponse = DeleteUtterancesResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteUtterancesResponse' with the minimum fields required to make a request.
mkDeleteUtterancesResponse ::
  DeleteUtterancesResponse
mkDeleteUtterancesResponse = DeleteUtterancesResponse'
