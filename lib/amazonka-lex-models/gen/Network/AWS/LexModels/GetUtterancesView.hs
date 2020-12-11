{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.GetUtterancesView
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use the @GetUtterancesView@ operation to get information about the utterances that your users have made to your bot. You can use this list to tune the utterances that your bot responds to.
--
-- For example, say that you have created a bot to order flowers. After your users have used your bot for a while, use the @GetUtterancesView@ operation to see the requests that they have made and whether they have been successful. You might find that the utterance "I want flowers" is not being recognized. You could add this utterance to the @OrderFlowers@ intent so that your bot recognizes that utterance.
-- After you publish a new version of a bot, you can get information about the old version and the new so that you can compare the performance across the two versions.
-- Utterance statistics are generated once a day. Data is available for the last 15 days. You can request information for up to 5 versions of your bot in each request. Amazon Lex returns the most frequent utterances received by the bot in the last 15 days. The response contains information about a maximum of 100 utterances for each version.
-- If you set @childDirected@ field to true when you created your bot, or if you opted out of participating in improving Amazon Lex, utterances are not available.
-- This operation requires permissions for the @lex:GetUtterancesView@ action.
module Network.AWS.LexModels.GetUtterancesView
  ( -- * Creating a request
    GetUtterancesView (..),
    mkGetUtterancesView,

    -- ** Request lenses
    guvBotName,
    guvBotVersions,
    guvStatusType,

    -- * Destructuring the response
    GetUtterancesViewResponse (..),
    mkGetUtterancesViewResponse,

    -- ** Response lenses
    guvrsBotName,
    guvrsUtterances,
    guvrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetUtterancesView' smart constructor.
data GetUtterancesView = GetUtterancesView'
  { botName :: Lude.Text,
    botVersions :: Lude.NonEmpty Lude.Text,
    statusType :: StatusType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetUtterancesView' with the minimum fields required to make a request.
--
-- * 'botName' - The name of the bot for which utterance information should be returned.
-- * 'botVersions' - An array of bot versions for which utterance information should be returned. The limit is 5 versions per request.
-- * 'statusType' - To return utterances that were recognized and handled, use @Detected@ . To return utterances that were not recognized, use @Missed@ .
mkGetUtterancesView ::
  -- | 'botName'
  Lude.Text ->
  -- | 'botVersions'
  Lude.NonEmpty Lude.Text ->
  -- | 'statusType'
  StatusType ->
  GetUtterancesView
mkGetUtterancesView pBotName_ pBotVersions_ pStatusType_ =
  GetUtterancesView'
    { botName = pBotName_,
      botVersions = pBotVersions_,
      statusType = pStatusType_
    }

-- | The name of the bot for which utterance information should be returned.
--
-- /Note:/ Consider using 'botName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guvBotName :: Lens.Lens' GetUtterancesView Lude.Text
guvBotName = Lens.lens (botName :: GetUtterancesView -> Lude.Text) (\s a -> s {botName = a} :: GetUtterancesView)
{-# DEPRECATED guvBotName "Use generic-lens or generic-optics with 'botName' instead." #-}

-- | An array of bot versions for which utterance information should be returned. The limit is 5 versions per request.
--
-- /Note:/ Consider using 'botVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guvBotVersions :: Lens.Lens' GetUtterancesView (Lude.NonEmpty Lude.Text)
guvBotVersions = Lens.lens (botVersions :: GetUtterancesView -> Lude.NonEmpty Lude.Text) (\s a -> s {botVersions = a} :: GetUtterancesView)
{-# DEPRECATED guvBotVersions "Use generic-lens or generic-optics with 'botVersions' instead." #-}

-- | To return utterances that were recognized and handled, use @Detected@ . To return utterances that were not recognized, use @Missed@ .
--
-- /Note:/ Consider using 'statusType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guvStatusType :: Lens.Lens' GetUtterancesView StatusType
guvStatusType = Lens.lens (statusType :: GetUtterancesView -> StatusType) (\s a -> s {statusType = a} :: GetUtterancesView)
{-# DEPRECATED guvStatusType "Use generic-lens or generic-optics with 'statusType' instead." #-}

instance Lude.AWSRequest GetUtterancesView where
  type Rs GetUtterancesView = GetUtterancesViewResponse
  request = Req.get lexModelsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetUtterancesViewResponse'
            Lude.<$> (x Lude..?> "botName")
            Lude.<*> (x Lude..?> "utterances" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetUtterancesView where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetUtterancesView where
  toPath GetUtterancesView' {..} =
    Lude.mconcat ["/bots/", Lude.toBS botName, "/utterances"]

instance Lude.ToQuery GetUtterancesView where
  toQuery GetUtterancesView' {..} =
    Lude.mconcat
      [ "bot_versions" Lude.=: Lude.toQueryList "member" botVersions,
        "status_type" Lude.=: statusType,
        "view=aggregation"
      ]

-- | /See:/ 'mkGetUtterancesViewResponse' smart constructor.
data GetUtterancesViewResponse = GetUtterancesViewResponse'
  { botName ::
      Lude.Maybe Lude.Text,
    utterances ::
      Lude.Maybe [UtteranceList],
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

-- | Creates a value of 'GetUtterancesViewResponse' with the minimum fields required to make a request.
--
-- * 'botName' - The name of the bot for which utterance information was returned.
-- * 'responseStatus' - The response status code.
-- * 'utterances' - An array of 'UtteranceList' objects, each containing a list of 'UtteranceData' objects describing the utterances that were processed by your bot. The response contains a maximum of 100 @UtteranceData@ objects for each version. Amazon Lex returns the most frequent utterances received by the bot in the last 15 days.
mkGetUtterancesViewResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetUtterancesViewResponse
mkGetUtterancesViewResponse pResponseStatus_ =
  GetUtterancesViewResponse'
    { botName = Lude.Nothing,
      utterances = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The name of the bot for which utterance information was returned.
--
-- /Note:/ Consider using 'botName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guvrsBotName :: Lens.Lens' GetUtterancesViewResponse (Lude.Maybe Lude.Text)
guvrsBotName = Lens.lens (botName :: GetUtterancesViewResponse -> Lude.Maybe Lude.Text) (\s a -> s {botName = a} :: GetUtterancesViewResponse)
{-# DEPRECATED guvrsBotName "Use generic-lens or generic-optics with 'botName' instead." #-}

-- | An array of 'UtteranceList' objects, each containing a list of 'UtteranceData' objects describing the utterances that were processed by your bot. The response contains a maximum of 100 @UtteranceData@ objects for each version. Amazon Lex returns the most frequent utterances received by the bot in the last 15 days.
--
-- /Note:/ Consider using 'utterances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guvrsUtterances :: Lens.Lens' GetUtterancesViewResponse (Lude.Maybe [UtteranceList])
guvrsUtterances = Lens.lens (utterances :: GetUtterancesViewResponse -> Lude.Maybe [UtteranceList]) (\s a -> s {utterances = a} :: GetUtterancesViewResponse)
{-# DEPRECATED guvrsUtterances "Use generic-lens or generic-optics with 'utterances' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guvrsResponseStatus :: Lens.Lens' GetUtterancesViewResponse Lude.Int
guvrsResponseStatus = Lens.lens (responseStatus :: GetUtterancesViewResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetUtterancesViewResponse)
{-# DEPRECATED guvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
