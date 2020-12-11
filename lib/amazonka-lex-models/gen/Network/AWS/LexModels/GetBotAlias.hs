{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.GetBotAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about an Amazon Lex bot alias. For more information about aliases, see 'versioning-aliases' .
--
-- This operation requires permissions for the @lex:GetBotAlias@ action.
module Network.AWS.LexModels.GetBotAlias
  ( -- * Creating a request
    GetBotAlias (..),
    mkGetBotAlias,

    -- ** Request lenses
    gbasName,
    gbasBotName,

    -- * Destructuring the response
    GetBotAliasResponse (..),
    mkGetBotAliasResponse,

    -- ** Response lenses
    gbasrsChecksum,
    gbasrsBotVersion,
    gbasrsBotName,
    gbasrsCreatedDate,
    gbasrsName,
    gbasrsConversationLogs,
    gbasrsLastUpdatedDate,
    gbasrsDescription,
    gbasrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetBotAlias' smart constructor.
data GetBotAlias = GetBotAlias'
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

-- | Creates a value of 'GetBotAlias' with the minimum fields required to make a request.
--
-- * 'botName' - The name of the bot.
-- * 'name' - The name of the bot alias. The name is case sensitive.
mkGetBotAlias ::
  -- | 'name'
  Lude.Text ->
  -- | 'botName'
  Lude.Text ->
  GetBotAlias
mkGetBotAlias pName_ pBotName_ =
  GetBotAlias' {name = pName_, botName = pBotName_}

-- | The name of the bot alias. The name is case sensitive.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbasName :: Lens.Lens' GetBotAlias Lude.Text
gbasName = Lens.lens (name :: GetBotAlias -> Lude.Text) (\s a -> s {name = a} :: GetBotAlias)
{-# DEPRECATED gbasName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The name of the bot.
--
-- /Note:/ Consider using 'botName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbasBotName :: Lens.Lens' GetBotAlias Lude.Text
gbasBotName = Lens.lens (botName :: GetBotAlias -> Lude.Text) (\s a -> s {botName = a} :: GetBotAlias)
{-# DEPRECATED gbasBotName "Use generic-lens or generic-optics with 'botName' instead." #-}

instance Lude.AWSRequest GetBotAlias where
  type Rs GetBotAlias = GetBotAliasResponse
  request = Req.get lexModelsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetBotAliasResponse'
            Lude.<$> (x Lude..?> "checksum")
            Lude.<*> (x Lude..?> "botVersion")
            Lude.<*> (x Lude..?> "botName")
            Lude.<*> (x Lude..?> "createdDate")
            Lude.<*> (x Lude..?> "name")
            Lude.<*> (x Lude..?> "conversationLogs")
            Lude.<*> (x Lude..?> "lastUpdatedDate")
            Lude.<*> (x Lude..?> "description")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetBotAlias where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetBotAlias where
  toPath GetBotAlias' {..} =
    Lude.mconcat
      ["/bots/", Lude.toBS botName, "/aliases/", Lude.toBS name]

instance Lude.ToQuery GetBotAlias where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetBotAliasResponse' smart constructor.
data GetBotAliasResponse = GetBotAliasResponse'
  { checksum ::
      Lude.Maybe Lude.Text,
    botVersion :: Lude.Maybe Lude.Text,
    botName :: Lude.Maybe Lude.Text,
    createdDate :: Lude.Maybe Lude.Timestamp,
    name :: Lude.Maybe Lude.Text,
    conversationLogs ::
      Lude.Maybe ConversationLogsResponse,
    lastUpdatedDate :: Lude.Maybe Lude.Timestamp,
    description :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'GetBotAliasResponse' with the minimum fields required to make a request.
--
-- * 'botName' - The name of the bot that the alias points to.
-- * 'botVersion' - The version of the bot that the alias points to.
-- * 'checksum' - Checksum of the bot alias.
-- * 'conversationLogs' - The settings that determine how Amazon Lex uses conversation logs for the alias.
-- * 'createdDate' - The date that the bot alias was created.
-- * 'description' - A description of the bot alias.
-- * 'lastUpdatedDate' - The date that the bot alias was updated. When you create a resource, the creation date and the last updated date are the same.
-- * 'name' - The name of the bot alias.
-- * 'responseStatus' - The response status code.
mkGetBotAliasResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetBotAliasResponse
mkGetBotAliasResponse pResponseStatus_ =
  GetBotAliasResponse'
    { checksum = Lude.Nothing,
      botVersion = Lude.Nothing,
      botName = Lude.Nothing,
      createdDate = Lude.Nothing,
      name = Lude.Nothing,
      conversationLogs = Lude.Nothing,
      lastUpdatedDate = Lude.Nothing,
      description = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Checksum of the bot alias.
--
-- /Note:/ Consider using 'checksum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbasrsChecksum :: Lens.Lens' GetBotAliasResponse (Lude.Maybe Lude.Text)
gbasrsChecksum = Lens.lens (checksum :: GetBotAliasResponse -> Lude.Maybe Lude.Text) (\s a -> s {checksum = a} :: GetBotAliasResponse)
{-# DEPRECATED gbasrsChecksum "Use generic-lens or generic-optics with 'checksum' instead." #-}

-- | The version of the bot that the alias points to.
--
-- /Note:/ Consider using 'botVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbasrsBotVersion :: Lens.Lens' GetBotAliasResponse (Lude.Maybe Lude.Text)
gbasrsBotVersion = Lens.lens (botVersion :: GetBotAliasResponse -> Lude.Maybe Lude.Text) (\s a -> s {botVersion = a} :: GetBotAliasResponse)
{-# DEPRECATED gbasrsBotVersion "Use generic-lens or generic-optics with 'botVersion' instead." #-}

-- | The name of the bot that the alias points to.
--
-- /Note:/ Consider using 'botName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbasrsBotName :: Lens.Lens' GetBotAliasResponse (Lude.Maybe Lude.Text)
gbasrsBotName = Lens.lens (botName :: GetBotAliasResponse -> Lude.Maybe Lude.Text) (\s a -> s {botName = a} :: GetBotAliasResponse)
{-# DEPRECATED gbasrsBotName "Use generic-lens or generic-optics with 'botName' instead." #-}

-- | The date that the bot alias was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbasrsCreatedDate :: Lens.Lens' GetBotAliasResponse (Lude.Maybe Lude.Timestamp)
gbasrsCreatedDate = Lens.lens (createdDate :: GetBotAliasResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdDate = a} :: GetBotAliasResponse)
{-# DEPRECATED gbasrsCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | The name of the bot alias.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbasrsName :: Lens.Lens' GetBotAliasResponse (Lude.Maybe Lude.Text)
gbasrsName = Lens.lens (name :: GetBotAliasResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: GetBotAliasResponse)
{-# DEPRECATED gbasrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The settings that determine how Amazon Lex uses conversation logs for the alias.
--
-- /Note:/ Consider using 'conversationLogs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbasrsConversationLogs :: Lens.Lens' GetBotAliasResponse (Lude.Maybe ConversationLogsResponse)
gbasrsConversationLogs = Lens.lens (conversationLogs :: GetBotAliasResponse -> Lude.Maybe ConversationLogsResponse) (\s a -> s {conversationLogs = a} :: GetBotAliasResponse)
{-# DEPRECATED gbasrsConversationLogs "Use generic-lens or generic-optics with 'conversationLogs' instead." #-}

-- | The date that the bot alias was updated. When you create a resource, the creation date and the last updated date are the same.
--
-- /Note:/ Consider using 'lastUpdatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbasrsLastUpdatedDate :: Lens.Lens' GetBotAliasResponse (Lude.Maybe Lude.Timestamp)
gbasrsLastUpdatedDate = Lens.lens (lastUpdatedDate :: GetBotAliasResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdatedDate = a} :: GetBotAliasResponse)
{-# DEPRECATED gbasrsLastUpdatedDate "Use generic-lens or generic-optics with 'lastUpdatedDate' instead." #-}

-- | A description of the bot alias.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbasrsDescription :: Lens.Lens' GetBotAliasResponse (Lude.Maybe Lude.Text)
gbasrsDescription = Lens.lens (description :: GetBotAliasResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: GetBotAliasResponse)
{-# DEPRECATED gbasrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbasrsResponseStatus :: Lens.Lens' GetBotAliasResponse Lude.Int
gbasrsResponseStatus = Lens.lens (responseStatus :: GetBotAliasResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetBotAliasResponse)
{-# DEPRECATED gbasrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
