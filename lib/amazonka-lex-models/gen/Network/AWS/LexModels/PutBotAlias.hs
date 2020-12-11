{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.PutBotAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an alias for the specified version of the bot or replaces an alias for the specified bot. To change the version of the bot that the alias points to, replace the alias. For more information about aliases, see 'versioning-aliases' .
--
-- This operation requires permissions for the @lex:PutBotAlias@ action.
module Network.AWS.LexModels.PutBotAlias
  ( -- * Creating a request
    PutBotAlias (..),
    mkPutBotAlias,

    -- ** Request lenses
    pbaChecksum,
    pbaConversationLogs,
    pbaDescription,
    pbaTags,
    pbaName,
    pbaBotVersion,
    pbaBotName,

    -- * Destructuring the response
    PutBotAliasResponse (..),
    mkPutBotAliasResponse,

    -- ** Response lenses
    pbarsChecksum,
    pbarsBotVersion,
    pbarsBotName,
    pbarsCreatedDate,
    pbarsName,
    pbarsConversationLogs,
    pbarsLastUpdatedDate,
    pbarsDescription,
    pbarsTags,
    pbarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutBotAlias' smart constructor.
data PutBotAlias = PutBotAlias'
  { checksum :: Lude.Maybe Lude.Text,
    conversationLogs :: Lude.Maybe ConversationLogsRequest,
    description :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    name :: Lude.Text,
    botVersion :: Lude.Text,
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

-- | Creates a value of 'PutBotAlias' with the minimum fields required to make a request.
--
-- * 'botName' - The name of the bot.
-- * 'botVersion' - The version of the bot.
-- * 'checksum' - Identifies a specific revision of the @> LATEST@ version.
--
-- When you create a new bot alias, leave the @checksum@ field blank. If you specify a checksum you get a @BadRequestException@ exception.
-- When you want to update a bot alias, set the @checksum@ field to the checksum of the most recent revision of the @> LATEST@ version. If you don't specify the @checksum@ field, or if the checksum does not match the @> LATEST@ version, you get a @PreconditionFailedException@ exception.
-- * 'conversationLogs' - Settings for conversation logs for the alias.
-- * 'description' - A description of the alias.
-- * 'name' - The name of the alias. The name is /not/ case sensitive.
-- * 'tags' - A list of tags to add to the bot alias. You can only add tags when you create an alias, you can't use the @PutBotAlias@ operation to update the tags on a bot alias. To update tags, use the @TagResource@ operation.
mkPutBotAlias ::
  -- | 'name'
  Lude.Text ->
  -- | 'botVersion'
  Lude.Text ->
  -- | 'botName'
  Lude.Text ->
  PutBotAlias
mkPutBotAlias pName_ pBotVersion_ pBotName_ =
  PutBotAlias'
    { checksum = Lude.Nothing,
      conversationLogs = Lude.Nothing,
      description = Lude.Nothing,
      tags = Lude.Nothing,
      name = pName_,
      botVersion = pBotVersion_,
      botName = pBotName_
    }

-- | Identifies a specific revision of the @> LATEST@ version.
--
-- When you create a new bot alias, leave the @checksum@ field blank. If you specify a checksum you get a @BadRequestException@ exception.
-- When you want to update a bot alias, set the @checksum@ field to the checksum of the most recent revision of the @> LATEST@ version. If you don't specify the @checksum@ field, or if the checksum does not match the @> LATEST@ version, you get a @PreconditionFailedException@ exception.
--
-- /Note:/ Consider using 'checksum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbaChecksum :: Lens.Lens' PutBotAlias (Lude.Maybe Lude.Text)
pbaChecksum = Lens.lens (checksum :: PutBotAlias -> Lude.Maybe Lude.Text) (\s a -> s {checksum = a} :: PutBotAlias)
{-# DEPRECATED pbaChecksum "Use generic-lens or generic-optics with 'checksum' instead." #-}

-- | Settings for conversation logs for the alias.
--
-- /Note:/ Consider using 'conversationLogs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbaConversationLogs :: Lens.Lens' PutBotAlias (Lude.Maybe ConversationLogsRequest)
pbaConversationLogs = Lens.lens (conversationLogs :: PutBotAlias -> Lude.Maybe ConversationLogsRequest) (\s a -> s {conversationLogs = a} :: PutBotAlias)
{-# DEPRECATED pbaConversationLogs "Use generic-lens or generic-optics with 'conversationLogs' instead." #-}

-- | A description of the alias.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbaDescription :: Lens.Lens' PutBotAlias (Lude.Maybe Lude.Text)
pbaDescription = Lens.lens (description :: PutBotAlias -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: PutBotAlias)
{-# DEPRECATED pbaDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A list of tags to add to the bot alias. You can only add tags when you create an alias, you can't use the @PutBotAlias@ operation to update the tags on a bot alias. To update tags, use the @TagResource@ operation.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbaTags :: Lens.Lens' PutBotAlias (Lude.Maybe [Tag])
pbaTags = Lens.lens (tags :: PutBotAlias -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: PutBotAlias)
{-# DEPRECATED pbaTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The name of the alias. The name is /not/ case sensitive.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbaName :: Lens.Lens' PutBotAlias Lude.Text
pbaName = Lens.lens (name :: PutBotAlias -> Lude.Text) (\s a -> s {name = a} :: PutBotAlias)
{-# DEPRECATED pbaName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The version of the bot.
--
-- /Note:/ Consider using 'botVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbaBotVersion :: Lens.Lens' PutBotAlias Lude.Text
pbaBotVersion = Lens.lens (botVersion :: PutBotAlias -> Lude.Text) (\s a -> s {botVersion = a} :: PutBotAlias)
{-# DEPRECATED pbaBotVersion "Use generic-lens or generic-optics with 'botVersion' instead." #-}

-- | The name of the bot.
--
-- /Note:/ Consider using 'botName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbaBotName :: Lens.Lens' PutBotAlias Lude.Text
pbaBotName = Lens.lens (botName :: PutBotAlias -> Lude.Text) (\s a -> s {botName = a} :: PutBotAlias)
{-# DEPRECATED pbaBotName "Use generic-lens or generic-optics with 'botName' instead." #-}

instance Lude.AWSRequest PutBotAlias where
  type Rs PutBotAlias = PutBotAliasResponse
  request = Req.putJSON lexModelsService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutBotAliasResponse'
            Lude.<$> (x Lude..?> "checksum")
            Lude.<*> (x Lude..?> "botVersion")
            Lude.<*> (x Lude..?> "botName")
            Lude.<*> (x Lude..?> "createdDate")
            Lude.<*> (x Lude..?> "name")
            Lude.<*> (x Lude..?> "conversationLogs")
            Lude.<*> (x Lude..?> "lastUpdatedDate")
            Lude.<*> (x Lude..?> "description")
            Lude.<*> (x Lude..?> "tags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutBotAlias where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutBotAlias where
  toJSON PutBotAlias' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("checksum" Lude..=) Lude.<$> checksum,
            ("conversationLogs" Lude..=) Lude.<$> conversationLogs,
            ("description" Lude..=) Lude.<$> description,
            ("tags" Lude..=) Lude.<$> tags,
            Lude.Just ("botVersion" Lude..= botVersion)
          ]
      )

instance Lude.ToPath PutBotAlias where
  toPath PutBotAlias' {..} =
    Lude.mconcat
      ["/bots/", Lude.toBS botName, "/aliases/", Lude.toBS name]

instance Lude.ToQuery PutBotAlias where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutBotAliasResponse' smart constructor.
data PutBotAliasResponse = PutBotAliasResponse'
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
    tags :: Lude.Maybe [Tag],
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

-- | Creates a value of 'PutBotAliasResponse' with the minimum fields required to make a request.
--
-- * 'botName' - The name of the bot that the alias points to.
-- * 'botVersion' - The version of the bot that the alias points to.
-- * 'checksum' - The checksum for the current version of the alias.
-- * 'conversationLogs' - The settings that determine how Amazon Lex uses conversation logs for the alias.
-- * 'createdDate' - The date that the bot alias was created.
-- * 'description' - A description of the alias.
-- * 'lastUpdatedDate' - The date that the bot alias was updated. When you create a resource, the creation date and the last updated date are the same.
-- * 'name' - The name of the alias.
-- * 'responseStatus' - The response status code.
-- * 'tags' - A list of tags associated with a bot.
mkPutBotAliasResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutBotAliasResponse
mkPutBotAliasResponse pResponseStatus_ =
  PutBotAliasResponse'
    { checksum = Lude.Nothing,
      botVersion = Lude.Nothing,
      botName = Lude.Nothing,
      createdDate = Lude.Nothing,
      name = Lude.Nothing,
      conversationLogs = Lude.Nothing,
      lastUpdatedDate = Lude.Nothing,
      description = Lude.Nothing,
      tags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The checksum for the current version of the alias.
--
-- /Note:/ Consider using 'checksum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbarsChecksum :: Lens.Lens' PutBotAliasResponse (Lude.Maybe Lude.Text)
pbarsChecksum = Lens.lens (checksum :: PutBotAliasResponse -> Lude.Maybe Lude.Text) (\s a -> s {checksum = a} :: PutBotAliasResponse)
{-# DEPRECATED pbarsChecksum "Use generic-lens or generic-optics with 'checksum' instead." #-}

-- | The version of the bot that the alias points to.
--
-- /Note:/ Consider using 'botVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbarsBotVersion :: Lens.Lens' PutBotAliasResponse (Lude.Maybe Lude.Text)
pbarsBotVersion = Lens.lens (botVersion :: PutBotAliasResponse -> Lude.Maybe Lude.Text) (\s a -> s {botVersion = a} :: PutBotAliasResponse)
{-# DEPRECATED pbarsBotVersion "Use generic-lens or generic-optics with 'botVersion' instead." #-}

-- | The name of the bot that the alias points to.
--
-- /Note:/ Consider using 'botName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbarsBotName :: Lens.Lens' PutBotAliasResponse (Lude.Maybe Lude.Text)
pbarsBotName = Lens.lens (botName :: PutBotAliasResponse -> Lude.Maybe Lude.Text) (\s a -> s {botName = a} :: PutBotAliasResponse)
{-# DEPRECATED pbarsBotName "Use generic-lens or generic-optics with 'botName' instead." #-}

-- | The date that the bot alias was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbarsCreatedDate :: Lens.Lens' PutBotAliasResponse (Lude.Maybe Lude.Timestamp)
pbarsCreatedDate = Lens.lens (createdDate :: PutBotAliasResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdDate = a} :: PutBotAliasResponse)
{-# DEPRECATED pbarsCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | The name of the alias.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbarsName :: Lens.Lens' PutBotAliasResponse (Lude.Maybe Lude.Text)
pbarsName = Lens.lens (name :: PutBotAliasResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: PutBotAliasResponse)
{-# DEPRECATED pbarsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The settings that determine how Amazon Lex uses conversation logs for the alias.
--
-- /Note:/ Consider using 'conversationLogs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbarsConversationLogs :: Lens.Lens' PutBotAliasResponse (Lude.Maybe ConversationLogsResponse)
pbarsConversationLogs = Lens.lens (conversationLogs :: PutBotAliasResponse -> Lude.Maybe ConversationLogsResponse) (\s a -> s {conversationLogs = a} :: PutBotAliasResponse)
{-# DEPRECATED pbarsConversationLogs "Use generic-lens or generic-optics with 'conversationLogs' instead." #-}

-- | The date that the bot alias was updated. When you create a resource, the creation date and the last updated date are the same.
--
-- /Note:/ Consider using 'lastUpdatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbarsLastUpdatedDate :: Lens.Lens' PutBotAliasResponse (Lude.Maybe Lude.Timestamp)
pbarsLastUpdatedDate = Lens.lens (lastUpdatedDate :: PutBotAliasResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdatedDate = a} :: PutBotAliasResponse)
{-# DEPRECATED pbarsLastUpdatedDate "Use generic-lens or generic-optics with 'lastUpdatedDate' instead." #-}

-- | A description of the alias.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbarsDescription :: Lens.Lens' PutBotAliasResponse (Lude.Maybe Lude.Text)
pbarsDescription = Lens.lens (description :: PutBotAliasResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: PutBotAliasResponse)
{-# DEPRECATED pbarsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A list of tags associated with a bot.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbarsTags :: Lens.Lens' PutBotAliasResponse (Lude.Maybe [Tag])
pbarsTags = Lens.lens (tags :: PutBotAliasResponse -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: PutBotAliasResponse)
{-# DEPRECATED pbarsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbarsResponseStatus :: Lens.Lens' PutBotAliasResponse Lude.Int
pbarsResponseStatus = Lens.lens (responseStatus :: PutBotAliasResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutBotAliasResponse)
{-# DEPRECATED pbarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
