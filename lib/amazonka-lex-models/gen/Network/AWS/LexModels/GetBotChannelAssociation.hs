{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.GetBotChannelAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the association between an Amazon Lex bot and a messaging platform.
--
-- This operation requires permissions for the @lex:GetBotChannelAssociation@ action.
module Network.AWS.LexModels.GetBotChannelAssociation
  ( -- * Creating a request
    GetBotChannelAssociation (..),
    mkGetBotChannelAssociation,

    -- ** Request lenses
    gName,
    gBotName,
    gBotAlias,

    -- * Destructuring the response
    GetBotChannelAssociationResponse (..),
    mkGetBotChannelAssociationResponse,

    -- ** Response lenses
    gbcarsFailureReason,
    gbcarsStatus,
    gbcarsBotAlias,
    gbcarsBotName,
    gbcarsBotConfiguration,
    gbcarsCreatedDate,
    gbcarsName,
    gbcarsType,
    gbcarsDescription,
    gbcarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetBotChannelAssociation' smart constructor.
data GetBotChannelAssociation = GetBotChannelAssociation'
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

-- | Creates a value of 'GetBotChannelAssociation' with the minimum fields required to make a request.
--
-- * 'botAlias' - An alias pointing to the specific version of the Amazon Lex bot to which this association is being made.
-- * 'botName' - The name of the Amazon Lex bot.
-- * 'name' - The name of the association between the bot and the channel. The name is case sensitive.
mkGetBotChannelAssociation ::
  -- | 'name'
  Lude.Text ->
  -- | 'botName'
  Lude.Text ->
  -- | 'botAlias'
  Lude.Text ->
  GetBotChannelAssociation
mkGetBotChannelAssociation pName_ pBotName_ pBotAlias_ =
  GetBotChannelAssociation'
    { name = pName_,
      botName = pBotName_,
      botAlias = pBotAlias_
    }

-- | The name of the association between the bot and the channel. The name is case sensitive.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gName :: Lens.Lens' GetBotChannelAssociation Lude.Text
gName = Lens.lens (name :: GetBotChannelAssociation -> Lude.Text) (\s a -> s {name = a} :: GetBotChannelAssociation)
{-# DEPRECATED gName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The name of the Amazon Lex bot.
--
-- /Note:/ Consider using 'botName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gBotName :: Lens.Lens' GetBotChannelAssociation Lude.Text
gBotName = Lens.lens (botName :: GetBotChannelAssociation -> Lude.Text) (\s a -> s {botName = a} :: GetBotChannelAssociation)
{-# DEPRECATED gBotName "Use generic-lens or generic-optics with 'botName' instead." #-}

-- | An alias pointing to the specific version of the Amazon Lex bot to which this association is being made.
--
-- /Note:/ Consider using 'botAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gBotAlias :: Lens.Lens' GetBotChannelAssociation Lude.Text
gBotAlias = Lens.lens (botAlias :: GetBotChannelAssociation -> Lude.Text) (\s a -> s {botAlias = a} :: GetBotChannelAssociation)
{-# DEPRECATED gBotAlias "Use generic-lens or generic-optics with 'botAlias' instead." #-}

instance Lude.AWSRequest GetBotChannelAssociation where
  type Rs GetBotChannelAssociation = GetBotChannelAssociationResponse
  request = Req.get lexModelsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetBotChannelAssociationResponse'
            Lude.<$> (x Lude..?> "failureReason")
            Lude.<*> (x Lude..?> "status")
            Lude.<*> (x Lude..?> "botAlias")
            Lude.<*> (x Lude..?> "botName")
            Lude.<*> (x Lude..?> "botConfiguration" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "createdDate")
            Lude.<*> (x Lude..?> "name")
            Lude.<*> (x Lude..?> "type")
            Lude.<*> (x Lude..?> "description")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetBotChannelAssociation where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetBotChannelAssociation where
  toPath GetBotChannelAssociation' {..} =
    Lude.mconcat
      [ "/bots/",
        Lude.toBS botName,
        "/aliases/",
        Lude.toBS botAlias,
        "/channels/",
        Lude.toBS name
      ]

instance Lude.ToQuery GetBotChannelAssociation where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetBotChannelAssociationResponse' smart constructor.
data GetBotChannelAssociationResponse = GetBotChannelAssociationResponse'
  { failureReason ::
      Lude.Maybe Lude.Text,
    status ::
      Lude.Maybe ChannelStatus,
    botAlias ::
      Lude.Maybe Lude.Text,
    botName ::
      Lude.Maybe Lude.Text,
    botConfiguration ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            (Lude.Text)
        ),
    createdDate ::
      Lude.Maybe Lude.Timestamp,
    name ::
      Lude.Maybe Lude.Text,
    type' ::
      Lude.Maybe ChannelType,
    description ::
      Lude.Maybe Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetBotChannelAssociationResponse' with the minimum fields required to make a request.
--
-- * 'botAlias' - An alias pointing to the specific version of the Amazon Lex bot to which this association is being made.
-- * 'botConfiguration' - Provides information that the messaging platform needs to communicate with the Amazon Lex bot.
-- * 'botName' - The name of the Amazon Lex bot.
-- * 'createdDate' - The date that the association between the bot and the channel was created.
-- * 'description' - A description of the association between the bot and the channel.
-- * 'failureReason' - If @status@ is @FAILED@ , Amazon Lex provides the reason that it failed to create the association.
-- * 'name' - The name of the association between the bot and the channel.
-- * 'responseStatus' - The response status code.
-- * 'status' - The status of the bot channel.
--
--
--     * @CREATED@ - The channel has been created and is ready for use.
--
--
--     * @IN_PROGRESS@ - Channel creation is in progress.
--
--
--     * @FAILED@ - There was an error creating the channel. For information about the reason for the failure, see the @failureReason@ field.
--
--
-- * 'type'' - The type of the messaging platform.
mkGetBotChannelAssociationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetBotChannelAssociationResponse
mkGetBotChannelAssociationResponse pResponseStatus_ =
  GetBotChannelAssociationResponse'
    { failureReason = Lude.Nothing,
      status = Lude.Nothing,
      botAlias = Lude.Nothing,
      botName = Lude.Nothing,
      botConfiguration = Lude.Nothing,
      createdDate = Lude.Nothing,
      name = Lude.Nothing,
      type' = Lude.Nothing,
      description = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If @status@ is @FAILED@ , Amazon Lex provides the reason that it failed to create the association.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbcarsFailureReason :: Lens.Lens' GetBotChannelAssociationResponse (Lude.Maybe Lude.Text)
gbcarsFailureReason = Lens.lens (failureReason :: GetBotChannelAssociationResponse -> Lude.Maybe Lude.Text) (\s a -> s {failureReason = a} :: GetBotChannelAssociationResponse)
{-# DEPRECATED gbcarsFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | The status of the bot channel.
--
--
--     * @CREATED@ - The channel has been created and is ready for use.
--
--
--     * @IN_PROGRESS@ - Channel creation is in progress.
--
--
--     * @FAILED@ - There was an error creating the channel. For information about the reason for the failure, see the @failureReason@ field.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbcarsStatus :: Lens.Lens' GetBotChannelAssociationResponse (Lude.Maybe ChannelStatus)
gbcarsStatus = Lens.lens (status :: GetBotChannelAssociationResponse -> Lude.Maybe ChannelStatus) (\s a -> s {status = a} :: GetBotChannelAssociationResponse)
{-# DEPRECATED gbcarsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | An alias pointing to the specific version of the Amazon Lex bot to which this association is being made.
--
-- /Note:/ Consider using 'botAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbcarsBotAlias :: Lens.Lens' GetBotChannelAssociationResponse (Lude.Maybe Lude.Text)
gbcarsBotAlias = Lens.lens (botAlias :: GetBotChannelAssociationResponse -> Lude.Maybe Lude.Text) (\s a -> s {botAlias = a} :: GetBotChannelAssociationResponse)
{-# DEPRECATED gbcarsBotAlias "Use generic-lens or generic-optics with 'botAlias' instead." #-}

-- | The name of the Amazon Lex bot.
--
-- /Note:/ Consider using 'botName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbcarsBotName :: Lens.Lens' GetBotChannelAssociationResponse (Lude.Maybe Lude.Text)
gbcarsBotName = Lens.lens (botName :: GetBotChannelAssociationResponse -> Lude.Maybe Lude.Text) (\s a -> s {botName = a} :: GetBotChannelAssociationResponse)
{-# DEPRECATED gbcarsBotName "Use generic-lens or generic-optics with 'botName' instead." #-}

-- | Provides information that the messaging platform needs to communicate with the Amazon Lex bot.
--
-- /Note:/ Consider using 'botConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbcarsBotConfiguration :: Lens.Lens' GetBotChannelAssociationResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
gbcarsBotConfiguration = Lens.lens (botConfiguration :: GetBotChannelAssociationResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {botConfiguration = a} :: GetBotChannelAssociationResponse)
{-# DEPRECATED gbcarsBotConfiguration "Use generic-lens or generic-optics with 'botConfiguration' instead." #-}

-- | The date that the association between the bot and the channel was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbcarsCreatedDate :: Lens.Lens' GetBotChannelAssociationResponse (Lude.Maybe Lude.Timestamp)
gbcarsCreatedDate = Lens.lens (createdDate :: GetBotChannelAssociationResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdDate = a} :: GetBotChannelAssociationResponse)
{-# DEPRECATED gbcarsCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | The name of the association between the bot and the channel.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbcarsName :: Lens.Lens' GetBotChannelAssociationResponse (Lude.Maybe Lude.Text)
gbcarsName = Lens.lens (name :: GetBotChannelAssociationResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: GetBotChannelAssociationResponse)
{-# DEPRECATED gbcarsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The type of the messaging platform.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbcarsType :: Lens.Lens' GetBotChannelAssociationResponse (Lude.Maybe ChannelType)
gbcarsType = Lens.lens (type' :: GetBotChannelAssociationResponse -> Lude.Maybe ChannelType) (\s a -> s {type' = a} :: GetBotChannelAssociationResponse)
{-# DEPRECATED gbcarsType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | A description of the association between the bot and the channel.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbcarsDescription :: Lens.Lens' GetBotChannelAssociationResponse (Lude.Maybe Lude.Text)
gbcarsDescription = Lens.lens (description :: GetBotChannelAssociationResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: GetBotChannelAssociationResponse)
{-# DEPRECATED gbcarsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbcarsResponseStatus :: Lens.Lens' GetBotChannelAssociationResponse Lude.Int
gbcarsResponseStatus = Lens.lens (responseStatus :: GetBotChannelAssociationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetBotChannelAssociationResponse)
{-# DEPRECATED gbcarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
