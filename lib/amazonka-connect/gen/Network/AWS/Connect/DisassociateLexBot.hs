{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.DisassociateLexBot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Revokes authorization from the specified instance to access the specified Amazon Lex bot.
module Network.AWS.Connect.DisassociateLexBot
  ( -- * Creating a request
    DisassociateLexBot (..),
    mkDisassociateLexBot,

    -- ** Request lenses
    dlbInstanceId,
    dlbBotName,
    dlbLexRegion,

    -- * Destructuring the response
    DisassociateLexBotResponse (..),
    mkDisassociateLexBotResponse,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDisassociateLexBot' smart constructor.
data DisassociateLexBot = DisassociateLexBot'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Lude.Text,
    -- | The name of the Amazon Lex bot. Maximum character limit of 50.
    botName :: Lude.Text,
    -- | The Region in which the Amazon Lex bot has been created.
    lexRegion :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateLexBot' with the minimum fields required to make a request.
--
-- * 'instanceId' - The identifier of the Amazon Connect instance.
-- * 'botName' - The name of the Amazon Lex bot. Maximum character limit of 50.
-- * 'lexRegion' - The Region in which the Amazon Lex bot has been created.
mkDisassociateLexBot ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'botName'
  Lude.Text ->
  -- | 'lexRegion'
  Lude.Text ->
  DisassociateLexBot
mkDisassociateLexBot pInstanceId_ pBotName_ pLexRegion_ =
  DisassociateLexBot'
    { instanceId = pInstanceId_,
      botName = pBotName_,
      lexRegion = pLexRegion_
    }

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbInstanceId :: Lens.Lens' DisassociateLexBot Lude.Text
dlbInstanceId = Lens.lens (instanceId :: DisassociateLexBot -> Lude.Text) (\s a -> s {instanceId = a} :: DisassociateLexBot)
{-# DEPRECATED dlbInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The name of the Amazon Lex bot. Maximum character limit of 50.
--
-- /Note:/ Consider using 'botName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbBotName :: Lens.Lens' DisassociateLexBot Lude.Text
dlbBotName = Lens.lens (botName :: DisassociateLexBot -> Lude.Text) (\s a -> s {botName = a} :: DisassociateLexBot)
{-# DEPRECATED dlbBotName "Use generic-lens or generic-optics with 'botName' instead." #-}

-- | The Region in which the Amazon Lex bot has been created.
--
-- /Note:/ Consider using 'lexRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbLexRegion :: Lens.Lens' DisassociateLexBot Lude.Text
dlbLexRegion = Lens.lens (lexRegion :: DisassociateLexBot -> Lude.Text) (\s a -> s {lexRegion = a} :: DisassociateLexBot)
{-# DEPRECATED dlbLexRegion "Use generic-lens or generic-optics with 'lexRegion' instead." #-}

instance Lude.AWSRequest DisassociateLexBot where
  type Rs DisassociateLexBot = DisassociateLexBotResponse
  request = Req.delete connectService
  response = Res.receiveNull DisassociateLexBotResponse'

instance Lude.ToHeaders DisassociateLexBot where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DisassociateLexBot where
  toPath DisassociateLexBot' {..} =
    Lude.mconcat ["/instance/", Lude.toBS instanceId, "/lex-bot"]

instance Lude.ToQuery DisassociateLexBot where
  toQuery DisassociateLexBot' {..} =
    Lude.mconcat
      ["botName" Lude.=: botName, "lexRegion" Lude.=: lexRegion]

-- | /See:/ 'mkDisassociateLexBotResponse' smart constructor.
data DisassociateLexBotResponse = DisassociateLexBotResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateLexBotResponse' with the minimum fields required to make a request.
mkDisassociateLexBotResponse ::
  DisassociateLexBotResponse
mkDisassociateLexBotResponse = DisassociateLexBotResponse'
