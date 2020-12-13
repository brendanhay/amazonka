{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.AssociateLexBot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows the specified Amazon Connect instance to access the specified Amazon Lex bot.
module Network.AWS.Connect.AssociateLexBot
  ( -- * Creating a request
    AssociateLexBot (..),
    mkAssociateLexBot,

    -- ** Request lenses
    albInstanceId,
    albLexBot,

    -- * Destructuring the response
    AssociateLexBotResponse (..),
    mkAssociateLexBotResponse,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAssociateLexBot' smart constructor.
data AssociateLexBot = AssociateLexBot'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Lude.Text,
    -- | The Amazon Lex box to associate with the instance.
    lexBot :: LexBot
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateLexBot' with the minimum fields required to make a request.
--
-- * 'instanceId' - The identifier of the Amazon Connect instance.
-- * 'lexBot' - The Amazon Lex box to associate with the instance.
mkAssociateLexBot ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'lexBot'
  LexBot ->
  AssociateLexBot
mkAssociateLexBot pInstanceId_ pLexBot_ =
  AssociateLexBot' {instanceId = pInstanceId_, lexBot = pLexBot_}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
albInstanceId :: Lens.Lens' AssociateLexBot Lude.Text
albInstanceId = Lens.lens (instanceId :: AssociateLexBot -> Lude.Text) (\s a -> s {instanceId = a} :: AssociateLexBot)
{-# DEPRECATED albInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The Amazon Lex box to associate with the instance.
--
-- /Note:/ Consider using 'lexBot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
albLexBot :: Lens.Lens' AssociateLexBot LexBot
albLexBot = Lens.lens (lexBot :: AssociateLexBot -> LexBot) (\s a -> s {lexBot = a} :: AssociateLexBot)
{-# DEPRECATED albLexBot "Use generic-lens or generic-optics with 'lexBot' instead." #-}

instance Lude.AWSRequest AssociateLexBot where
  type Rs AssociateLexBot = AssociateLexBotResponse
  request = Req.putJSON connectService
  response = Res.receiveNull AssociateLexBotResponse'

instance Lude.ToHeaders AssociateLexBot where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AssociateLexBot where
  toJSON AssociateLexBot' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("LexBot" Lude..= lexBot)])

instance Lude.ToPath AssociateLexBot where
  toPath AssociateLexBot' {..} =
    Lude.mconcat ["/instance/", Lude.toBS instanceId, "/lex-bot"]

instance Lude.ToQuery AssociateLexBot where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAssociateLexBotResponse' smart constructor.
data AssociateLexBotResponse = AssociateLexBotResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateLexBotResponse' with the minimum fields required to make a request.
mkAssociateLexBotResponse ::
  AssociateLexBotResponse
mkAssociateLexBotResponse = AssociateLexBotResponse'
