{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexRuntime.DeleteSession
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes session information for a specified bot, alias, and user ID.
module Network.AWS.LexRuntime.DeleteSession
  ( -- * Creating a request
    DeleteSession (..),
    mkDeleteSession,

    -- ** Request lenses
    dsBotAlias,
    dsBotName,
    dsUserId,

    -- * Destructuring the response
    DeleteSessionResponse (..),
    mkDeleteSessionResponse,

    -- ** Response lenses
    dsrsBotAlias,
    dsrsBotName,
    dsrsUserId,
    dsrsSessionId,
    dsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexRuntime.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteSession' smart constructor.
data DeleteSession = DeleteSession'
  { -- | The alias in use for the bot that contains the session data.
    botAlias :: Lude.Text,
    -- | The name of the bot that contains the session data.
    botName :: Lude.Text,
    -- | The identifier of the user associated with the session data.
    userId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteSession' with the minimum fields required to make a request.
--
-- * 'botAlias' - The alias in use for the bot that contains the session data.
-- * 'botName' - The name of the bot that contains the session data.
-- * 'userId' - The identifier of the user associated with the session data.
mkDeleteSession ::
  -- | 'botAlias'
  Lude.Text ->
  -- | 'botName'
  Lude.Text ->
  -- | 'userId'
  Lude.Text ->
  DeleteSession
mkDeleteSession pBotAlias_ pBotName_ pUserId_ =
  DeleteSession'
    { botAlias = pBotAlias_,
      botName = pBotName_,
      userId = pUserId_
    }

-- | The alias in use for the bot that contains the session data.
--
-- /Note:/ Consider using 'botAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsBotAlias :: Lens.Lens' DeleteSession Lude.Text
dsBotAlias = Lens.lens (botAlias :: DeleteSession -> Lude.Text) (\s a -> s {botAlias = a} :: DeleteSession)
{-# DEPRECATED dsBotAlias "Use generic-lens or generic-optics with 'botAlias' instead." #-}

-- | The name of the bot that contains the session data.
--
-- /Note:/ Consider using 'botName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsBotName :: Lens.Lens' DeleteSession Lude.Text
dsBotName = Lens.lens (botName :: DeleteSession -> Lude.Text) (\s a -> s {botName = a} :: DeleteSession)
{-# DEPRECATED dsBotName "Use generic-lens or generic-optics with 'botName' instead." #-}

-- | The identifier of the user associated with the session data.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsUserId :: Lens.Lens' DeleteSession Lude.Text
dsUserId = Lens.lens (userId :: DeleteSession -> Lude.Text) (\s a -> s {userId = a} :: DeleteSession)
{-# DEPRECATED dsUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

instance Lude.AWSRequest DeleteSession where
  type Rs DeleteSession = DeleteSessionResponse
  request = Req.delete lexRuntimeService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteSessionResponse'
            Lude.<$> (x Lude..?> "botAlias")
            Lude.<*> (x Lude..?> "botName")
            Lude.<*> (x Lude..?> "userId")
            Lude.<*> (x Lude..?> "sessionId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteSession where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteSession where
  toPath DeleteSession' {..} =
    Lude.mconcat
      [ "/bot/",
        Lude.toBS botName,
        "/alias/",
        Lude.toBS botAlias,
        "/user/",
        Lude.toBS userId,
        "/session"
      ]

instance Lude.ToQuery DeleteSession where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteSessionResponse' smart constructor.
data DeleteSessionResponse = DeleteSessionResponse'
  { -- | The alias in use for the bot associated with the session data.
    botAlias :: Lude.Maybe Lude.Text,
    -- | The name of the bot associated with the session data.
    botName :: Lude.Maybe Lude.Text,
    -- | The ID of the client application user.
    userId :: Lude.Maybe Lude.Text,
    -- | The unique identifier for the session.
    sessionId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteSessionResponse' with the minimum fields required to make a request.
--
-- * 'botAlias' - The alias in use for the bot associated with the session data.
-- * 'botName' - The name of the bot associated with the session data.
-- * 'userId' - The ID of the client application user.
-- * 'sessionId' - The unique identifier for the session.
-- * 'responseStatus' - The response status code.
mkDeleteSessionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteSessionResponse
mkDeleteSessionResponse pResponseStatus_ =
  DeleteSessionResponse'
    { botAlias = Lude.Nothing,
      botName = Lude.Nothing,
      userId = Lude.Nothing,
      sessionId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The alias in use for the bot associated with the session data.
--
-- /Note:/ Consider using 'botAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsBotAlias :: Lens.Lens' DeleteSessionResponse (Lude.Maybe Lude.Text)
dsrsBotAlias = Lens.lens (botAlias :: DeleteSessionResponse -> Lude.Maybe Lude.Text) (\s a -> s {botAlias = a} :: DeleteSessionResponse)
{-# DEPRECATED dsrsBotAlias "Use generic-lens or generic-optics with 'botAlias' instead." #-}

-- | The name of the bot associated with the session data.
--
-- /Note:/ Consider using 'botName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsBotName :: Lens.Lens' DeleteSessionResponse (Lude.Maybe Lude.Text)
dsrsBotName = Lens.lens (botName :: DeleteSessionResponse -> Lude.Maybe Lude.Text) (\s a -> s {botName = a} :: DeleteSessionResponse)
{-# DEPRECATED dsrsBotName "Use generic-lens or generic-optics with 'botName' instead." #-}

-- | The ID of the client application user.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsUserId :: Lens.Lens' DeleteSessionResponse (Lude.Maybe Lude.Text)
dsrsUserId = Lens.lens (userId :: DeleteSessionResponse -> Lude.Maybe Lude.Text) (\s a -> s {userId = a} :: DeleteSessionResponse)
{-# DEPRECATED dsrsUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

-- | The unique identifier for the session.
--
-- /Note:/ Consider using 'sessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsSessionId :: Lens.Lens' DeleteSessionResponse (Lude.Maybe Lude.Text)
dsrsSessionId = Lens.lens (sessionId :: DeleteSessionResponse -> Lude.Maybe Lude.Text) (\s a -> s {sessionId = a} :: DeleteSessionResponse)
{-# DEPRECATED dsrsSessionId "Use generic-lens or generic-optics with 'sessionId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsResponseStatus :: Lens.Lens' DeleteSessionResponse Lude.Int
dsrsResponseStatus = Lens.lens (responseStatus :: DeleteSessionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteSessionResponse)
{-# DEPRECATED dsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
