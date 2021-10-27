{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexV2Runtime.DeleteSession
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes session information for a specified bot, alias, and user ID.
--
-- You can use this operation to restart a conversation with a bot. When
-- you remove a session, the entire history of the session is removed so
-- that you can start again.
--
-- You don\'t need to delete a session. Sessions have a time limit and will
-- expire. Set the session time limit when you create the bot. The default
-- is 5 minutes, but you can specify anything between 1 minute and 24
-- hours.
--
-- If you specify a bot or alias ID that doesn\'t exist, you receive a
-- @BadRequestException.@
--
-- If the locale doesn\'t exist in the bot, or if the locale hasn\'t been
-- enables for the alias, you receive a @BadRequestException@.
module Network.AWS.LexV2Runtime.DeleteSession
  ( -- * Creating a Request
    DeleteSession (..),
    newDeleteSession,

    -- * Request Lenses
    deleteSession_botId,
    deleteSession_botAliasId,
    deleteSession_sessionId,
    deleteSession_localeId,

    -- * Destructuring the Response
    DeleteSessionResponse (..),
    newDeleteSessionResponse,

    -- * Response Lenses
    deleteSessionResponse_botId,
    deleteSessionResponse_botAliasId,
    deleteSessionResponse_localeId,
    deleteSessionResponse_sessionId,
    deleteSessionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Runtime.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteSession' smart constructor.
data DeleteSession = DeleteSession'
  { -- | The identifier of the bot that contains the session data.
    botId :: Prelude.Text,
    -- | The alias identifier in use for the bot that contains the session data.
    botAliasId :: Prelude.Text,
    -- | The identifier of the session to delete.
    sessionId :: Prelude.Text,
    -- | The locale where the session is in use.
    localeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botId', 'deleteSession_botId' - The identifier of the bot that contains the session data.
--
-- 'botAliasId', 'deleteSession_botAliasId' - The alias identifier in use for the bot that contains the session data.
--
-- 'sessionId', 'deleteSession_sessionId' - The identifier of the session to delete.
--
-- 'localeId', 'deleteSession_localeId' - The locale where the session is in use.
newDeleteSession ::
  -- | 'botId'
  Prelude.Text ->
  -- | 'botAliasId'
  Prelude.Text ->
  -- | 'sessionId'
  Prelude.Text ->
  -- | 'localeId'
  Prelude.Text ->
  DeleteSession
newDeleteSession
  pBotId_
  pBotAliasId_
  pSessionId_
  pLocaleId_ =
    DeleteSession'
      { botId = pBotId_,
        botAliasId = pBotAliasId_,
        sessionId = pSessionId_,
        localeId = pLocaleId_
      }

-- | The identifier of the bot that contains the session data.
deleteSession_botId :: Lens.Lens' DeleteSession Prelude.Text
deleteSession_botId = Lens.lens (\DeleteSession' {botId} -> botId) (\s@DeleteSession' {} a -> s {botId = a} :: DeleteSession)

-- | The alias identifier in use for the bot that contains the session data.
deleteSession_botAliasId :: Lens.Lens' DeleteSession Prelude.Text
deleteSession_botAliasId = Lens.lens (\DeleteSession' {botAliasId} -> botAliasId) (\s@DeleteSession' {} a -> s {botAliasId = a} :: DeleteSession)

-- | The identifier of the session to delete.
deleteSession_sessionId :: Lens.Lens' DeleteSession Prelude.Text
deleteSession_sessionId = Lens.lens (\DeleteSession' {sessionId} -> sessionId) (\s@DeleteSession' {} a -> s {sessionId = a} :: DeleteSession)

-- | The locale where the session is in use.
deleteSession_localeId :: Lens.Lens' DeleteSession Prelude.Text
deleteSession_localeId = Lens.lens (\DeleteSession' {localeId} -> localeId) (\s@DeleteSession' {} a -> s {localeId = a} :: DeleteSession)

instance Core.AWSRequest DeleteSession where
  type
    AWSResponse DeleteSession =
      DeleteSessionResponse
  request = Request.delete defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteSessionResponse'
            Prelude.<$> (x Core..?> "botId")
            Prelude.<*> (x Core..?> "botAliasId")
            Prelude.<*> (x Core..?> "localeId")
            Prelude.<*> (x Core..?> "sessionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteSession

instance Prelude.NFData DeleteSession

instance Core.ToHeaders DeleteSession where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteSession where
  toPath DeleteSession' {..} =
    Prelude.mconcat
      [ "/bots/",
        Core.toBS botId,
        "/botAliases/",
        Core.toBS botAliasId,
        "/botLocales/",
        Core.toBS localeId,
        "/sessions/",
        Core.toBS sessionId
      ]

instance Core.ToQuery DeleteSession where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteSessionResponse' smart constructor.
data DeleteSessionResponse = DeleteSessionResponse'
  { -- | The identifier of the bot that contained the session data.
    botId :: Prelude.Maybe Prelude.Text,
    -- | The alias identifier in use for the bot that contained the session data.
    botAliasId :: Prelude.Maybe Prelude.Text,
    -- | The locale where the session was used.
    localeId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the deleted session.
    sessionId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSessionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botId', 'deleteSessionResponse_botId' - The identifier of the bot that contained the session data.
--
-- 'botAliasId', 'deleteSessionResponse_botAliasId' - The alias identifier in use for the bot that contained the session data.
--
-- 'localeId', 'deleteSessionResponse_localeId' - The locale where the session was used.
--
-- 'sessionId', 'deleteSessionResponse_sessionId' - The identifier of the deleted session.
--
-- 'httpStatus', 'deleteSessionResponse_httpStatus' - The response's http status code.
newDeleteSessionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteSessionResponse
newDeleteSessionResponse pHttpStatus_ =
  DeleteSessionResponse'
    { botId = Prelude.Nothing,
      botAliasId = Prelude.Nothing,
      localeId = Prelude.Nothing,
      sessionId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of the bot that contained the session data.
deleteSessionResponse_botId :: Lens.Lens' DeleteSessionResponse (Prelude.Maybe Prelude.Text)
deleteSessionResponse_botId = Lens.lens (\DeleteSessionResponse' {botId} -> botId) (\s@DeleteSessionResponse' {} a -> s {botId = a} :: DeleteSessionResponse)

-- | The alias identifier in use for the bot that contained the session data.
deleteSessionResponse_botAliasId :: Lens.Lens' DeleteSessionResponse (Prelude.Maybe Prelude.Text)
deleteSessionResponse_botAliasId = Lens.lens (\DeleteSessionResponse' {botAliasId} -> botAliasId) (\s@DeleteSessionResponse' {} a -> s {botAliasId = a} :: DeleteSessionResponse)

-- | The locale where the session was used.
deleteSessionResponse_localeId :: Lens.Lens' DeleteSessionResponse (Prelude.Maybe Prelude.Text)
deleteSessionResponse_localeId = Lens.lens (\DeleteSessionResponse' {localeId} -> localeId) (\s@DeleteSessionResponse' {} a -> s {localeId = a} :: DeleteSessionResponse)

-- | The identifier of the deleted session.
deleteSessionResponse_sessionId :: Lens.Lens' DeleteSessionResponse (Prelude.Maybe Prelude.Text)
deleteSessionResponse_sessionId = Lens.lens (\DeleteSessionResponse' {sessionId} -> sessionId) (\s@DeleteSessionResponse' {} a -> s {sessionId = a} :: DeleteSessionResponse)

-- | The response's http status code.
deleteSessionResponse_httpStatus :: Lens.Lens' DeleteSessionResponse Prelude.Int
deleteSessionResponse_httpStatus = Lens.lens (\DeleteSessionResponse' {httpStatus} -> httpStatus) (\s@DeleteSessionResponse' {} a -> s {httpStatus = a} :: DeleteSessionResponse)

instance Prelude.NFData DeleteSessionResponse
