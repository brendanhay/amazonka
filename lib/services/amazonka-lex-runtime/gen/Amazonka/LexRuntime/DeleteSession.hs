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
-- Module      : Amazonka.LexRuntime.DeleteSession
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes session information for a specified bot, alias, and user ID.
module Amazonka.LexRuntime.DeleteSession
  ( -- * Creating a Request
    DeleteSession (..),
    newDeleteSession,

    -- * Request Lenses
    deleteSession_botName,
    deleteSession_botAlias,
    deleteSession_userId,

    -- * Destructuring the Response
    DeleteSessionResponse (..),
    newDeleteSessionResponse,

    -- * Response Lenses
    deleteSessionResponse_botAlias,
    deleteSessionResponse_botName,
    deleteSessionResponse_sessionId,
    deleteSessionResponse_userId,
    deleteSessionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexRuntime.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteSession' smart constructor.
data DeleteSession = DeleteSession'
  { -- | The name of the bot that contains the session data.
    botName :: Prelude.Text,
    -- | The alias in use for the bot that contains the session data.
    botAlias :: Prelude.Text,
    -- | The identifier of the user associated with the session data.
    userId :: Prelude.Text
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
-- 'botName', 'deleteSession_botName' - The name of the bot that contains the session data.
--
-- 'botAlias', 'deleteSession_botAlias' - The alias in use for the bot that contains the session data.
--
-- 'userId', 'deleteSession_userId' - The identifier of the user associated with the session data.
newDeleteSession ::
  -- | 'botName'
  Prelude.Text ->
  -- | 'botAlias'
  Prelude.Text ->
  -- | 'userId'
  Prelude.Text ->
  DeleteSession
newDeleteSession pBotName_ pBotAlias_ pUserId_ =
  DeleteSession'
    { botName = pBotName_,
      botAlias = pBotAlias_,
      userId = pUserId_
    }

-- | The name of the bot that contains the session data.
deleteSession_botName :: Lens.Lens' DeleteSession Prelude.Text
deleteSession_botName = Lens.lens (\DeleteSession' {botName} -> botName) (\s@DeleteSession' {} a -> s {botName = a} :: DeleteSession)

-- | The alias in use for the bot that contains the session data.
deleteSession_botAlias :: Lens.Lens' DeleteSession Prelude.Text
deleteSession_botAlias = Lens.lens (\DeleteSession' {botAlias} -> botAlias) (\s@DeleteSession' {} a -> s {botAlias = a} :: DeleteSession)

-- | The identifier of the user associated with the session data.
deleteSession_userId :: Lens.Lens' DeleteSession Prelude.Text
deleteSession_userId = Lens.lens (\DeleteSession' {userId} -> userId) (\s@DeleteSession' {} a -> s {userId = a} :: DeleteSession)

instance Core.AWSRequest DeleteSession where
  type
    AWSResponse DeleteSession =
      DeleteSessionResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteSessionResponse'
            Prelude.<$> (x Data..?> "botAlias")
            Prelude.<*> (x Data..?> "botName")
            Prelude.<*> (x Data..?> "sessionId")
            Prelude.<*> (x Data..?> "userId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteSession where
  hashWithSalt _salt DeleteSession' {..} =
    _salt
      `Prelude.hashWithSalt` botName
      `Prelude.hashWithSalt` botAlias
      `Prelude.hashWithSalt` userId

instance Prelude.NFData DeleteSession where
  rnf DeleteSession' {..} =
    Prelude.rnf botName
      `Prelude.seq` Prelude.rnf botAlias
      `Prelude.seq` Prelude.rnf userId

instance Data.ToHeaders DeleteSession where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteSession where
  toPath DeleteSession' {..} =
    Prelude.mconcat
      [ "/bot/",
        Data.toBS botName,
        "/alias/",
        Data.toBS botAlias,
        "/user/",
        Data.toBS userId,
        "/session"
      ]

instance Data.ToQuery DeleteSession where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteSessionResponse' smart constructor.
data DeleteSessionResponse = DeleteSessionResponse'
  { -- | The alias in use for the bot associated with the session data.
    botAlias :: Prelude.Maybe Prelude.Text,
    -- | The name of the bot associated with the session data.
    botName :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the session.
    sessionId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the client application user.
    userId :: Prelude.Maybe Prelude.Text,
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
-- 'botAlias', 'deleteSessionResponse_botAlias' - The alias in use for the bot associated with the session data.
--
-- 'botName', 'deleteSessionResponse_botName' - The name of the bot associated with the session data.
--
-- 'sessionId', 'deleteSessionResponse_sessionId' - The unique identifier for the session.
--
-- 'userId', 'deleteSessionResponse_userId' - The ID of the client application user.
--
-- 'httpStatus', 'deleteSessionResponse_httpStatus' - The response's http status code.
newDeleteSessionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteSessionResponse
newDeleteSessionResponse pHttpStatus_ =
  DeleteSessionResponse'
    { botAlias = Prelude.Nothing,
      botName = Prelude.Nothing,
      sessionId = Prelude.Nothing,
      userId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The alias in use for the bot associated with the session data.
deleteSessionResponse_botAlias :: Lens.Lens' DeleteSessionResponse (Prelude.Maybe Prelude.Text)
deleteSessionResponse_botAlias = Lens.lens (\DeleteSessionResponse' {botAlias} -> botAlias) (\s@DeleteSessionResponse' {} a -> s {botAlias = a} :: DeleteSessionResponse)

-- | The name of the bot associated with the session data.
deleteSessionResponse_botName :: Lens.Lens' DeleteSessionResponse (Prelude.Maybe Prelude.Text)
deleteSessionResponse_botName = Lens.lens (\DeleteSessionResponse' {botName} -> botName) (\s@DeleteSessionResponse' {} a -> s {botName = a} :: DeleteSessionResponse)

-- | The unique identifier for the session.
deleteSessionResponse_sessionId :: Lens.Lens' DeleteSessionResponse (Prelude.Maybe Prelude.Text)
deleteSessionResponse_sessionId = Lens.lens (\DeleteSessionResponse' {sessionId} -> sessionId) (\s@DeleteSessionResponse' {} a -> s {sessionId = a} :: DeleteSessionResponse)

-- | The ID of the client application user.
deleteSessionResponse_userId :: Lens.Lens' DeleteSessionResponse (Prelude.Maybe Prelude.Text)
deleteSessionResponse_userId = Lens.lens (\DeleteSessionResponse' {userId} -> userId) (\s@DeleteSessionResponse' {} a -> s {userId = a} :: DeleteSessionResponse)

-- | The response's http status code.
deleteSessionResponse_httpStatus :: Lens.Lens' DeleteSessionResponse Prelude.Int
deleteSessionResponse_httpStatus = Lens.lens (\DeleteSessionResponse' {httpStatus} -> httpStatus) (\s@DeleteSessionResponse' {} a -> s {httpStatus = a} :: DeleteSessionResponse)

instance Prelude.NFData DeleteSessionResponse where
  rnf DeleteSessionResponse' {..} =
    Prelude.rnf botAlias
      `Prelude.seq` Prelude.rnf botName
      `Prelude.seq` Prelude.rnf sessionId
      `Prelude.seq` Prelude.rnf userId
      `Prelude.seq` Prelude.rnf httpStatus
