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
-- Module      : Amazonka.LexRuntime.GetSession
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns session information for a specified bot, alias, and user ID.
module Amazonka.LexRuntime.GetSession
  ( -- * Creating a Request
    GetSession (..),
    newGetSession,

    -- * Request Lenses
    getSession_checkpointLabelFilter,
    getSession_botName,
    getSession_botAlias,
    getSession_userId,

    -- * Destructuring the Response
    GetSessionResponse (..),
    newGetSessionResponse,

    -- * Response Lenses
    getSessionResponse_dialogAction,
    getSessionResponse_sessionAttributes,
    getSessionResponse_sessionId,
    getSessionResponse_activeContexts,
    getSessionResponse_recentIntentSummaryView,
    getSessionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LexRuntime.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetSession' smart constructor.
data GetSession = GetSession'
  { -- | A string used to filter the intents returned in the
    -- @recentIntentSummaryView@ structure.
    --
    -- When you specify a filter, only intents with their @checkpointLabel@
    -- field set to that string are returned.
    checkpointLabelFilter :: Prelude.Maybe Prelude.Text,
    -- | The name of the bot that contains the session data.
    botName :: Prelude.Text,
    -- | The alias in use for the bot that contains the session data.
    botAlias :: Prelude.Text,
    -- | The ID of the client application user. Amazon Lex uses this to identify
    -- a user\'s conversation with your bot.
    userId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'checkpointLabelFilter', 'getSession_checkpointLabelFilter' - A string used to filter the intents returned in the
-- @recentIntentSummaryView@ structure.
--
-- When you specify a filter, only intents with their @checkpointLabel@
-- field set to that string are returned.
--
-- 'botName', 'getSession_botName' - The name of the bot that contains the session data.
--
-- 'botAlias', 'getSession_botAlias' - The alias in use for the bot that contains the session data.
--
-- 'userId', 'getSession_userId' - The ID of the client application user. Amazon Lex uses this to identify
-- a user\'s conversation with your bot.
newGetSession ::
  -- | 'botName'
  Prelude.Text ->
  -- | 'botAlias'
  Prelude.Text ->
  -- | 'userId'
  Prelude.Text ->
  GetSession
newGetSession pBotName_ pBotAlias_ pUserId_ =
  GetSession'
    { checkpointLabelFilter =
        Prelude.Nothing,
      botName = pBotName_,
      botAlias = pBotAlias_,
      userId = pUserId_
    }

-- | A string used to filter the intents returned in the
-- @recentIntentSummaryView@ structure.
--
-- When you specify a filter, only intents with their @checkpointLabel@
-- field set to that string are returned.
getSession_checkpointLabelFilter :: Lens.Lens' GetSession (Prelude.Maybe Prelude.Text)
getSession_checkpointLabelFilter = Lens.lens (\GetSession' {checkpointLabelFilter} -> checkpointLabelFilter) (\s@GetSession' {} a -> s {checkpointLabelFilter = a} :: GetSession)

-- | The name of the bot that contains the session data.
getSession_botName :: Lens.Lens' GetSession Prelude.Text
getSession_botName = Lens.lens (\GetSession' {botName} -> botName) (\s@GetSession' {} a -> s {botName = a} :: GetSession)

-- | The alias in use for the bot that contains the session data.
getSession_botAlias :: Lens.Lens' GetSession Prelude.Text
getSession_botAlias = Lens.lens (\GetSession' {botAlias} -> botAlias) (\s@GetSession' {} a -> s {botAlias = a} :: GetSession)

-- | The ID of the client application user. Amazon Lex uses this to identify
-- a user\'s conversation with your bot.
getSession_userId :: Lens.Lens' GetSession Prelude.Text
getSession_userId = Lens.lens (\GetSession' {userId} -> userId) (\s@GetSession' {} a -> s {userId = a} :: GetSession)

instance Core.AWSRequest GetSession where
  type AWSResponse GetSession = GetSessionResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSessionResponse'
            Prelude.<$> (x Core..?> "dialogAction")
            Prelude.<*> ( x Core..?> "sessionAttributes"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "sessionId")
            Prelude.<*> (x Core..?> "activeContexts" Core..!@ Prelude.mempty)
            Prelude.<*> ( x Core..?> "recentIntentSummaryView"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSession where
  hashWithSalt _salt GetSession' {..} =
    _salt `Prelude.hashWithSalt` checkpointLabelFilter
      `Prelude.hashWithSalt` botName
      `Prelude.hashWithSalt` botAlias
      `Prelude.hashWithSalt` userId

instance Prelude.NFData GetSession where
  rnf GetSession' {..} =
    Prelude.rnf checkpointLabelFilter
      `Prelude.seq` Prelude.rnf botName
      `Prelude.seq` Prelude.rnf botAlias
      `Prelude.seq` Prelude.rnf userId

instance Core.ToHeaders GetSession where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetSession where
  toPath GetSession' {..} =
    Prelude.mconcat
      [ "/bot/",
        Core.toBS botName,
        "/alias/",
        Core.toBS botAlias,
        "/user/",
        Core.toBS userId,
        "/session/"
      ]

instance Core.ToQuery GetSession where
  toQuery GetSession' {..} =
    Prelude.mconcat
      [ "checkpointLabelFilter"
          Core.=: checkpointLabelFilter
      ]

-- | /See:/ 'newGetSessionResponse' smart constructor.
data GetSessionResponse = GetSessionResponse'
  { -- | Describes the current state of the bot.
    dialogAction :: Prelude.Maybe DialogAction,
    -- | Map of key\/value pairs representing the session-specific context
    -- information. It contains application information passed between Amazon
    -- Lex and a client application.
    sessionAttributes :: Prelude.Maybe (Core.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | A unique identifier for the session.
    sessionId :: Prelude.Maybe Prelude.Text,
    -- | A list of active contexts for the session. A context can be set when an
    -- intent is fulfilled or by calling the @PostContent@, @PostText@, or
    -- @PutSession@ operation.
    --
    -- You can use a context to control the intents that can follow up an
    -- intent, or to modify the operation of your application.
    activeContexts :: Prelude.Maybe (Core.Sensitive [ActiveContext]),
    -- | An array of information about the intents used in the session. The array
    -- can contain a maximum of three summaries. If more than three intents are
    -- used in the session, the @recentIntentSummaryView@ operation contains
    -- information about the last three intents used.
    --
    -- If you set the @checkpointLabelFilter@ parameter in the request, the
    -- array contains only the intents with the specified label.
    recentIntentSummaryView :: Prelude.Maybe [IntentSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSessionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dialogAction', 'getSessionResponse_dialogAction' - Describes the current state of the bot.
--
-- 'sessionAttributes', 'getSessionResponse_sessionAttributes' - Map of key\/value pairs representing the session-specific context
-- information. It contains application information passed between Amazon
-- Lex and a client application.
--
-- 'sessionId', 'getSessionResponse_sessionId' - A unique identifier for the session.
--
-- 'activeContexts', 'getSessionResponse_activeContexts' - A list of active contexts for the session. A context can be set when an
-- intent is fulfilled or by calling the @PostContent@, @PostText@, or
-- @PutSession@ operation.
--
-- You can use a context to control the intents that can follow up an
-- intent, or to modify the operation of your application.
--
-- 'recentIntentSummaryView', 'getSessionResponse_recentIntentSummaryView' - An array of information about the intents used in the session. The array
-- can contain a maximum of three summaries. If more than three intents are
-- used in the session, the @recentIntentSummaryView@ operation contains
-- information about the last three intents used.
--
-- If you set the @checkpointLabelFilter@ parameter in the request, the
-- array contains only the intents with the specified label.
--
-- 'httpStatus', 'getSessionResponse_httpStatus' - The response's http status code.
newGetSessionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSessionResponse
newGetSessionResponse pHttpStatus_ =
  GetSessionResponse'
    { dialogAction = Prelude.Nothing,
      sessionAttributes = Prelude.Nothing,
      sessionId = Prelude.Nothing,
      activeContexts = Prelude.Nothing,
      recentIntentSummaryView = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Describes the current state of the bot.
getSessionResponse_dialogAction :: Lens.Lens' GetSessionResponse (Prelude.Maybe DialogAction)
getSessionResponse_dialogAction = Lens.lens (\GetSessionResponse' {dialogAction} -> dialogAction) (\s@GetSessionResponse' {} a -> s {dialogAction = a} :: GetSessionResponse)

-- | Map of key\/value pairs representing the session-specific context
-- information. It contains application information passed between Amazon
-- Lex and a client application.
getSessionResponse_sessionAttributes :: Lens.Lens' GetSessionResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getSessionResponse_sessionAttributes = Lens.lens (\GetSessionResponse' {sessionAttributes} -> sessionAttributes) (\s@GetSessionResponse' {} a -> s {sessionAttributes = a} :: GetSessionResponse) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Lens.coerced)

-- | A unique identifier for the session.
getSessionResponse_sessionId :: Lens.Lens' GetSessionResponse (Prelude.Maybe Prelude.Text)
getSessionResponse_sessionId = Lens.lens (\GetSessionResponse' {sessionId} -> sessionId) (\s@GetSessionResponse' {} a -> s {sessionId = a} :: GetSessionResponse)

-- | A list of active contexts for the session. A context can be set when an
-- intent is fulfilled or by calling the @PostContent@, @PostText@, or
-- @PutSession@ operation.
--
-- You can use a context to control the intents that can follow up an
-- intent, or to modify the operation of your application.
getSessionResponse_activeContexts :: Lens.Lens' GetSessionResponse (Prelude.Maybe [ActiveContext])
getSessionResponse_activeContexts = Lens.lens (\GetSessionResponse' {activeContexts} -> activeContexts) (\s@GetSessionResponse' {} a -> s {activeContexts = a} :: GetSessionResponse) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Lens.coerced)

-- | An array of information about the intents used in the session. The array
-- can contain a maximum of three summaries. If more than three intents are
-- used in the session, the @recentIntentSummaryView@ operation contains
-- information about the last three intents used.
--
-- If you set the @checkpointLabelFilter@ parameter in the request, the
-- array contains only the intents with the specified label.
getSessionResponse_recentIntentSummaryView :: Lens.Lens' GetSessionResponse (Prelude.Maybe [IntentSummary])
getSessionResponse_recentIntentSummaryView = Lens.lens (\GetSessionResponse' {recentIntentSummaryView} -> recentIntentSummaryView) (\s@GetSessionResponse' {} a -> s {recentIntentSummaryView = a} :: GetSessionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getSessionResponse_httpStatus :: Lens.Lens' GetSessionResponse Prelude.Int
getSessionResponse_httpStatus = Lens.lens (\GetSessionResponse' {httpStatus} -> httpStatus) (\s@GetSessionResponse' {} a -> s {httpStatus = a} :: GetSessionResponse)

instance Prelude.NFData GetSessionResponse where
  rnf GetSessionResponse' {..} =
    Prelude.rnf dialogAction
      `Prelude.seq` Prelude.rnf sessionAttributes
      `Prelude.seq` Prelude.rnf sessionId
      `Prelude.seq` Prelude.rnf activeContexts
      `Prelude.seq` Prelude.rnf recentIntentSummaryView
      `Prelude.seq` Prelude.rnf httpStatus
