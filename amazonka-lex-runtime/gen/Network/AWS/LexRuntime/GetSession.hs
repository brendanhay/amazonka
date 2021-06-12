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
-- Module      : Network.AWS.LexRuntime.GetSession
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns session information for a specified bot, alias, and user ID.
module Network.AWS.LexRuntime.GetSession
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
    getSessionResponse_sessionAttributes,
    getSessionResponse_dialogAction,
    getSessionResponse_sessionId,
    getSessionResponse_recentIntentSummaryView,
    getSessionResponse_activeContexts,
    getSessionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexRuntime.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetSession' smart constructor.
data GetSession = GetSession'
  { -- | A string used to filter the intents returned in the
    -- @recentIntentSummaryView@ structure.
    --
    -- When you specify a filter, only intents with their @checkpointLabel@
    -- field set to that string are returned.
    checkpointLabelFilter :: Core.Maybe Core.Text,
    -- | The name of the bot that contains the session data.
    botName :: Core.Text,
    -- | The alias in use for the bot that contains the session data.
    botAlias :: Core.Text,
    -- | The ID of the client application user. Amazon Lex uses this to identify
    -- a user\'s conversation with your bot.
    userId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'botAlias'
  Core.Text ->
  -- | 'userId'
  Core.Text ->
  GetSession
newGetSession pBotName_ pBotAlias_ pUserId_ =
  GetSession'
    { checkpointLabelFilter = Core.Nothing,
      botName = pBotName_,
      botAlias = pBotAlias_,
      userId = pUserId_
    }

-- | A string used to filter the intents returned in the
-- @recentIntentSummaryView@ structure.
--
-- When you specify a filter, only intents with their @checkpointLabel@
-- field set to that string are returned.
getSession_checkpointLabelFilter :: Lens.Lens' GetSession (Core.Maybe Core.Text)
getSession_checkpointLabelFilter = Lens.lens (\GetSession' {checkpointLabelFilter} -> checkpointLabelFilter) (\s@GetSession' {} a -> s {checkpointLabelFilter = a} :: GetSession)

-- | The name of the bot that contains the session data.
getSession_botName :: Lens.Lens' GetSession Core.Text
getSession_botName = Lens.lens (\GetSession' {botName} -> botName) (\s@GetSession' {} a -> s {botName = a} :: GetSession)

-- | The alias in use for the bot that contains the session data.
getSession_botAlias :: Lens.Lens' GetSession Core.Text
getSession_botAlias = Lens.lens (\GetSession' {botAlias} -> botAlias) (\s@GetSession' {} a -> s {botAlias = a} :: GetSession)

-- | The ID of the client application user. Amazon Lex uses this to identify
-- a user\'s conversation with your bot.
getSession_userId :: Lens.Lens' GetSession Core.Text
getSession_userId = Lens.lens (\GetSession' {userId} -> userId) (\s@GetSession' {} a -> s {userId = a} :: GetSession)

instance Core.AWSRequest GetSession where
  type AWSResponse GetSession = GetSessionResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSessionResponse'
            Core.<$> (x Core..?> "sessionAttributes" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "dialogAction")
            Core.<*> (x Core..?> "sessionId")
            Core.<*> ( x Core..?> "recentIntentSummaryView"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "activeContexts" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetSession

instance Core.NFData GetSession

instance Core.ToHeaders GetSession where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetSession where
  toPath GetSession' {..} =
    Core.mconcat
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
    Core.mconcat
      [ "checkpointLabelFilter"
          Core.=: checkpointLabelFilter
      ]

-- | /See:/ 'newGetSessionResponse' smart constructor.
data GetSessionResponse = GetSessionResponse'
  { -- | Map of key\/value pairs representing the session-specific context
    -- information. It contains application information passed between Amazon
    -- Lex and a client application.
    sessionAttributes :: Core.Maybe (Core.Sensitive (Core.HashMap Core.Text Core.Text)),
    -- | Describes the current state of the bot.
    dialogAction :: Core.Maybe DialogAction,
    -- | A unique identifier for the session.
    sessionId :: Core.Maybe Core.Text,
    -- | An array of information about the intents used in the session. The array
    -- can contain a maximum of three summaries. If more than three intents are
    -- used in the session, the @recentIntentSummaryView@ operation contains
    -- information about the last three intents used.
    --
    -- If you set the @checkpointLabelFilter@ parameter in the request, the
    -- array contains only the intents with the specified label.
    recentIntentSummaryView :: Core.Maybe [IntentSummary],
    -- | A list of active contexts for the session. A context can be set when an
    -- intent is fulfilled or by calling the @PostContent@, @PostText@, or
    -- @PutSession@ operation.
    --
    -- You can use a context to control the intents that can follow up an
    -- intent, or to modify the operation of your application.
    activeContexts :: Core.Maybe (Core.Sensitive [ActiveContext]),
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetSessionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sessionAttributes', 'getSessionResponse_sessionAttributes' - Map of key\/value pairs representing the session-specific context
-- information. It contains application information passed between Amazon
-- Lex and a client application.
--
-- 'dialogAction', 'getSessionResponse_dialogAction' - Describes the current state of the bot.
--
-- 'sessionId', 'getSessionResponse_sessionId' - A unique identifier for the session.
--
-- 'recentIntentSummaryView', 'getSessionResponse_recentIntentSummaryView' - An array of information about the intents used in the session. The array
-- can contain a maximum of three summaries. If more than three intents are
-- used in the session, the @recentIntentSummaryView@ operation contains
-- information about the last three intents used.
--
-- If you set the @checkpointLabelFilter@ parameter in the request, the
-- array contains only the intents with the specified label.
--
-- 'activeContexts', 'getSessionResponse_activeContexts' - A list of active contexts for the session. A context can be set when an
-- intent is fulfilled or by calling the @PostContent@, @PostText@, or
-- @PutSession@ operation.
--
-- You can use a context to control the intents that can follow up an
-- intent, or to modify the operation of your application.
--
-- 'httpStatus', 'getSessionResponse_httpStatus' - The response's http status code.
newGetSessionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetSessionResponse
newGetSessionResponse pHttpStatus_ =
  GetSessionResponse'
    { sessionAttributes =
        Core.Nothing,
      dialogAction = Core.Nothing,
      sessionId = Core.Nothing,
      recentIntentSummaryView = Core.Nothing,
      activeContexts = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Map of key\/value pairs representing the session-specific context
-- information. It contains application information passed between Amazon
-- Lex and a client application.
getSessionResponse_sessionAttributes :: Lens.Lens' GetSessionResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
getSessionResponse_sessionAttributes = Lens.lens (\GetSessionResponse' {sessionAttributes} -> sessionAttributes) (\s@GetSessionResponse' {} a -> s {sessionAttributes = a} :: GetSessionResponse) Core.. Lens.mapping (Core._Sensitive Core.. Lens._Coerce)

-- | Describes the current state of the bot.
getSessionResponse_dialogAction :: Lens.Lens' GetSessionResponse (Core.Maybe DialogAction)
getSessionResponse_dialogAction = Lens.lens (\GetSessionResponse' {dialogAction} -> dialogAction) (\s@GetSessionResponse' {} a -> s {dialogAction = a} :: GetSessionResponse)

-- | A unique identifier for the session.
getSessionResponse_sessionId :: Lens.Lens' GetSessionResponse (Core.Maybe Core.Text)
getSessionResponse_sessionId = Lens.lens (\GetSessionResponse' {sessionId} -> sessionId) (\s@GetSessionResponse' {} a -> s {sessionId = a} :: GetSessionResponse)

-- | An array of information about the intents used in the session. The array
-- can contain a maximum of three summaries. If more than three intents are
-- used in the session, the @recentIntentSummaryView@ operation contains
-- information about the last three intents used.
--
-- If you set the @checkpointLabelFilter@ parameter in the request, the
-- array contains only the intents with the specified label.
getSessionResponse_recentIntentSummaryView :: Lens.Lens' GetSessionResponse (Core.Maybe [IntentSummary])
getSessionResponse_recentIntentSummaryView = Lens.lens (\GetSessionResponse' {recentIntentSummaryView} -> recentIntentSummaryView) (\s@GetSessionResponse' {} a -> s {recentIntentSummaryView = a} :: GetSessionResponse) Core.. Lens.mapping Lens._Coerce

-- | A list of active contexts for the session. A context can be set when an
-- intent is fulfilled or by calling the @PostContent@, @PostText@, or
-- @PutSession@ operation.
--
-- You can use a context to control the intents that can follow up an
-- intent, or to modify the operation of your application.
getSessionResponse_activeContexts :: Lens.Lens' GetSessionResponse (Core.Maybe [ActiveContext])
getSessionResponse_activeContexts = Lens.lens (\GetSessionResponse' {activeContexts} -> activeContexts) (\s@GetSessionResponse' {} a -> s {activeContexts = a} :: GetSessionResponse) Core.. Lens.mapping (Core._Sensitive Core.. Lens._Coerce)

-- | The response's http status code.
getSessionResponse_httpStatus :: Lens.Lens' GetSessionResponse Core.Int
getSessionResponse_httpStatus = Lens.lens (\GetSessionResponse' {httpStatus} -> httpStatus) (\s@GetSessionResponse' {} a -> s {httpStatus = a} :: GetSessionResponse)

instance Core.NFData GetSessionResponse
