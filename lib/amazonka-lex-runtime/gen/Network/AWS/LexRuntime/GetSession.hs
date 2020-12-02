{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexRuntime.GetSession
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns session information for a specified bot, alias, and user ID.
module Network.AWS.LexRuntime.GetSession
  ( -- * Creating a Request
    getSession,
    GetSession,

    -- * Request Lenses
    gsCheckpointLabelFilter,
    gsBotName,
    gsBotAlias,
    gsUserId,

    -- * Destructuring the Response
    getSessionResponse,
    GetSessionResponse,

    -- * Response Lenses
    gsrsActiveContexts,
    gsrsSessionId,
    gsrsRecentIntentSummaryView,
    gsrsDialogAction,
    gsrsSessionAttributes,
    gsrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.LexRuntime.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getSession' smart constructor.
data GetSession = GetSession'
  { _gsCheckpointLabelFilter ::
      !(Maybe Text),
    _gsBotName :: !Text,
    _gsBotAlias :: !Text,
    _gsUserId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetSession' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsCheckpointLabelFilter' - A string used to filter the intents returned in the @recentIntentSummaryView@ structure.  When you specify a filter, only intents with their @checkpointLabel@ field set to that string are returned.
--
-- * 'gsBotName' - The name of the bot that contains the session data.
--
-- * 'gsBotAlias' - The alias in use for the bot that contains the session data.
--
-- * 'gsUserId' - The ID of the client application user. Amazon Lex uses this to identify a user's conversation with your bot.
getSession ::
  -- | 'gsBotName'
  Text ->
  -- | 'gsBotAlias'
  Text ->
  -- | 'gsUserId'
  Text ->
  GetSession
getSession pBotName_ pBotAlias_ pUserId_ =
  GetSession'
    { _gsCheckpointLabelFilter = Nothing,
      _gsBotName = pBotName_,
      _gsBotAlias = pBotAlias_,
      _gsUserId = pUserId_
    }

-- | A string used to filter the intents returned in the @recentIntentSummaryView@ structure.  When you specify a filter, only intents with their @checkpointLabel@ field set to that string are returned.
gsCheckpointLabelFilter :: Lens' GetSession (Maybe Text)
gsCheckpointLabelFilter = lens _gsCheckpointLabelFilter (\s a -> s {_gsCheckpointLabelFilter = a})

-- | The name of the bot that contains the session data.
gsBotName :: Lens' GetSession Text
gsBotName = lens _gsBotName (\s a -> s {_gsBotName = a})

-- | The alias in use for the bot that contains the session data.
gsBotAlias :: Lens' GetSession Text
gsBotAlias = lens _gsBotAlias (\s a -> s {_gsBotAlias = a})

-- | The ID of the client application user. Amazon Lex uses this to identify a user's conversation with your bot.
gsUserId :: Lens' GetSession Text
gsUserId = lens _gsUserId (\s a -> s {_gsUserId = a})

instance AWSRequest GetSession where
  type Rs GetSession = GetSessionResponse
  request = get lexRuntime
  response =
    receiveJSON
      ( \s h x ->
          GetSessionResponse'
            <$> (x .?> "activeContexts" .!@ mempty)
            <*> (x .?> "sessionId")
            <*> (x .?> "recentIntentSummaryView" .!@ mempty)
            <*> (x .?> "dialogAction")
            <*> (x .?> "sessionAttributes" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable GetSession

instance NFData GetSession

instance ToHeaders GetSession where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath GetSession where
  toPath GetSession' {..} =
    mconcat
      [ "/bot/",
        toBS _gsBotName,
        "/alias/",
        toBS _gsBotAlias,
        "/user/",
        toBS _gsUserId,
        "/session/"
      ]

instance ToQuery GetSession where
  toQuery GetSession' {..} =
    mconcat ["checkpointLabelFilter" =: _gsCheckpointLabelFilter]

-- | /See:/ 'getSessionResponse' smart constructor.
data GetSessionResponse = GetSessionResponse'
  { _gsrsActiveContexts ::
      !(Maybe (Sensitive [ActiveContext])),
    _gsrsSessionId :: !(Maybe Text),
    _gsrsRecentIntentSummaryView ::
      !(Maybe [IntentSummary]),
    _gsrsDialogAction :: !(Maybe DialogAction),
    _gsrsSessionAttributes ::
      !(Maybe (Sensitive (Map Text (Text)))),
    _gsrsResponseStatus :: !Int
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetSessionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsrsActiveContexts' - A list of active contexts for the session. A context can be set when an intent is fulfilled or by calling the @PostContent@ , @PostText@ , or @PutSession@ operation. You can use a context to control the intents that can follow up an intent, or to modify the operation of your application.
--
-- * 'gsrsSessionId' - A unique identifier for the session.
--
-- * 'gsrsRecentIntentSummaryView' - An array of information about the intents used in the session. The array can contain a maximum of three summaries. If more than three intents are used in the session, the @recentIntentSummaryView@ operation contains information about the last three intents used. If you set the @checkpointLabelFilter@ parameter in the request, the array contains only the intents with the specified label.
--
-- * 'gsrsDialogAction' - Describes the current state of the bot.
--
-- * 'gsrsSessionAttributes' - Map of key/value pairs representing the session-specific context information. It contains application information passed between Amazon Lex and a client application.
--
-- * 'gsrsResponseStatus' - -- | The response status code.
getSessionResponse ::
  -- | 'gsrsResponseStatus'
  Int ->
  GetSessionResponse
getSessionResponse pResponseStatus_ =
  GetSessionResponse'
    { _gsrsActiveContexts = Nothing,
      _gsrsSessionId = Nothing,
      _gsrsRecentIntentSummaryView = Nothing,
      _gsrsDialogAction = Nothing,
      _gsrsSessionAttributes = Nothing,
      _gsrsResponseStatus = pResponseStatus_
    }

-- | A list of active contexts for the session. A context can be set when an intent is fulfilled or by calling the @PostContent@ , @PostText@ , or @PutSession@ operation. You can use a context to control the intents that can follow up an intent, or to modify the operation of your application.
gsrsActiveContexts :: Lens' GetSessionResponse (Maybe [ActiveContext])
gsrsActiveContexts = lens _gsrsActiveContexts (\s a -> s {_gsrsActiveContexts = a}) . mapping (_Sensitive . _Coerce)

-- | A unique identifier for the session.
gsrsSessionId :: Lens' GetSessionResponse (Maybe Text)
gsrsSessionId = lens _gsrsSessionId (\s a -> s {_gsrsSessionId = a})

-- | An array of information about the intents used in the session. The array can contain a maximum of three summaries. If more than three intents are used in the session, the @recentIntentSummaryView@ operation contains information about the last three intents used. If you set the @checkpointLabelFilter@ parameter in the request, the array contains only the intents with the specified label.
gsrsRecentIntentSummaryView :: Lens' GetSessionResponse [IntentSummary]
gsrsRecentIntentSummaryView = lens _gsrsRecentIntentSummaryView (\s a -> s {_gsrsRecentIntentSummaryView = a}) . _Default . _Coerce

-- | Describes the current state of the bot.
gsrsDialogAction :: Lens' GetSessionResponse (Maybe DialogAction)
gsrsDialogAction = lens _gsrsDialogAction (\s a -> s {_gsrsDialogAction = a})

-- | Map of key/value pairs representing the session-specific context information. It contains application information passed between Amazon Lex and a client application.
gsrsSessionAttributes :: Lens' GetSessionResponse (Maybe (HashMap Text (Text)))
gsrsSessionAttributes = lens _gsrsSessionAttributes (\s a -> s {_gsrsSessionAttributes = a}) . mapping (_Sensitive . _Map)

-- | -- | The response status code.
gsrsResponseStatus :: Lens' GetSessionResponse Int
gsrsResponseStatus = lens _gsrsResponseStatus (\s a -> s {_gsrsResponseStatus = a})

instance NFData GetSessionResponse
