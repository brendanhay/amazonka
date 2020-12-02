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
-- Module      : Network.AWS.LexRuntime.DeleteSession
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes session information for a specified bot, alias, and user ID.
module Network.AWS.LexRuntime.DeleteSession
  ( -- * Creating a Request
    deleteSession,
    DeleteSession,

    -- * Request Lenses
    dsBotName,
    dsBotAlias,
    dsUserId,

    -- * Destructuring the Response
    deleteSessionResponse,
    DeleteSessionResponse,

    -- * Response Lenses
    dsrsBotAlias,
    dsrsBotName,
    dsrsUserId,
    dsrsSessionId,
    dsrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.LexRuntime.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteSession' smart constructor.
data DeleteSession = DeleteSession'
  { _dsBotName :: !Text,
    _dsBotAlias :: !Text,
    _dsUserId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteSession' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsBotName' - The name of the bot that contains the session data.
--
-- * 'dsBotAlias' - The alias in use for the bot that contains the session data.
--
-- * 'dsUserId' - The identifier of the user associated with the session data.
deleteSession ::
  -- | 'dsBotName'
  Text ->
  -- | 'dsBotAlias'
  Text ->
  -- | 'dsUserId'
  Text ->
  DeleteSession
deleteSession pBotName_ pBotAlias_ pUserId_ =
  DeleteSession'
    { _dsBotName = pBotName_,
      _dsBotAlias = pBotAlias_,
      _dsUserId = pUserId_
    }

-- | The name of the bot that contains the session data.
dsBotName :: Lens' DeleteSession Text
dsBotName = lens _dsBotName (\s a -> s {_dsBotName = a})

-- | The alias in use for the bot that contains the session data.
dsBotAlias :: Lens' DeleteSession Text
dsBotAlias = lens _dsBotAlias (\s a -> s {_dsBotAlias = a})

-- | The identifier of the user associated with the session data.
dsUserId :: Lens' DeleteSession Text
dsUserId = lens _dsUserId (\s a -> s {_dsUserId = a})

instance AWSRequest DeleteSession where
  type Rs DeleteSession = DeleteSessionResponse
  request = delete lexRuntime
  response =
    receiveJSON
      ( \s h x ->
          DeleteSessionResponse'
            <$> (x .?> "botAlias")
            <*> (x .?> "botName")
            <*> (x .?> "userId")
            <*> (x .?> "sessionId")
            <*> (pure (fromEnum s))
      )

instance Hashable DeleteSession

instance NFData DeleteSession

instance ToHeaders DeleteSession where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath DeleteSession where
  toPath DeleteSession' {..} =
    mconcat
      [ "/bot/",
        toBS _dsBotName,
        "/alias/",
        toBS _dsBotAlias,
        "/user/",
        toBS _dsUserId,
        "/session"
      ]

instance ToQuery DeleteSession where
  toQuery = const mempty

-- | /See:/ 'deleteSessionResponse' smart constructor.
data DeleteSessionResponse = DeleteSessionResponse'
  { _dsrsBotAlias ::
      !(Maybe Text),
    _dsrsBotName :: !(Maybe Text),
    _dsrsUserId :: !(Maybe Text),
    _dsrsSessionId :: !(Maybe Text),
    _dsrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteSessionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsrsBotAlias' - The alias in use for the bot associated with the session data.
--
-- * 'dsrsBotName' - The name of the bot associated with the session data.
--
-- * 'dsrsUserId' - The ID of the client application user.
--
-- * 'dsrsSessionId' - The unique identifier for the session.
--
-- * 'dsrsResponseStatus' - -- | The response status code.
deleteSessionResponse ::
  -- | 'dsrsResponseStatus'
  Int ->
  DeleteSessionResponse
deleteSessionResponse pResponseStatus_ =
  DeleteSessionResponse'
    { _dsrsBotAlias = Nothing,
      _dsrsBotName = Nothing,
      _dsrsUserId = Nothing,
      _dsrsSessionId = Nothing,
      _dsrsResponseStatus = pResponseStatus_
    }

-- | The alias in use for the bot associated with the session data.
dsrsBotAlias :: Lens' DeleteSessionResponse (Maybe Text)
dsrsBotAlias = lens _dsrsBotAlias (\s a -> s {_dsrsBotAlias = a})

-- | The name of the bot associated with the session data.
dsrsBotName :: Lens' DeleteSessionResponse (Maybe Text)
dsrsBotName = lens _dsrsBotName (\s a -> s {_dsrsBotName = a})

-- | The ID of the client application user.
dsrsUserId :: Lens' DeleteSessionResponse (Maybe Text)
dsrsUserId = lens _dsrsUserId (\s a -> s {_dsrsUserId = a})

-- | The unique identifier for the session.
dsrsSessionId :: Lens' DeleteSessionResponse (Maybe Text)
dsrsSessionId = lens _dsrsSessionId (\s a -> s {_dsrsSessionId = a})

-- | -- | The response status code.
dsrsResponseStatus :: Lens' DeleteSessionResponse Int
dsrsResponseStatus = lens _dsrsResponseStatus (\s a -> s {_dsrsResponseStatus = a})

instance NFData DeleteSessionResponse
