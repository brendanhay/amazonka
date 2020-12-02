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
-- Module      : Network.AWS.GameLift.CreatePlayerSessions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reserves open slots in a game session for a group of players. Before players can be added, a game session must have an @ACTIVE@ status, have a creation policy of @ALLOW_ALL@ , and have an open player slot. To add a single player to a game session, use 'CreatePlayerSession' . When a player connects to the game server and references a player session ID, the game server contacts the Amazon GameLift service to validate the player reservation and accept the player.
--
--
-- To create player sessions, specify a game session ID, a list of player IDs, and optionally a set of player data strings. If successful, a slot is reserved in the game session for each player and a set of new 'PlayerSession' objects is returned. Player sessions cannot be updated.
--
-- /Available in Amazon GameLift Local./
--
--     * 'CreatePlayerSession'
--
--     * 'CreatePlayerSessions'
--
--     * 'DescribePlayerSessions'
--
--     * Game session placements
--
--     * 'StartGameSessionPlacement'
--
--     * 'DescribeGameSessionPlacement'
--
--     * 'StopGameSessionPlacement'
module Network.AWS.GameLift.CreatePlayerSessions
  ( -- * Creating a Request
    createPlayerSessions,
    CreatePlayerSessions,

    -- * Request Lenses
    cpsPlayerDataMap,
    cpsGameSessionId,
    cpsPlayerIds,

    -- * Destructuring the Response
    createPlayerSessionsResponse,
    CreatePlayerSessionsResponse,

    -- * Response Lenses
    cpssrsPlayerSessions,
    cpssrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input for a request operation.
--
--
--
-- /See:/ 'createPlayerSessions' smart constructor.
data CreatePlayerSessions = CreatePlayerSessions'
  { _cpsPlayerDataMap ::
      !(Maybe (Map Text (Text))),
    _cpsGameSessionId :: !Text,
    _cpsPlayerIds :: !(List1 Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreatePlayerSessions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpsPlayerDataMap' - Map of string pairs, each specifying a player ID and a set of developer-defined information related to the player. Amazon GameLift does not use this data, so it can be formatted as needed for use in the game. Player data strings for player IDs not included in the @PlayerIds@ parameter are ignored.
--
-- * 'cpsGameSessionId' - A unique identifier for the game session to add players to.
--
-- * 'cpsPlayerIds' - List of unique identifiers for the players to be added.
createPlayerSessions ::
  -- | 'cpsGameSessionId'
  Text ->
  -- | 'cpsPlayerIds'
  NonEmpty Text ->
  CreatePlayerSessions
createPlayerSessions pGameSessionId_ pPlayerIds_ =
  CreatePlayerSessions'
    { _cpsPlayerDataMap = Nothing,
      _cpsGameSessionId = pGameSessionId_,
      _cpsPlayerIds = _List1 # pPlayerIds_
    }

-- | Map of string pairs, each specifying a player ID and a set of developer-defined information related to the player. Amazon GameLift does not use this data, so it can be formatted as needed for use in the game. Player data strings for player IDs not included in the @PlayerIds@ parameter are ignored.
cpsPlayerDataMap :: Lens' CreatePlayerSessions (HashMap Text (Text))
cpsPlayerDataMap = lens _cpsPlayerDataMap (\s a -> s {_cpsPlayerDataMap = a}) . _Default . _Map

-- | A unique identifier for the game session to add players to.
cpsGameSessionId :: Lens' CreatePlayerSessions Text
cpsGameSessionId = lens _cpsGameSessionId (\s a -> s {_cpsGameSessionId = a})

-- | List of unique identifiers for the players to be added.
cpsPlayerIds :: Lens' CreatePlayerSessions (NonEmpty Text)
cpsPlayerIds = lens _cpsPlayerIds (\s a -> s {_cpsPlayerIds = a}) . _List1

instance AWSRequest CreatePlayerSessions where
  type Rs CreatePlayerSessions = CreatePlayerSessionsResponse
  request = postJSON gameLift
  response =
    receiveJSON
      ( \s h x ->
          CreatePlayerSessionsResponse'
            <$> (x .?> "PlayerSessions" .!@ mempty) <*> (pure (fromEnum s))
      )

instance Hashable CreatePlayerSessions

instance NFData CreatePlayerSessions

instance ToHeaders CreatePlayerSessions where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("GameLift.CreatePlayerSessions" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreatePlayerSessions where
  toJSON CreatePlayerSessions' {..} =
    object
      ( catMaybes
          [ ("PlayerDataMap" .=) <$> _cpsPlayerDataMap,
            Just ("GameSessionId" .= _cpsGameSessionId),
            Just ("PlayerIds" .= _cpsPlayerIds)
          ]
      )

instance ToPath CreatePlayerSessions where
  toPath = const "/"

instance ToQuery CreatePlayerSessions where
  toQuery = const mempty

-- | Represents the returned data in response to a request operation.
--
--
--
-- /See:/ 'createPlayerSessionsResponse' smart constructor.
data CreatePlayerSessionsResponse = CreatePlayerSessionsResponse'
  { _cpssrsPlayerSessions ::
      !(Maybe [PlayerSession]),
    _cpssrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreatePlayerSessionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpssrsPlayerSessions' - A collection of player session objects created for the added players.
--
-- * 'cpssrsResponseStatus' - -- | The response status code.
createPlayerSessionsResponse ::
  -- | 'cpssrsResponseStatus'
  Int ->
  CreatePlayerSessionsResponse
createPlayerSessionsResponse pResponseStatus_ =
  CreatePlayerSessionsResponse'
    { _cpssrsPlayerSessions = Nothing,
      _cpssrsResponseStatus = pResponseStatus_
    }

-- | A collection of player session objects created for the added players.
cpssrsPlayerSessions :: Lens' CreatePlayerSessionsResponse [PlayerSession]
cpssrsPlayerSessions = lens _cpssrsPlayerSessions (\s a -> s {_cpssrsPlayerSessions = a}) . _Default . _Coerce

-- | -- | The response status code.
cpssrsResponseStatus :: Lens' CreatePlayerSessionsResponse Int
cpssrsResponseStatus = lens _cpssrsResponseStatus (\s a -> s {_cpssrsResponseStatus = a})

instance NFData CreatePlayerSessionsResponse
