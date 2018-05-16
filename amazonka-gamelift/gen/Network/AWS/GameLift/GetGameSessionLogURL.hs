{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.GetGameSessionLogURL
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the location of stored game session logs for a specified game session. When a game session is terminated, Amazon GameLift automatically stores the logs in Amazon S3 and retains them for 14 days. Use this URL to download the logs.
--
--
-- Game-session-related operations include:
--
--     * 'CreateGameSession'
--
--     * 'DescribeGameSessions'
--
--     * 'DescribeGameSessionDetails'
--
--     * 'SearchGameSessions'
--
--     * 'UpdateGameSession'
--
--     * 'GetGameSessionLogUrl'
--
--     * Game session placements
--
--     * 'StartGameSessionPlacement'
--
--     * 'DescribeGameSessionPlacement'
--
--     * 'StopGameSessionPlacement'
--
--
--
--
--
module Network.AWS.GameLift.GetGameSessionLogURL
    (
    -- * Creating a Request
      getGameSessionLogURL
    , GetGameSessionLogURL
    -- * Request Lenses
    , ggsluGameSessionId

    -- * Destructuring the Response
    , getGameSessionLogURLResponse
    , GetGameSessionLogURLResponse
    -- * Response Lenses
    , ggslursPreSignedURL
    , ggslursResponseStatus
    ) where

import Network.AWS.GameLift.Types
import Network.AWS.GameLift.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input for a request action.
--
--
--
-- /See:/ 'getGameSessionLogURL' smart constructor.
newtype GetGameSessionLogURL = GetGameSessionLogURL'
  { _ggsluGameSessionId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetGameSessionLogURL' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ggsluGameSessionId' - Unique identifier for the game session to get logs for.
getGameSessionLogURL
    :: Text -- ^ 'ggsluGameSessionId'
    -> GetGameSessionLogURL
getGameSessionLogURL pGameSessionId_ =
  GetGameSessionLogURL' {_ggsluGameSessionId = pGameSessionId_}


-- | Unique identifier for the game session to get logs for.
ggsluGameSessionId :: Lens' GetGameSessionLogURL Text
ggsluGameSessionId = lens _ggsluGameSessionId (\ s a -> s{_ggsluGameSessionId = a})

instance AWSRequest GetGameSessionLogURL where
        type Rs GetGameSessionLogURL =
             GetGameSessionLogURLResponse
        request = postJSON gameLift
        response
          = receiveJSON
              (\ s h x ->
                 GetGameSessionLogURLResponse' <$>
                   (x .?> "PreSignedUrl") <*> (pure (fromEnum s)))

instance Hashable GetGameSessionLogURL where

instance NFData GetGameSessionLogURL where

instance ToHeaders GetGameSessionLogURL where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("GameLift.GetGameSessionLogUrl" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetGameSessionLogURL where
        toJSON GetGameSessionLogURL'{..}
          = object
              (catMaybes
                 [Just ("GameSessionId" .= _ggsluGameSessionId)])

instance ToPath GetGameSessionLogURL where
        toPath = const "/"

instance ToQuery GetGameSessionLogURL where
        toQuery = const mempty

-- | Represents the returned data in response to a request action.
--
--
--
-- /See:/ 'getGameSessionLogURLResponse' smart constructor.
data GetGameSessionLogURLResponse = GetGameSessionLogURLResponse'
  { _ggslursPreSignedURL   :: !(Maybe Text)
  , _ggslursResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetGameSessionLogURLResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ggslursPreSignedURL' - Location of the requested game session logs, available for download.
--
-- * 'ggslursResponseStatus' - -- | The response status code.
getGameSessionLogURLResponse
    :: Int -- ^ 'ggslursResponseStatus'
    -> GetGameSessionLogURLResponse
getGameSessionLogURLResponse pResponseStatus_ =
  GetGameSessionLogURLResponse'
    {_ggslursPreSignedURL = Nothing, _ggslursResponseStatus = pResponseStatus_}


-- | Location of the requested game session logs, available for download.
ggslursPreSignedURL :: Lens' GetGameSessionLogURLResponse (Maybe Text)
ggslursPreSignedURL = lens _ggslursPreSignedURL (\ s a -> s{_ggslursPreSignedURL = a})

-- | -- | The response status code.
ggslursResponseStatus :: Lens' GetGameSessionLogURLResponse Int
ggslursResponseStatus = lens _ggslursResponseStatus (\ s a -> s{_ggslursResponseStatus = a})

instance NFData GetGameSessionLogURLResponse where
