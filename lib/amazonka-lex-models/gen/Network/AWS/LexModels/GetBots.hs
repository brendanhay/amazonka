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
-- Module      : Network.AWS.LexModels.GetBots
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns bot information as follows:
--
--
--     * If you provide the @nameContains@ field, the response includes information for the @> LATEST@ version of all bots whose name contains the specified string.
--
--     * If you don't specify the @nameContains@ field, the operation returns information about the @> LATEST@ version of all of your bots.
--
--
--
-- This operation requires permission for the @lex:GetBots@ action.
--
--
-- This operation returns paginated results.
module Network.AWS.LexModels.GetBots
    (
    -- * Creating a Request
      getBots
    , GetBots
    -- * Request Lenses
    , gbNameContains
    , gbNextToken
    , gbMaxResults

    -- * Destructuring the Response
    , getBotsResponse
    , GetBotsResponse
    -- * Response Lenses
    , gbsrsBots
    , gbsrsNextToken
    , gbsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.LexModels.Types
import Network.AWS.LexModels.Types.Product
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getBots' smart constructor.
data GetBots = GetBots'
  { _gbNameContains :: !(Maybe Text)
  , _gbNextToken    :: !(Maybe Text)
  , _gbMaxResults   :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBots' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbNameContains' - Substring to match in bot names. A bot will be returned if any part of its name matches the substring. For example, "xyz" matches both "xyzabc" and "abcxyz."
--
-- * 'gbNextToken' - A pagination token that fetches the next page of bots. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of bots, specify the pagination token in the next request.
--
-- * 'gbMaxResults' - The maximum number of bots to return in the response that the request will return. The default is 10.
getBots
    :: GetBots
getBots =
  GetBots'
    {_gbNameContains = Nothing, _gbNextToken = Nothing, _gbMaxResults = Nothing}


-- | Substring to match in bot names. A bot will be returned if any part of its name matches the substring. For example, "xyz" matches both "xyzabc" and "abcxyz."
gbNameContains :: Lens' GetBots (Maybe Text)
gbNameContains = lens _gbNameContains (\ s a -> s{_gbNameContains = a})

-- | A pagination token that fetches the next page of bots. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of bots, specify the pagination token in the next request.
gbNextToken :: Lens' GetBots (Maybe Text)
gbNextToken = lens _gbNextToken (\ s a -> s{_gbNextToken = a})

-- | The maximum number of bots to return in the response that the request will return. The default is 10.
gbMaxResults :: Lens' GetBots (Maybe Natural)
gbMaxResults = lens _gbMaxResults (\ s a -> s{_gbMaxResults = a}) . mapping _Nat

instance AWSPager GetBots where
        page rq rs
          | stop (rs ^. gbsrsNextToken) = Nothing
          | stop (rs ^. gbsrsBots) = Nothing
          | otherwise =
            Just $ rq & gbNextToken .~ rs ^. gbsrsNextToken

instance AWSRequest GetBots where
        type Rs GetBots = GetBotsResponse
        request = get lexModels
        response
          = receiveJSON
              (\ s h x ->
                 GetBotsResponse' <$>
                   (x .?> "bots" .!@ mempty) <*> (x .?> "nextToken") <*>
                     (pure (fromEnum s)))

instance Hashable GetBots where

instance NFData GetBots where

instance ToHeaders GetBots where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetBots where
        toPath = const "/bots/"

instance ToQuery GetBots where
        toQuery GetBots'{..}
          = mconcat
              ["nameContains" =: _gbNameContains,
               "nextToken" =: _gbNextToken,
               "maxResults" =: _gbMaxResults]

-- | /See:/ 'getBotsResponse' smart constructor.
data GetBotsResponse = GetBotsResponse'
  { _gbsrsBots           :: !(Maybe [BotMetadata])
  , _gbsrsNextToken      :: !(Maybe Text)
  , _gbsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBotsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbsrsBots' - An array of @botMetadata@ objects, with one entry for each bot.
--
-- * 'gbsrsNextToken' - If the response is truncated, it includes a pagination token that you can specify in your next request to fetch the next page of bots.
--
-- * 'gbsrsResponseStatus' - -- | The response status code.
getBotsResponse
    :: Int -- ^ 'gbsrsResponseStatus'
    -> GetBotsResponse
getBotsResponse pResponseStatus_ =
  GetBotsResponse'
    { _gbsrsBots = Nothing
    , _gbsrsNextToken = Nothing
    , _gbsrsResponseStatus = pResponseStatus_
    }


-- | An array of @botMetadata@ objects, with one entry for each bot.
gbsrsBots :: Lens' GetBotsResponse [BotMetadata]
gbsrsBots = lens _gbsrsBots (\ s a -> s{_gbsrsBots = a}) . _Default . _Coerce

-- | If the response is truncated, it includes a pagination token that you can specify in your next request to fetch the next page of bots.
gbsrsNextToken :: Lens' GetBotsResponse (Maybe Text)
gbsrsNextToken = lens _gbsrsNextToken (\ s a -> s{_gbsrsNextToken = a})

-- | -- | The response status code.
gbsrsResponseStatus :: Lens' GetBotsResponse Int
gbsrsResponseStatus = lens _gbsrsResponseStatus (\ s a -> s{_gbsrsResponseStatus = a})

instance NFData GetBotsResponse where
