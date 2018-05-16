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
-- Module      : Network.AWS.LexModels.GetBotVersions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about all of the versions of a bot.
--
--
-- The @GetBotVersions@ operation returns a @BotMetadata@ object for each version of a bot. For example, if a bot has three numbered versions, the @GetBotVersions@ operation returns four @BotMetadata@ objects in the response, one for each numbered version and one for the @> LATEST@ version.
--
-- The @GetBotVersions@ operation always returns at least one version, the @> LATEST@ version.
--
-- This operation requires permissions for the @lex:GetBotVersions@ action.
--
--
-- This operation returns paginated results.
module Network.AWS.LexModels.GetBotVersions
    (
    -- * Creating a Request
      getBotVersions
    , GetBotVersions
    -- * Request Lenses
    , gbvNextToken
    , gbvMaxResults
    , gbvName

    -- * Destructuring the Response
    , getBotVersionsResponse
    , GetBotVersionsResponse
    -- * Response Lenses
    , gbvrsBots
    , gbvrsNextToken
    , gbvrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.LexModels.Types
import Network.AWS.LexModels.Types.Product
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getBotVersions' smart constructor.
data GetBotVersions = GetBotVersions'
  { _gbvNextToken  :: !(Maybe Text)
  , _gbvMaxResults :: !(Maybe Nat)
  , _gbvName       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBotVersions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbvNextToken' - A pagination token for fetching the next page of bot versions. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of versions, specify the pagination token in the next request.
--
-- * 'gbvMaxResults' - The maximum number of bot versions to return in the response. The default is 10.
--
-- * 'gbvName' - The name of the bot for which versions should be returned.
getBotVersions
    :: Text -- ^ 'gbvName'
    -> GetBotVersions
getBotVersions pName_ =
  GetBotVersions'
    {_gbvNextToken = Nothing, _gbvMaxResults = Nothing, _gbvName = pName_}


-- | A pagination token for fetching the next page of bot versions. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of versions, specify the pagination token in the next request.
gbvNextToken :: Lens' GetBotVersions (Maybe Text)
gbvNextToken = lens _gbvNextToken (\ s a -> s{_gbvNextToken = a})

-- | The maximum number of bot versions to return in the response. The default is 10.
gbvMaxResults :: Lens' GetBotVersions (Maybe Natural)
gbvMaxResults = lens _gbvMaxResults (\ s a -> s{_gbvMaxResults = a}) . mapping _Nat

-- | The name of the bot for which versions should be returned.
gbvName :: Lens' GetBotVersions Text
gbvName = lens _gbvName (\ s a -> s{_gbvName = a})

instance AWSPager GetBotVersions where
        page rq rs
          | stop (rs ^. gbvrsNextToken) = Nothing
          | stop (rs ^. gbvrsBots) = Nothing
          | otherwise =
            Just $ rq & gbvNextToken .~ rs ^. gbvrsNextToken

instance AWSRequest GetBotVersions where
        type Rs GetBotVersions = GetBotVersionsResponse
        request = get lexModels
        response
          = receiveJSON
              (\ s h x ->
                 GetBotVersionsResponse' <$>
                   (x .?> "bots" .!@ mempty) <*> (x .?> "nextToken") <*>
                     (pure (fromEnum s)))

instance Hashable GetBotVersions where

instance NFData GetBotVersions where

instance ToHeaders GetBotVersions where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetBotVersions where
        toPath GetBotVersions'{..}
          = mconcat ["/bots/", toBS _gbvName, "/versions/"]

instance ToQuery GetBotVersions where
        toQuery GetBotVersions'{..}
          = mconcat
              ["nextToken" =: _gbvNextToken,
               "maxResults" =: _gbvMaxResults]

-- | /See:/ 'getBotVersionsResponse' smart constructor.
data GetBotVersionsResponse = GetBotVersionsResponse'
  { _gbvrsBots           :: !(Maybe [BotMetadata])
  , _gbvrsNextToken      :: !(Maybe Text)
  , _gbvrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBotVersionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbvrsBots' - An array of @BotMetadata@ objects, one for each numbered version of the bot plus one for the @> LATEST@ version.
--
-- * 'gbvrsNextToken' - A pagination token for fetching the next page of bot versions. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of versions, specify the pagination token in the next request.
--
-- * 'gbvrsResponseStatus' - -- | The response status code.
getBotVersionsResponse
    :: Int -- ^ 'gbvrsResponseStatus'
    -> GetBotVersionsResponse
getBotVersionsResponse pResponseStatus_ =
  GetBotVersionsResponse'
    { _gbvrsBots = Nothing
    , _gbvrsNextToken = Nothing
    , _gbvrsResponseStatus = pResponseStatus_
    }


-- | An array of @BotMetadata@ objects, one for each numbered version of the bot plus one for the @> LATEST@ version.
gbvrsBots :: Lens' GetBotVersionsResponse [BotMetadata]
gbvrsBots = lens _gbvrsBots (\ s a -> s{_gbvrsBots = a}) . _Default . _Coerce

-- | A pagination token for fetching the next page of bot versions. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of versions, specify the pagination token in the next request.
gbvrsNextToken :: Lens' GetBotVersionsResponse (Maybe Text)
gbvrsNextToken = lens _gbvrsNextToken (\ s a -> s{_gbvrsNextToken = a})

-- | -- | The response status code.
gbvrsResponseStatus :: Lens' GetBotVersionsResponse Int
gbvrsResponseStatus = lens _gbvrsResponseStatus (\ s a -> s{_gbvrsResponseStatus = a})

instance NFData GetBotVersionsResponse where
