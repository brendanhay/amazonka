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
-- Module      : Network.AWS.LexModels.GetBotAliases
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of aliases for a specified Amazon Lex bot.
--
--
-- This operation requires permissions for the @lex:GetBotAliases@ action.
--
--
-- This operation returns paginated results.
module Network.AWS.LexModels.GetBotAliases
    (
    -- * Creating a Request
      getBotAliases
    , GetBotAliases
    -- * Request Lenses
    , gbaNameContains
    , gbaNextToken
    , gbaMaxResults
    , gbaBotName

    -- * Destructuring the Response
    , getBotAliasesResponse
    , GetBotAliasesResponse
    -- * Response Lenses
    , gbarsNextToken
    , gbarsBotAliases
    , gbarsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.LexModels.Types
import Network.AWS.LexModels.Types.Product
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getBotAliases' smart constructor.
data GetBotAliases = GetBotAliases'
  { _gbaNameContains :: !(Maybe Text)
  , _gbaNextToken    :: !(Maybe Text)
  , _gbaMaxResults   :: !(Maybe Nat)
  , _gbaBotName      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBotAliases' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbaNameContains' - Substring to match in bot alias names. An alias will be returned if any part of its name matches the substring. For example, "xyz" matches both "xyzabc" and "abcxyz."
--
-- * 'gbaNextToken' - A pagination token for fetching the next page of aliases. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of aliases, specify the pagination token in the next request.
--
-- * 'gbaMaxResults' - The maximum number of aliases to return in the response. The default is 50. .
--
-- * 'gbaBotName' - The name of the bot.
getBotAliases
    :: Text -- ^ 'gbaBotName'
    -> GetBotAliases
getBotAliases pBotName_ =
  GetBotAliases'
    { _gbaNameContains = Nothing
    , _gbaNextToken = Nothing
    , _gbaMaxResults = Nothing
    , _gbaBotName = pBotName_
    }


-- | Substring to match in bot alias names. An alias will be returned if any part of its name matches the substring. For example, "xyz" matches both "xyzabc" and "abcxyz."
gbaNameContains :: Lens' GetBotAliases (Maybe Text)
gbaNameContains = lens _gbaNameContains (\ s a -> s{_gbaNameContains = a})

-- | A pagination token for fetching the next page of aliases. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of aliases, specify the pagination token in the next request.
gbaNextToken :: Lens' GetBotAliases (Maybe Text)
gbaNextToken = lens _gbaNextToken (\ s a -> s{_gbaNextToken = a})

-- | The maximum number of aliases to return in the response. The default is 50. .
gbaMaxResults :: Lens' GetBotAliases (Maybe Natural)
gbaMaxResults = lens _gbaMaxResults (\ s a -> s{_gbaMaxResults = a}) . mapping _Nat

-- | The name of the bot.
gbaBotName :: Lens' GetBotAliases Text
gbaBotName = lens _gbaBotName (\ s a -> s{_gbaBotName = a})

instance AWSPager GetBotAliases where
        page rq rs
          | stop (rs ^. gbarsNextToken) = Nothing
          | stop (rs ^. gbarsBotAliases) = Nothing
          | otherwise =
            Just $ rq & gbaNextToken .~ rs ^. gbarsNextToken

instance AWSRequest GetBotAliases where
        type Rs GetBotAliases = GetBotAliasesResponse
        request = get lexModels
        response
          = receiveJSON
              (\ s h x ->
                 GetBotAliasesResponse' <$>
                   (x .?> "nextToken") <*>
                     (x .?> "BotAliases" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetBotAliases where

instance NFData GetBotAliases where

instance ToHeaders GetBotAliases where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetBotAliases where
        toPath GetBotAliases'{..}
          = mconcat ["/bots/", toBS _gbaBotName, "/aliases/"]

instance ToQuery GetBotAliases where
        toQuery GetBotAliases'{..}
          = mconcat
              ["nameContains" =: _gbaNameContains,
               "nextToken" =: _gbaNextToken,
               "maxResults" =: _gbaMaxResults]

-- | /See:/ 'getBotAliasesResponse' smart constructor.
data GetBotAliasesResponse = GetBotAliasesResponse'
  { _gbarsNextToken      :: !(Maybe Text)
  , _gbarsBotAliases     :: !(Maybe [BotAliasMetadata])
  , _gbarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBotAliasesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbarsNextToken' - A pagination token for fetching next page of aliases. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of aliases, specify the pagination token in the next request.
--
-- * 'gbarsBotAliases' - An array of @BotAliasMetadata@ objects, each describing a bot alias.
--
-- * 'gbarsResponseStatus' - -- | The response status code.
getBotAliasesResponse
    :: Int -- ^ 'gbarsResponseStatus'
    -> GetBotAliasesResponse
getBotAliasesResponse pResponseStatus_ =
  GetBotAliasesResponse'
    { _gbarsNextToken = Nothing
    , _gbarsBotAliases = Nothing
    , _gbarsResponseStatus = pResponseStatus_
    }


-- | A pagination token for fetching next page of aliases. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of aliases, specify the pagination token in the next request.
gbarsNextToken :: Lens' GetBotAliasesResponse (Maybe Text)
gbarsNextToken = lens _gbarsNextToken (\ s a -> s{_gbarsNextToken = a})

-- | An array of @BotAliasMetadata@ objects, each describing a bot alias.
gbarsBotAliases :: Lens' GetBotAliasesResponse [BotAliasMetadata]
gbarsBotAliases = lens _gbarsBotAliases (\ s a -> s{_gbarsBotAliases = a}) . _Default . _Coerce

-- | -- | The response status code.
gbarsResponseStatus :: Lens' GetBotAliasesResponse Int
gbarsResponseStatus = lens _gbarsResponseStatus (\ s a -> s{_gbarsResponseStatus = a})

instance NFData GetBotAliasesResponse where
