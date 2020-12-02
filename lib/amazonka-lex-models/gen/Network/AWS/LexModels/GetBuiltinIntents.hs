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
-- Module      : Network.AWS.LexModels.GetBuiltinIntents
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of built-in intents that meet the specified criteria.
--
--
-- This operation requires permission for the @lex:GetBuiltinIntents@ action.
--
--
-- This operation returns paginated results.
module Network.AWS.LexModels.GetBuiltinIntents
    (
    -- * Creating a Request
      getBuiltinIntents
    , GetBuiltinIntents
    -- * Request Lenses
    , gbiLocale
    , gbiNextToken
    , gbiSignatureContains
    , gbiMaxResults

    -- * Destructuring the Response
    , getBuiltinIntentsResponse
    , GetBuiltinIntentsResponse
    -- * Response Lenses
    , grsIntents
    , grsNextToken
    , grsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.LexModels.Types
import Network.AWS.LexModels.Types.Product
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getBuiltinIntents' smart constructor.
data GetBuiltinIntents = GetBuiltinIntents'
  { _gbiLocale            :: !(Maybe Locale)
  , _gbiNextToken         :: !(Maybe Text)
  , _gbiSignatureContains :: !(Maybe Text)
  , _gbiMaxResults        :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBuiltinIntents' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbiLocale' - A list of locales that the intent supports.
--
-- * 'gbiNextToken' - A pagination token that fetches the next page of intents. If this API call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of intents, use the pagination token in the next request.
--
-- * 'gbiSignatureContains' - Substring to match in built-in intent signatures. An intent will be returned if any part of its signature matches the substring. For example, "xyz" matches both "xyzabc" and "abcxyz." To find the signature for an intent, see <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/standard-intents Standard Built-in Intents> in the /Alexa Skills Kit/ .
--
-- * 'gbiMaxResults' - The maximum number of intents to return in the response. The default is 10.
getBuiltinIntents
    :: GetBuiltinIntents
getBuiltinIntents =
  GetBuiltinIntents'
    { _gbiLocale = Nothing
    , _gbiNextToken = Nothing
    , _gbiSignatureContains = Nothing
    , _gbiMaxResults = Nothing
    }


-- | A list of locales that the intent supports.
gbiLocale :: Lens' GetBuiltinIntents (Maybe Locale)
gbiLocale = lens _gbiLocale (\ s a -> s{_gbiLocale = a})

-- | A pagination token that fetches the next page of intents. If this API call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of intents, use the pagination token in the next request.
gbiNextToken :: Lens' GetBuiltinIntents (Maybe Text)
gbiNextToken = lens _gbiNextToken (\ s a -> s{_gbiNextToken = a})

-- | Substring to match in built-in intent signatures. An intent will be returned if any part of its signature matches the substring. For example, "xyz" matches both "xyzabc" and "abcxyz." To find the signature for an intent, see <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/standard-intents Standard Built-in Intents> in the /Alexa Skills Kit/ .
gbiSignatureContains :: Lens' GetBuiltinIntents (Maybe Text)
gbiSignatureContains = lens _gbiSignatureContains (\ s a -> s{_gbiSignatureContains = a})

-- | The maximum number of intents to return in the response. The default is 10.
gbiMaxResults :: Lens' GetBuiltinIntents (Maybe Natural)
gbiMaxResults = lens _gbiMaxResults (\ s a -> s{_gbiMaxResults = a}) . mapping _Nat

instance AWSPager GetBuiltinIntents where
        page rq rs
          | stop (rs ^. grsNextToken) = Nothing
          | stop (rs ^. grsIntents) = Nothing
          | otherwise =
            Just $ rq & gbiNextToken .~ rs ^. grsNextToken

instance AWSRequest GetBuiltinIntents where
        type Rs GetBuiltinIntents = GetBuiltinIntentsResponse
        request = get lexModels
        response
          = receiveJSON
              (\ s h x ->
                 GetBuiltinIntentsResponse' <$>
                   (x .?> "intents" .!@ mempty) <*> (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable GetBuiltinIntents where

instance NFData GetBuiltinIntents where

instance ToHeaders GetBuiltinIntents where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetBuiltinIntents where
        toPath = const "/builtins/intents/"

instance ToQuery GetBuiltinIntents where
        toQuery GetBuiltinIntents'{..}
          = mconcat
              ["locale" =: _gbiLocale,
               "nextToken" =: _gbiNextToken,
               "signatureContains" =: _gbiSignatureContains,
               "maxResults" =: _gbiMaxResults]

-- | /See:/ 'getBuiltinIntentsResponse' smart constructor.
data GetBuiltinIntentsResponse = GetBuiltinIntentsResponse'
  { _grsIntents        :: !(Maybe [BuiltinIntentMetadata])
  , _grsNextToken      :: !(Maybe Text)
  , _grsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBuiltinIntentsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grsIntents' - An array of @builtinIntentMetadata@ objects, one for each intent in the response.
--
-- * 'grsNextToken' - A pagination token that fetches the next page of intents. If the response to this API call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of intents, specify the pagination token in the next request.
--
-- * 'grsResponseStatus' - -- | The response status code.
getBuiltinIntentsResponse
    :: Int -- ^ 'grsResponseStatus'
    -> GetBuiltinIntentsResponse
getBuiltinIntentsResponse pResponseStatus_ =
  GetBuiltinIntentsResponse'
    { _grsIntents = Nothing
    , _grsNextToken = Nothing
    , _grsResponseStatus = pResponseStatus_
    }


-- | An array of @builtinIntentMetadata@ objects, one for each intent in the response.
grsIntents :: Lens' GetBuiltinIntentsResponse [BuiltinIntentMetadata]
grsIntents = lens _grsIntents (\ s a -> s{_grsIntents = a}) . _Default . _Coerce

-- | A pagination token that fetches the next page of intents. If the response to this API call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of intents, specify the pagination token in the next request.
grsNextToken :: Lens' GetBuiltinIntentsResponse (Maybe Text)
grsNextToken = lens _grsNextToken (\ s a -> s{_grsNextToken = a})

-- | -- | The response status code.
grsResponseStatus :: Lens' GetBuiltinIntentsResponse Int
grsResponseStatus = lens _grsResponseStatus (\ s a -> s{_grsResponseStatus = a})

instance NFData GetBuiltinIntentsResponse where
