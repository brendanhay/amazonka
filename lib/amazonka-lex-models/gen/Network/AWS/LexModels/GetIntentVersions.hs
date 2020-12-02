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
-- Module      : Network.AWS.LexModels.GetIntentVersions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about all of the versions of an intent.
--
--
-- The @GetIntentVersions@ operation returns an @IntentMetadata@ object for each version of an intent. For example, if an intent has three numbered versions, the @GetIntentVersions@ operation returns four @IntentMetadata@ objects in the response, one for each numbered version and one for the @> LATEST@ version.
--
-- The @GetIntentVersions@ operation always returns at least one version, the @> LATEST@ version.
--
-- This operation requires permissions for the @lex:GetIntentVersions@ action.
--
--
-- This operation returns paginated results.
module Network.AWS.LexModels.GetIntentVersions
    (
    -- * Creating a Request
      getIntentVersions
    , GetIntentVersions
    -- * Request Lenses
    , givNextToken
    , givMaxResults
    , givName

    -- * Destructuring the Response
    , getIntentVersionsResponse
    , GetIntentVersionsResponse
    -- * Response Lenses
    , givrsIntents
    , givrsNextToken
    , givrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.LexModels.Types
import Network.AWS.LexModels.Types.Product
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getIntentVersions' smart constructor.
data GetIntentVersions = GetIntentVersions'
  { _givNextToken  :: !(Maybe Text)
  , _givMaxResults :: !(Maybe Nat)
  , _givName       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetIntentVersions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'givNextToken' - A pagination token for fetching the next page of intent versions. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of versions, specify the pagination token in the next request.
--
-- * 'givMaxResults' - The maximum number of intent versions to return in the response. The default is 10.
--
-- * 'givName' - The name of the intent for which versions should be returned.
getIntentVersions
    :: Text -- ^ 'givName'
    -> GetIntentVersions
getIntentVersions pName_ =
  GetIntentVersions'
    {_givNextToken = Nothing, _givMaxResults = Nothing, _givName = pName_}


-- | A pagination token for fetching the next page of intent versions. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of versions, specify the pagination token in the next request.
givNextToken :: Lens' GetIntentVersions (Maybe Text)
givNextToken = lens _givNextToken (\ s a -> s{_givNextToken = a})

-- | The maximum number of intent versions to return in the response. The default is 10.
givMaxResults :: Lens' GetIntentVersions (Maybe Natural)
givMaxResults = lens _givMaxResults (\ s a -> s{_givMaxResults = a}) . mapping _Nat

-- | The name of the intent for which versions should be returned.
givName :: Lens' GetIntentVersions Text
givName = lens _givName (\ s a -> s{_givName = a})

instance AWSPager GetIntentVersions where
        page rq rs
          | stop (rs ^. givrsNextToken) = Nothing
          | stop (rs ^. givrsIntents) = Nothing
          | otherwise =
            Just $ rq & givNextToken .~ rs ^. givrsNextToken

instance AWSRequest GetIntentVersions where
        type Rs GetIntentVersions = GetIntentVersionsResponse
        request = get lexModels
        response
          = receiveJSON
              (\ s h x ->
                 GetIntentVersionsResponse' <$>
                   (x .?> "intents" .!@ mempty) <*> (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable GetIntentVersions where

instance NFData GetIntentVersions where

instance ToHeaders GetIntentVersions where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetIntentVersions where
        toPath GetIntentVersions'{..}
          = mconcat ["/intents/", toBS _givName, "/versions/"]

instance ToQuery GetIntentVersions where
        toQuery GetIntentVersions'{..}
          = mconcat
              ["nextToken" =: _givNextToken,
               "maxResults" =: _givMaxResults]

-- | /See:/ 'getIntentVersionsResponse' smart constructor.
data GetIntentVersionsResponse = GetIntentVersionsResponse'
  { _givrsIntents        :: !(Maybe [IntentMetadata])
  , _givrsNextToken      :: !(Maybe Text)
  , _givrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetIntentVersionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'givrsIntents' - An array of @IntentMetadata@ objects, one for each numbered version of the intent plus one for the @> LATEST@ version.
--
-- * 'givrsNextToken' - A pagination token for fetching the next page of intent versions. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of versions, specify the pagination token in the next request.
--
-- * 'givrsResponseStatus' - -- | The response status code.
getIntentVersionsResponse
    :: Int -- ^ 'givrsResponseStatus'
    -> GetIntentVersionsResponse
getIntentVersionsResponse pResponseStatus_ =
  GetIntentVersionsResponse'
    { _givrsIntents = Nothing
    , _givrsNextToken = Nothing
    , _givrsResponseStatus = pResponseStatus_
    }


-- | An array of @IntentMetadata@ objects, one for each numbered version of the intent plus one for the @> LATEST@ version.
givrsIntents :: Lens' GetIntentVersionsResponse [IntentMetadata]
givrsIntents = lens _givrsIntents (\ s a -> s{_givrsIntents = a}) . _Default . _Coerce

-- | A pagination token for fetching the next page of intent versions. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of versions, specify the pagination token in the next request.
givrsNextToken :: Lens' GetIntentVersionsResponse (Maybe Text)
givrsNextToken = lens _givrsNextToken (\ s a -> s{_givrsNextToken = a})

-- | -- | The response status code.
givrsResponseStatus :: Lens' GetIntentVersionsResponse Int
givrsResponseStatus = lens _givrsResponseStatus (\ s a -> s{_givrsResponseStatus = a})

instance NFData GetIntentVersionsResponse where
