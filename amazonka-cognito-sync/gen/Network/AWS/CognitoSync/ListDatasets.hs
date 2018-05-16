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
-- Module      : Network.AWS.CognitoSync.ListDatasets
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists datasets for an identity. With Amazon Cognito Sync, each identity has access only to its own data. Thus, the credentials used to make this API call need to have access to the identity data.
--
--
-- ListDatasets can be called with temporary user credentials provided by Cognito Identity or with developer credentials. You should use the Cognito Identity credentials to make this API call.
--
module Network.AWS.CognitoSync.ListDatasets
    (
    -- * Creating a Request
      listDatasets
    , ListDatasets
    -- * Request Lenses
    , ldNextToken
    , ldMaxResults
    , ldIdentityId
    , ldIdentityPoolId

    -- * Destructuring the Response
    , listDatasetsResponse
    , ListDatasetsResponse
    -- * Response Lenses
    , ldrsCount
    , ldrsNextToken
    , ldrsDatasets
    , ldrsResponseStatus
    ) where

import Network.AWS.CognitoSync.Types
import Network.AWS.CognitoSync.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request for a list of datasets for an identity.
--
-- /See:/ 'listDatasets' smart constructor.
data ListDatasets = ListDatasets'
  { _ldNextToken      :: !(Maybe Text)
  , _ldMaxResults     :: !(Maybe Int)
  , _ldIdentityId     :: !Text
  , _ldIdentityPoolId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListDatasets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldNextToken' - A pagination token for obtaining the next page of results.
--
-- * 'ldMaxResults' - The maximum number of results to be returned.
--
-- * 'ldIdentityId' - A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
--
-- * 'ldIdentityPoolId' - A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
listDatasets
    :: Text -- ^ 'ldIdentityId'
    -> Text -- ^ 'ldIdentityPoolId'
    -> ListDatasets
listDatasets pIdentityId_ pIdentityPoolId_ =
  ListDatasets'
    { _ldNextToken = Nothing
    , _ldMaxResults = Nothing
    , _ldIdentityId = pIdentityId_
    , _ldIdentityPoolId = pIdentityPoolId_
    }


-- | A pagination token for obtaining the next page of results.
ldNextToken :: Lens' ListDatasets (Maybe Text)
ldNextToken = lens _ldNextToken (\ s a -> s{_ldNextToken = a})

-- | The maximum number of results to be returned.
ldMaxResults :: Lens' ListDatasets (Maybe Int)
ldMaxResults = lens _ldMaxResults (\ s a -> s{_ldMaxResults = a})

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
ldIdentityId :: Lens' ListDatasets Text
ldIdentityId = lens _ldIdentityId (\ s a -> s{_ldIdentityId = a})

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
ldIdentityPoolId :: Lens' ListDatasets Text
ldIdentityPoolId = lens _ldIdentityPoolId (\ s a -> s{_ldIdentityPoolId = a})

instance AWSRequest ListDatasets where
        type Rs ListDatasets = ListDatasetsResponse
        request = get cognitoSync
        response
          = receiveJSON
              (\ s h x ->
                 ListDatasetsResponse' <$>
                   (x .?> "Count") <*> (x .?> "NextToken") <*>
                     (x .?> "Datasets" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListDatasets where

instance NFData ListDatasets where

instance ToHeaders ListDatasets where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ListDatasets where
        toPath ListDatasets'{..}
          = mconcat
              ["/identitypools/", toBS _ldIdentityPoolId,
               "/identities/", toBS _ldIdentityId, "/datasets"]

instance ToQuery ListDatasets where
        toQuery ListDatasets'{..}
          = mconcat
              ["nextToken" =: _ldNextToken,
               "maxResults" =: _ldMaxResults]

-- | Returned for a successful ListDatasets request.
--
-- /See:/ 'listDatasetsResponse' smart constructor.
data ListDatasetsResponse = ListDatasetsResponse'
  { _ldrsCount          :: !(Maybe Int)
  , _ldrsNextToken      :: !(Maybe Text)
  , _ldrsDatasets       :: !(Maybe [Dataset])
  , _ldrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListDatasetsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldrsCount' - Number of datasets returned.
--
-- * 'ldrsNextToken' - A pagination token for obtaining the next page of results.
--
-- * 'ldrsDatasets' - A set of datasets.
--
-- * 'ldrsResponseStatus' - -- | The response status code.
listDatasetsResponse
    :: Int -- ^ 'ldrsResponseStatus'
    -> ListDatasetsResponse
listDatasetsResponse pResponseStatus_ =
  ListDatasetsResponse'
    { _ldrsCount = Nothing
    , _ldrsNextToken = Nothing
    , _ldrsDatasets = Nothing
    , _ldrsResponseStatus = pResponseStatus_
    }


-- | Number of datasets returned.
ldrsCount :: Lens' ListDatasetsResponse (Maybe Int)
ldrsCount = lens _ldrsCount (\ s a -> s{_ldrsCount = a})

-- | A pagination token for obtaining the next page of results.
ldrsNextToken :: Lens' ListDatasetsResponse (Maybe Text)
ldrsNextToken = lens _ldrsNextToken (\ s a -> s{_ldrsNextToken = a})

-- | A set of datasets.
ldrsDatasets :: Lens' ListDatasetsResponse [Dataset]
ldrsDatasets = lens _ldrsDatasets (\ s a -> s{_ldrsDatasets = a}) . _Default . _Coerce

-- | -- | The response status code.
ldrsResponseStatus :: Lens' ListDatasetsResponse Int
ldrsResponseStatus = lens _ldrsResponseStatus (\ s a -> s{_ldrsResponseStatus = a})

instance NFData ListDatasetsResponse where
