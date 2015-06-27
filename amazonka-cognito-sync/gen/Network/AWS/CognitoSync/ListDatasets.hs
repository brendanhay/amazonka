{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CognitoSync.ListDatasets
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Lists datasets for an identity. With Amazon Cognito Sync, each identity
-- has access only to its own data. Thus, the credentials used to make this
-- API call need to have access to the identity data.
--
-- ListDatasets can be called with temporary user credentials provided by
-- Cognito Identity or with developer credentials. You should use the
-- Cognito Identity credentials to make this API call.
--
-- <http://docs.aws.amazon.com/cognitosync/latest/APIReference/API_ListDatasets.html>
module Network.AWS.CognitoSync.ListDatasets
    (
    -- * Request
      ListDatasets
    -- ** Request constructor
    , listDatasets
    -- ** Request lenses
    , ldNextToken
    , ldMaxResults
    , ldIdentityId
    , ldIdentityPoolId

    -- * Response
    , ListDatasetsResponse
    -- ** Response constructor
    , listDatasetsResponse
    -- ** Response lenses
    , ldrCount
    , ldrNextToken
    , ldrDatasets
    , ldrStatus
    ) where

import           Network.AWS.CognitoSync.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Request for a list of datasets for an identity.
--
-- /See:/ 'listDatasets' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldNextToken'
--
-- * 'ldMaxResults'
--
-- * 'ldIdentityId'
--
-- * 'ldIdentityPoolId'
data ListDatasets = ListDatasets'
    { _ldNextToken      :: !(Maybe Text)
    , _ldMaxResults     :: !(Maybe Int)
    , _ldIdentityId     :: !Text
    , _ldIdentityPoolId :: !Text
    } deriving (Eq,Read,Show)

-- | 'ListDatasets' smart constructor.
listDatasets :: Text -> Text -> ListDatasets
listDatasets pIdentityId pIdentityPoolId =
    ListDatasets'
    { _ldNextToken = Nothing
    , _ldMaxResults = Nothing
    , _ldIdentityId = pIdentityId
    , _ldIdentityPoolId = pIdentityPoolId
    }

-- | A pagination token for obtaining the next page of results.
ldNextToken :: Lens' ListDatasets (Maybe Text)
ldNextToken = lens _ldNextToken (\ s a -> s{_ldNextToken = a});

-- | The maximum number of results to be returned.
ldMaxResults :: Lens' ListDatasets (Maybe Int)
ldMaxResults = lens _ldMaxResults (\ s a -> s{_ldMaxResults = a});

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
ldIdentityId :: Lens' ListDatasets Text
ldIdentityId = lens _ldIdentityId (\ s a -> s{_ldIdentityId = a});

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
ldIdentityPoolId :: Lens' ListDatasets Text
ldIdentityPoolId = lens _ldIdentityPoolId (\ s a -> s{_ldIdentityPoolId = a});

instance AWSRequest ListDatasets where
        type Sv ListDatasets = CognitoSync
        type Rs ListDatasets = ListDatasetsResponse
        request = get
        response
          = receiveJSON
              (\ s h x ->
                 ListDatasetsResponse' <$>
                   (x .?> "Count") <*> (x .?> "NextToken") <*>
                     (x .?> "Datasets" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance ToHeaders ListDatasets where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ListDatasets where
        toPath ListDatasets'{..}
          = mconcat
              ["/identitypools/", toText _ldIdentityPoolId,
               "/identities/", toText _ldIdentityId, "/datasets"]

instance ToQuery ListDatasets where
        toQuery ListDatasets'{..}
          = mconcat
              ["nextToken" =: _ldNextToken,
               "maxResults" =: _ldMaxResults]

-- | Returned for a successful ListDatasets request.
--
-- /See:/ 'listDatasetsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldrCount'
--
-- * 'ldrNextToken'
--
-- * 'ldrDatasets'
--
-- * 'ldrStatus'
data ListDatasetsResponse = ListDatasetsResponse'
    { _ldrCount     :: !(Maybe Int)
    , _ldrNextToken :: !(Maybe Text)
    , _ldrDatasets  :: !(Maybe [Dataset])
    , _ldrStatus    :: !Int
    } deriving (Eq,Read,Show)

-- | 'ListDatasetsResponse' smart constructor.
listDatasetsResponse :: Int -> ListDatasetsResponse
listDatasetsResponse pStatus =
    ListDatasetsResponse'
    { _ldrCount = Nothing
    , _ldrNextToken = Nothing
    , _ldrDatasets = Nothing
    , _ldrStatus = pStatus
    }

-- | Number of datasets returned.
ldrCount :: Lens' ListDatasetsResponse (Maybe Int)
ldrCount = lens _ldrCount (\ s a -> s{_ldrCount = a});

-- | A pagination token for obtaining the next page of results.
ldrNextToken :: Lens' ListDatasetsResponse (Maybe Text)
ldrNextToken = lens _ldrNextToken (\ s a -> s{_ldrNextToken = a});

-- | A set of datasets.
ldrDatasets :: Lens' ListDatasetsResponse [Dataset]
ldrDatasets = lens _ldrDatasets (\ s a -> s{_ldrDatasets = a}) . _Default;

-- | FIXME: Undocumented member.
ldrStatus :: Lens' ListDatasetsResponse Int
ldrStatus = lens _ldrStatus (\ s a -> s{_ldrStatus = a});
