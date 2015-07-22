{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.ListDatasets
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Lists datasets for an identity. With Amazon Cognito Sync, each identity
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
    , ldrqNextToken
    , ldrqMaxResults
    , ldrqIdentityId
    , ldrqIdentityPoolId

    -- * Response
    , ListDatasetsResponse
    -- ** Response constructor
    , listDatasetsResponse
    -- ** Response lenses
    , ldrsCount
    , ldrsNextToken
    , ldrsDatasets
    , ldrsStatus
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
-- * 'ldrqNextToken'
--
-- * 'ldrqMaxResults'
--
-- * 'ldrqIdentityId'
--
-- * 'ldrqIdentityPoolId'
data ListDatasets = ListDatasets'
    { _ldrqNextToken      :: !(Maybe Text)
    , _ldrqMaxResults     :: !(Maybe Int)
    , _ldrqIdentityId     :: !Text
    , _ldrqIdentityPoolId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListDatasets' smart constructor.
listDatasets :: Text -> Text -> ListDatasets
listDatasets pIdentityId pIdentityPoolId =
    ListDatasets'
    { _ldrqNextToken = Nothing
    , _ldrqMaxResults = Nothing
    , _ldrqIdentityId = pIdentityId
    , _ldrqIdentityPoolId = pIdentityPoolId
    }

-- | A pagination token for obtaining the next page of results.
ldrqNextToken :: Lens' ListDatasets (Maybe Text)
ldrqNextToken = lens _ldrqNextToken (\ s a -> s{_ldrqNextToken = a});

-- | The maximum number of results to be returned.
ldrqMaxResults :: Lens' ListDatasets (Maybe Int)
ldrqMaxResults = lens _ldrqMaxResults (\ s a -> s{_ldrqMaxResults = a});

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
ldrqIdentityId :: Lens' ListDatasets Text
ldrqIdentityId = lens _ldrqIdentityId (\ s a -> s{_ldrqIdentityId = a});

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
ldrqIdentityPoolId :: Lens' ListDatasets Text
ldrqIdentityPoolId = lens _ldrqIdentityPoolId (\ s a -> s{_ldrqIdentityPoolId = a});

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
              ["/identitypools/", toText _ldrqIdentityPoolId,
               "/identities/", toText _ldrqIdentityId, "/datasets"]

instance ToQuery ListDatasets where
        toQuery ListDatasets'{..}
          = mconcat
              ["nextToken" =: _ldrqNextToken,
               "maxResults" =: _ldrqMaxResults]

-- | Returned for a successful ListDatasets request.
--
-- /See:/ 'listDatasetsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldrsCount'
--
-- * 'ldrsNextToken'
--
-- * 'ldrsDatasets'
--
-- * 'ldrsStatus'
data ListDatasetsResponse = ListDatasetsResponse'
    { _ldrsCount     :: !(Maybe Int)
    , _ldrsNextToken :: !(Maybe Text)
    , _ldrsDatasets  :: !(Maybe [Dataset])
    , _ldrsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListDatasetsResponse' smart constructor.
listDatasetsResponse :: Int -> ListDatasetsResponse
listDatasetsResponse pStatus =
    ListDatasetsResponse'
    { _ldrsCount = Nothing
    , _ldrsNextToken = Nothing
    , _ldrsDatasets = Nothing
    , _ldrsStatus = pStatus
    }

-- | Number of datasets returned.
ldrsCount :: Lens' ListDatasetsResponse (Maybe Int)
ldrsCount = lens _ldrsCount (\ s a -> s{_ldrsCount = a});

-- | A pagination token for obtaining the next page of results.
ldrsNextToken :: Lens' ListDatasetsResponse (Maybe Text)
ldrsNextToken = lens _ldrsNextToken (\ s a -> s{_ldrsNextToken = a});

-- | A set of datasets.
ldrsDatasets :: Lens' ListDatasetsResponse [Dataset]
ldrsDatasets = lens _ldrsDatasets (\ s a -> s{_ldrsDatasets = a}) . _Default;

-- | FIXME: Undocumented member.
ldrsStatus :: Lens' ListDatasetsResponse Int
ldrsStatus = lens _ldrsStatus (\ s a -> s{_ldrsStatus = a});
