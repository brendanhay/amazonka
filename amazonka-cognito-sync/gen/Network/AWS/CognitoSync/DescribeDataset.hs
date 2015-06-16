{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CognitoSync.DescribeDataset
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

-- | Gets meta data about a dataset by identity and dataset name. With Amazon
-- Cognito Sync, each identity has access only to its own data. Thus, the
-- credentials used to make this API call need to have access to the
-- identity data.
--
-- DescribeDataset can be called with temporary user credentials provided
-- by Cognito Identity or with developer credentials. You should use
-- Cognito Identity credentials to make this API call.
--
-- <http://docs.aws.amazon.com/cognitosync/latest/APIReference/API_DescribeDataset.html>
module Network.AWS.CognitoSync.DescribeDataset
    (
    -- * Request
      DescribeDataset
    -- ** Request constructor
    , describeDataset
    -- ** Request lenses
    , ddIdentityPoolId
    , ddIdentityId
    , ddDatasetName

    -- * Response
    , DescribeDatasetResponse
    -- ** Response constructor
    , describeDatasetResponse
    -- ** Response lenses
    , ddrDataset
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CognitoSync.Types

-- | /See:/ 'describeDataset' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddIdentityPoolId'
--
-- * 'ddIdentityId'
--
-- * 'ddDatasetName'
data DescribeDataset = DescribeDataset'{_ddIdentityPoolId :: Text, _ddIdentityId :: Text, _ddDatasetName :: Text} deriving (Eq, Read, Show)

-- | 'DescribeDataset' smart constructor.
describeDataset :: Text -> Text -> Text -> DescribeDataset
describeDataset pIdentityPoolId pIdentityId pDatasetName = DescribeDataset'{_ddIdentityPoolId = pIdentityPoolId, _ddIdentityId = pIdentityId, _ddDatasetName = pDatasetName};

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
ddIdentityPoolId :: Lens' DescribeDataset Text
ddIdentityPoolId = lens _ddIdentityPoolId (\ s a -> s{_ddIdentityPoolId = a});

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
ddIdentityId :: Lens' DescribeDataset Text
ddIdentityId = lens _ddIdentityId (\ s a -> s{_ddIdentityId = a});

-- | A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9,
-- \'_\' (underscore), \'-\' (dash), and \'.\' (dot).
ddDatasetName :: Lens' DescribeDataset Text
ddDatasetName = lens _ddDatasetName (\ s a -> s{_ddDatasetName = a});

instance AWSRequest DescribeDataset where
        type Sv DescribeDataset = CognitoSync
        type Rs DescribeDataset = DescribeDatasetResponse
        request = get
        response
          = receiveJSON
              (\ s h x ->
                 DescribeDatasetResponse' <$> (x .?> "Dataset"))

instance ToHeaders DescribeDataset where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DescribeDataset where
        toPath DescribeDataset'{..}
          = mconcat
              ["/identitypools/", toText _ddIdentityPoolId,
               "/identities/", toText _ddIdentityId, "/datasets/",
               toText _ddDatasetName]

instance ToQuery DescribeDataset where
        toQuery = const mempty

-- | /See:/ 'describeDatasetResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddrDataset'
newtype DescribeDatasetResponse = DescribeDatasetResponse'{_ddrDataset :: Maybe Dataset} deriving (Eq, Read, Show)

-- | 'DescribeDatasetResponse' smart constructor.
describeDatasetResponse :: DescribeDatasetResponse
describeDatasetResponse = DescribeDatasetResponse'{_ddrDataset = Nothing};

-- | Meta data for a collection of data for an identity. An identity can have
-- multiple datasets. A dataset can be general or associated with a
-- particular entity in an application (like a saved game). Datasets are
-- automatically created if they don\'t exist. Data is synced by dataset,
-- and a dataset can hold up to 1MB of key-value pairs.
ddrDataset :: Lens' DescribeDatasetResponse (Maybe Dataset)
ddrDataset = lens _ddrDataset (\ s a -> s{_ddrDataset = a});
