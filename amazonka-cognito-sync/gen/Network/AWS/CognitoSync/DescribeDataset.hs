{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CognitoSync.DescribeDataset
-- Copyright   : (c) 2013-2015 Brendan Hay
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
-- This API can be called with temporary user credentials provided by
-- Cognito Identity or with developer credentials. You should use Cognito
-- Identity credentials to make this API call.
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
    , ddrStatus
    ) where

import           Network.AWS.CognitoSync.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | A request for meta data about a dataset (creation date, number of
-- records, size) by owner and dataset name.
--
-- /See:/ 'describeDataset' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddIdentityPoolId'
--
-- * 'ddIdentityId'
--
-- * 'ddDatasetName'
data DescribeDataset = DescribeDataset'
    { _ddIdentityPoolId :: !Text
    , _ddIdentityId     :: !Text
    , _ddDatasetName    :: !Text
    } deriving (Eq,Read,Show)

-- | 'DescribeDataset' smart constructor.
describeDataset :: Text -> Text -> Text -> DescribeDataset
describeDataset pIdentityPoolId pIdentityId pDatasetName =
    DescribeDataset'
    { _ddIdentityPoolId = pIdentityPoolId
    , _ddIdentityId = pIdentityId
    , _ddDatasetName = pDatasetName
    }

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
                 DescribeDatasetResponse' <$>
                   (x .?> "Dataset") <*> (pure (fromEnum s)))

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

-- | Response to a successful DescribeDataset request.
--
-- /See:/ 'describeDatasetResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddrDataset'
--
-- * 'ddrStatus'
data DescribeDatasetResponse = DescribeDatasetResponse'
    { _ddrDataset :: !(Maybe Dataset)
    , _ddrStatus  :: !Int
    } deriving (Eq,Read,Show)

-- | 'DescribeDatasetResponse' smart constructor.
describeDatasetResponse :: Int -> DescribeDatasetResponse
describeDatasetResponse pStatus =
    DescribeDatasetResponse'
    { _ddrDataset = Nothing
    , _ddrStatus = pStatus
    }

-- | Meta data for a collection of data for an identity. An identity can have
-- multiple datasets. A dataset can be general or associated with a
-- particular entity in an application (like a saved game). Datasets are
-- automatically created if they don\'t exist. Data is synced by dataset,
-- and a dataset can hold up to 1MB of key-value pairs.
ddrDataset :: Lens' DescribeDatasetResponse (Maybe Dataset)
ddrDataset = lens _ddrDataset (\ s a -> s{_ddrDataset = a});

-- | FIXME: Undocumented member.
ddrStatus :: Lens' DescribeDatasetResponse Int
ddrStatus = lens _ddrStatus (\ s a -> s{_ddrStatus = a});
