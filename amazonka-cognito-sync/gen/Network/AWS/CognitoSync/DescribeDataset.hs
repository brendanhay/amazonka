{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.DescribeDataset
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Gets meta data about a dataset by identity and dataset name. With Amazon
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
    , ddrsDataset
    , ddrsStatus
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeDataset' smart constructor.
describeDataset :: Text -> Text -> Text -> DescribeDataset
describeDataset pIdentityPoolId_ pIdentityId_ pDatasetName_ =
    DescribeDataset'
    { _ddIdentityPoolId = pIdentityPoolId_
    , _ddIdentityId = pIdentityId_
    , _ddDatasetName = pDatasetName_
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
              ["/identitypools/", toPath _ddIdentityPoolId,
               "/identities/", toPath _ddIdentityId, "/datasets/",
               toPath _ddDatasetName]

instance ToQuery DescribeDataset where
        toQuery = const mempty

-- | Response to a successful DescribeDataset request.
--
-- /See:/ 'describeDatasetResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddrsDataset'
--
-- * 'ddrsStatus'
data DescribeDatasetResponse = DescribeDatasetResponse'
    { _ddrsDataset :: !(Maybe Dataset)
    , _ddrsStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeDatasetResponse' smart constructor.
describeDatasetResponse :: Int -> DescribeDatasetResponse
describeDatasetResponse pStatus_ =
    DescribeDatasetResponse'
    { _ddrsDataset = Nothing
    , _ddrsStatus = pStatus_
    }

-- | Meta data for a collection of data for an identity. An identity can have
-- multiple datasets. A dataset can be general or associated with a
-- particular entity in an application (like a saved game). Datasets are
-- automatically created if they don\'t exist. Data is synced by dataset,
-- and a dataset can hold up to 1MB of key-value pairs.
ddrsDataset :: Lens' DescribeDatasetResponse (Maybe Dataset)
ddrsDataset = lens _ddrsDataset (\ s a -> s{_ddrsDataset = a});

-- | FIXME: Undocumented member.
ddrsStatus :: Lens' DescribeDatasetResponse Int
ddrsStatus = lens _ddrsStatus (\ s a -> s{_ddrsStatus = a});
