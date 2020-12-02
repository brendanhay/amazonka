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
-- Module      : Network.AWS.CognitoSync.DescribeDataset
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets meta data about a dataset by identity and dataset name. With Amazon Cognito Sync, each identity has access only to its own data. Thus, the credentials used to make this API call need to have access to the identity data.
--
--
-- This API can be called with temporary user credentials provided by Cognito Identity or with developer credentials. You should use Cognito Identity credentials to make this API call.
--
module Network.AWS.CognitoSync.DescribeDataset
    (
    -- * Creating a Request
      describeDataset
    , DescribeDataset
    -- * Request Lenses
    , ddIdentityPoolId
    , ddIdentityId
    , ddDatasetName

    -- * Destructuring the Response
    , describeDatasetResponse
    , DescribeDatasetResponse
    -- * Response Lenses
    , ddrsDataset
    , ddrsResponseStatus
    ) where

import Network.AWS.CognitoSync.Types
import Network.AWS.CognitoSync.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | A request for meta data about a dataset (creation date, number of records, size) by owner and dataset name.
--
-- /See:/ 'describeDataset' smart constructor.
data DescribeDataset = DescribeDataset'
  { _ddIdentityPoolId :: !Text
  , _ddIdentityId     :: !Text
  , _ddDatasetName    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDataset' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddIdentityPoolId' - A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
--
-- * 'ddIdentityId' - A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
--
-- * 'ddDatasetName' - A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9, '_' (underscore), '-' (dash), and '.' (dot).
describeDataset
    :: Text -- ^ 'ddIdentityPoolId'
    -> Text -- ^ 'ddIdentityId'
    -> Text -- ^ 'ddDatasetName'
    -> DescribeDataset
describeDataset pIdentityPoolId_ pIdentityId_ pDatasetName_ =
  DescribeDataset'
    { _ddIdentityPoolId = pIdentityPoolId_
    , _ddIdentityId = pIdentityId_
    , _ddDatasetName = pDatasetName_
    }


-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
ddIdentityPoolId :: Lens' DescribeDataset Text
ddIdentityPoolId = lens _ddIdentityPoolId (\ s a -> s{_ddIdentityPoolId = a})

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
ddIdentityId :: Lens' DescribeDataset Text
ddIdentityId = lens _ddIdentityId (\ s a -> s{_ddIdentityId = a})

-- | A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9, '_' (underscore), '-' (dash), and '.' (dot).
ddDatasetName :: Lens' DescribeDataset Text
ddDatasetName = lens _ddDatasetName (\ s a -> s{_ddDatasetName = a})

instance AWSRequest DescribeDataset where
        type Rs DescribeDataset = DescribeDatasetResponse
        request = get cognitoSync
        response
          = receiveJSON
              (\ s h x ->
                 DescribeDatasetResponse' <$>
                   (x .?> "Dataset") <*> (pure (fromEnum s)))

instance Hashable DescribeDataset where

instance NFData DescribeDataset where

instance ToHeaders DescribeDataset where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DescribeDataset where
        toPath DescribeDataset'{..}
          = mconcat
              ["/identitypools/", toBS _ddIdentityPoolId,
               "/identities/", toBS _ddIdentityId, "/datasets/",
               toBS _ddDatasetName]

instance ToQuery DescribeDataset where
        toQuery = const mempty

-- | Response to a successful DescribeDataset request.
--
-- /See:/ 'describeDatasetResponse' smart constructor.
data DescribeDatasetResponse = DescribeDatasetResponse'
  { _ddrsDataset        :: !(Maybe Dataset)
  , _ddrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDatasetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddrsDataset' - Meta data for a collection of data for an identity. An identity can have multiple datasets. A dataset can be general or associated with a particular entity in an application (like a saved game). Datasets are automatically created if they don't exist. Data is synced by dataset, and a dataset can hold up to 1MB of key-value pairs.
--
-- * 'ddrsResponseStatus' - -- | The response status code.
describeDatasetResponse
    :: Int -- ^ 'ddrsResponseStatus'
    -> DescribeDatasetResponse
describeDatasetResponse pResponseStatus_ =
  DescribeDatasetResponse'
    {_ddrsDataset = Nothing, _ddrsResponseStatus = pResponseStatus_}


-- | Meta data for a collection of data for an identity. An identity can have multiple datasets. A dataset can be general or associated with a particular entity in an application (like a saved game). Datasets are automatically created if they don't exist. Data is synced by dataset, and a dataset can hold up to 1MB of key-value pairs.
ddrsDataset :: Lens' DescribeDatasetResponse (Maybe Dataset)
ddrsDataset = lens _ddrsDataset (\ s a -> s{_ddrsDataset = a})

-- | -- | The response status code.
ddrsResponseStatus :: Lens' DescribeDatasetResponse Int
ddrsResponseStatus = lens _ddrsResponseStatus (\ s a -> s{_ddrsResponseStatus = a})

instance NFData DescribeDatasetResponse where
