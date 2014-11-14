{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

-- Module      : Network.AWS.CognitoSync.DescribeDataset
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Gets metadata about a dataset by identity and dataset name. The credentials
-- used to make this API call need to have access to the identity data. With
-- Amazon Cognito Sync, each identity has access only to its own data. You
-- should use Amazon Cognito Identity service to retrieve the credentials
-- necessary to make this API call.
module Network.AWS.CognitoSync.DescribeDataset
    (
    -- * Request
      DescribeDataset
    -- ** Request constructor
    , describeDataset
    -- ** Request lenses
    , ddDatasetName
    , ddIdentityId
    , ddIdentityPoolId

    -- * Response
    , DescribeDatasetResponse
    -- ** Response constructor
    , describeDatasetResponse
    -- ** Response lenses
    , ddrDataset
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.CognitoSync.Types

data DescribeDataset = DescribeDataset
    { _ddDatasetName    :: Text
    , _ddIdentityId     :: Text
    , _ddIdentityPoolId :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeDataset' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddDatasetName' @::@ 'Text'
--
-- * 'ddIdentityId' @::@ 'Text'
--
-- * 'ddIdentityPoolId' @::@ 'Text'
--
describeDataset :: Text -- ^ 'ddIdentityPoolId'
                -> Text -- ^ 'ddIdentityId'
                -> Text -- ^ 'ddDatasetName'
                -> DescribeDataset
describeDataset p1 p2 p3 = DescribeDataset
    { _ddIdentityPoolId = p1
    , _ddIdentityId     = p2
    , _ddDatasetName    = p3
    }

-- | A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9,
-- '_' (underscore), '-' (dash), and '.' (dot).
ddDatasetName :: Lens' DescribeDataset Text
ddDatasetName = lens _ddDatasetName (\s a -> s { _ddDatasetName = a })

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
ddIdentityId :: Lens' DescribeDataset Text
ddIdentityId = lens _ddIdentityId (\s a -> s { _ddIdentityId = a })

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
ddIdentityPoolId :: Lens' DescribeDataset Text
ddIdentityPoolId = lens _ddIdentityPoolId (\s a -> s { _ddIdentityPoolId = a })

instance ToPath DescribeDataset where
    toPath DescribeDataset{..} = mconcat
        [ "/identitypools/"
        , toText _ddIdentityPoolId
        , "/identities/"
        , toText _ddIdentityId
        , "/datasets/"
        , toText _ddDatasetName
        ]

instance ToQuery DescribeDataset where
    toQuery = const mempty

instance ToHeaders DescribeDataset

newtype DescribeDatasetResponse = DescribeDatasetResponse
    { _ddrDataset :: Maybe Dataset
    } deriving (Eq, Show, Generic)

-- | 'DescribeDatasetResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddrDataset' @::@ 'Maybe' 'Dataset'
--
describeDatasetResponse :: DescribeDatasetResponse
describeDatasetResponse = DescribeDatasetResponse
    { _ddrDataset = Nothing
    }

-- | Metadata for a collection of data for an identity. An identity can have
-- multiple datasets. A dataset can be general or associated with a
-- particular entity in an application (like a saved game). Datasets are
-- automatically created if they don't exist. Data is synced by dataset, and
-- a dataset can hold up to 1MB of key-value pairs.
ddrDataset :: Lens' DescribeDatasetResponse (Maybe Dataset)
ddrDataset = lens _ddrDataset (\s a -> s { _ddrDataset = a })

instance AWSRequest DescribeDataset where
    type Sv DescribeDataset = CognitoSync
    type Rs DescribeDataset = DescribeDatasetResponse

    request  = get
    response = jsonResponse $ \h o -> DescribeDatasetResponse
        <$> o .: "Dataset"
