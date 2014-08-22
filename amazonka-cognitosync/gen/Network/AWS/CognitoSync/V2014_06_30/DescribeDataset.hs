{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CognitoSync.V2014_06_30.DescribeDataset
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Gets metadata about a dataset by identity and dataset name.
module Network.AWS.CognitoSync.V2014_06_30.DescribeDataset where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.CognitoSync.V2014_06_30.Types
import Network.AWS.Prelude
import qualified Network.AWS.Types.Map as Map

data DescribeDataset = DescribeDataset
    { _ddrDatasetName :: Text
      -- ^ A string of up to 128 characters. Allowed characters are a-z,
      -- A-Z, 0-9, '_' (underscore), '-' (dash), and '.' (dot).
    , _ddrIdentityId :: Text
      -- ^ A name-spaced GUID (for example,
      -- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
      -- Cognito. GUID generation is unique within a region.
    , _ddrIdentityPoolId :: Text
      -- ^ A name-spaced GUID (for example,
      -- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
      -- Cognito. GUID generation is unique within a region.
    } deriving (Show, Generic)

makeLenses ''DescribeDataset

instance ToPath DescribeDataset where
    toPath DescribeDataset{..} = mconcat
        [ "/identitypools/"
        , toBS _ddrIdentityPoolId
        , "/identities/"
        , toBS _ddrIdentityId
        , "/datasets/"
        , toBS _ddrDatasetName
        ]

instance ToQuery DescribeDataset

instance ToHeaders DescribeDataset

instance ToJSON DescribeDataset

data DescribeDatasetResponse = DescribeDatasetResponse
    { _ddsDataset :: Maybe Dataset
      -- ^ Metadata for a collection of data for an identity. An identity
      -- can have multiple datasets. A dataset can be general or
      -- associated with a particular entity in an application (like a
      -- saved game). Datasets are automatically created if they don't
      -- exist. Data is synced by dataset, and a dataset can hold up to
      -- 1MB of key-value pairs.
    } deriving (Show, Generic)

makeLenses ''DescribeDatasetResponse

instance FromJSON DescribeDatasetResponse

instance AWSRequest DescribeDataset where
    type Sv DescribeDataset = CognitoSync
    type Rs DescribeDataset = DescribeDatasetResponse

    request = get
    response _ = jsonResponse
