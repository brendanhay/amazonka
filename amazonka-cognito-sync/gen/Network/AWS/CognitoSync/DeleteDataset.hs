{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CognitoSync.DeleteDataset
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specific dataset. The dataset will be deleted permanently, and
-- the action can't be undone. Datasets that this dataset was merged with will
-- no longer report the merge. Any consequent operation on this dataset will
-- result in a ResourceNotFoundException.
--
-- <http://docs.aws.amazon.com/cognitosync/latest/APIReference/API_DeleteDataset.html>
module Network.AWS.CognitoSync.DeleteDataset
    (
    -- * Request
      DeleteDataset
    -- ** Request constructor
    , deleteDataset
    -- ** Request lenses
    , dd1DatasetName
    , dd1IdentityId
    , dd1IdentityPoolId

    -- * Response
    , DeleteDatasetResponse
    -- ** Response constructor
    , deleteDatasetResponse
    -- ** Response lenses
    , ddr1Dataset
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestJSON
import Network.AWS.CognitoSync.Types
import qualified GHC.Exts

data DeleteDataset = DeleteDataset
    { _dd1DatasetName    :: Text
    , _dd1IdentityId     :: Text
    , _dd1IdentityPoolId :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DeleteDataset' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dd1DatasetName' @::@ 'Text'
--
-- * 'dd1IdentityId' @::@ 'Text'
--
-- * 'dd1IdentityPoolId' @::@ 'Text'
--
deleteDataset :: Text -- ^ 'dd1IdentityPoolId'
              -> Text -- ^ 'dd1IdentityId'
              -> Text -- ^ 'dd1DatasetName'
              -> DeleteDataset
deleteDataset p1 p2 p3 = DeleteDataset
    { _dd1IdentityPoolId = p1
    , _dd1IdentityId     = p2
    , _dd1DatasetName    = p3
    }

-- | A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9,
-- '_' (underscore), '-' (dash), and '.' (dot).
dd1DatasetName :: Lens' DeleteDataset Text
dd1DatasetName = lens _dd1DatasetName (\s a -> s { _dd1DatasetName = a })

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
dd1IdentityId :: Lens' DeleteDataset Text
dd1IdentityId = lens _dd1IdentityId (\s a -> s { _dd1IdentityId = a })

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
dd1IdentityPoolId :: Lens' DeleteDataset Text
dd1IdentityPoolId =
    lens _dd1IdentityPoolId (\s a -> s { _dd1IdentityPoolId = a })

newtype DeleteDatasetResponse = DeleteDatasetResponse
    { _ddr1Dataset :: Maybe Dataset
    } deriving (Eq, Show, Generic)

-- | 'DeleteDatasetResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddr1Dataset' @::@ 'Maybe' 'Dataset'
--
deleteDatasetResponse :: DeleteDatasetResponse
deleteDatasetResponse = DeleteDatasetResponse
    { _ddr1Dataset = Nothing
    }

-- | A collection of data for an identity pool. An identity pool can have
-- multiple datasets. A dataset is per identity and can be general or
-- associated with a particular entity in an application (like a saved
-- game). Datasets are automatically created if they don't exist. Data is
-- synced by dataset, and a dataset can hold up to 1MB of key-value pairs.
ddr1Dataset :: Lens' DeleteDatasetResponse (Maybe Dataset)
ddr1Dataset = lens _ddr1Dataset (\s a -> s { _ddr1Dataset = a })

instance ToPath DeleteDataset where
    toPath DeleteDataset{..} = mconcat
        [ "/identitypools/"
        , toText _dd1IdentityPoolId
        , "/identities/"
        , toText _dd1IdentityId
        , "/datasets/"
        , toText _dd1DatasetName
        ]

instance ToQuery DeleteDataset where
    toQuery = const mempty

instance ToHeaders DeleteDataset

instance ToJSON DeleteDataset where
    toJSON = const Null

instance AWSRequest DeleteDataset where
    type Sv DeleteDataset = CognitoSync
    type Rs DeleteDataset = DeleteDatasetResponse

    request  = delete
    response = jsonResponse

instance FromJSON DeleteDatasetResponse where
    parseJSON = withObject "DeleteDatasetResponse" $ \o -> DeleteDatasetResponse
        <$> o .:? "Dataset"
