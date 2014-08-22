{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CognitoSync.V2014_06_30.DeleteDataset
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
module Network.AWS.CognitoSync.V2014_06_30.DeleteDataset where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.CognitoSync.V2014_06_30.Types
import Network.AWS.Prelude
import qualified Network.AWS.Types.Map as Map

data DeleteDataset = DeleteDataset
    { _ddtDatasetName :: Text
      -- ^ A string of up to 128 characters. Allowed characters are a-z,
      -- A-Z, 0-9, '_' (underscore), '-' (dash), and '.' (dot).
    , _ddtIdentityId :: Text
      -- ^ A name-spaced GUID (for example,
      -- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
      -- Cognito. GUID generation is unique within a region.
    , _ddtIdentityPoolId :: Text
      -- ^ A name-spaced GUID (for example,
      -- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
      -- Cognito. GUID generation is unique within a region.
    } deriving (Show, Generic)

makeLenses ''DeleteDataset

instance ToPath DeleteDataset where
    toPath DeleteDataset{..} = mconcat
        [ "/identitypools/"
        , toBS _ddtIdentityPoolId
        , "/identities/"
        , toBS _ddtIdentityId
        , "/datasets/"
        , toBS _ddtDatasetName
        ]

instance ToQuery DeleteDataset

instance ToHeaders DeleteDataset

instance ToJSON DeleteDataset

data DeleteDatasetResponse = DeleteDatasetResponse
    { _dduDataset :: Maybe Dataset
      -- ^ A collection of data for an identity pool. An identity pool can
      -- have multiple datasets. A dataset is per identity and can be
      -- general or associated with a particular entity in an application
      -- (like a saved game). Datasets are automatically created if they
      -- don't exist. Data is synced by dataset, and a dataset can hold up
      -- to 1MB of key-value pairs.
    } deriving (Show, Generic)

makeLenses ''DeleteDatasetResponse

instance FromJSON DeleteDatasetResponse

instance AWSRequest DeleteDataset where
    type Sv DeleteDataset = CognitoSync
    type Rs DeleteDataset = DeleteDatasetResponse

    request = delete
    response _ = jsonResponse
