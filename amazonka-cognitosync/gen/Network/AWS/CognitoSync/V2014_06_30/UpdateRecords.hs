{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CognitoSync.V2014_06_30.UpdateRecords
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Posts updates to records and add and delete records for a dataset and user.
module Network.AWS.CognitoSync.V2014_06_30.UpdateRecords where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.CognitoSync.V2014_06_30.Types
import Network.AWS.Prelude
import qualified Network.AWS.Types.Map as Map

-- | Minimum specification for a 'UpdateRecords' request.
updateRecords :: Text -- ^ '_urrSyncSessionToken'
              -> Text -- ^ '_urrDatasetName'
              -> Text -- ^ '_urrIdentityId'
              -> Text -- ^ '_urrIdentityPoolId'
              -> UpdateRecords
updateRecords p1 p2 p3 p4 = UpdateRecords
    { _urrSyncSessionToken = p1
    , _urrDatasetName = p2
    , _urrIdentityId = p3
    , _urrIdentityPoolId = p4
    , _urrRecordPatches = mempty
    , _urrClientContext = Nothing
    }

data UpdateRecords = UpdateRecords
    { _urrSyncSessionToken :: Text
      -- ^ The SyncSessionToken returned by a previous call to ListRecords
      -- for this dataset and identity.
    , _urrDatasetName :: Text
      -- ^ A string of up to 128 characters. Allowed characters are a-z,
      -- A-Z, 0-9, '_' (underscore), '-' (dash), and '.' (dot).
    , _urrIdentityId :: Text
      -- ^ A name-spaced GUID (for example,
      -- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
      -- Cognito. GUID generation is unique within a region.
    , _urrIdentityPoolId :: Text
      -- ^ A name-spaced GUID (for example,
      -- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
      -- Cognito. GUID generation is unique within a region.
    , _urrRecordPatches :: [RecordPatch]
      -- ^ 
    , _urrClientContext :: Maybe Text
    } deriving (Show, Generic)

makeLenses ''UpdateRecords

instance ToPath UpdateRecords where
    toPath UpdateRecords{..} = mconcat
        [ "/identitypools/"
        , toBS _urrIdentityPoolId
        , "/identities/"
        , toBS _urrIdentityId
        , "/datasets/"
        , toBS _urrDatasetName
        ]

instance ToQuery UpdateRecords

instance ToHeaders UpdateRecords where
    toHeaders UpdateRecords{..} = concat
        [ "x-amz-Client-Context" =: _urrClientContext
        ]

instance ToJSON UpdateRecords

data UpdateRecordsResponse = UpdateRecordsResponse
    { _ursRecords :: [Record]
      -- ^ A list of records that have been updated.
    } deriving (Show, Generic)

makeLenses ''UpdateRecordsResponse

instance FromJSON UpdateRecordsResponse

instance AWSRequest UpdateRecords where
    type Sv UpdateRecords = CognitoSync
    type Rs UpdateRecords = UpdateRecordsResponse

    request = post
    response _ = jsonResponse
