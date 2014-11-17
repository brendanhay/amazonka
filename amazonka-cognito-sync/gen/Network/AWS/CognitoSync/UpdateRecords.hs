{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CognitoSync.UpdateRecords
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Posts updates to records and add and delete records for a dataset and user.
-- The credentials used to make this API call need to have access to the
-- identity data. With Amazon Cognito Sync, each identity has access only to
-- its own data. You should use Amazon Cognito Identity service to retrieve
-- the credentials necessary to make this API call.
module Network.AWS.CognitoSync.UpdateRecords
    (
    -- * Request
      UpdateRecords
    -- ** Request constructor
    , updateRecords
    -- ** Request lenses
    , urClientContext
    , urDatasetName
    , urDeviceId
    , urIdentityId
    , urIdentityPoolId
    , urRecordPatches
    , urSyncSessionToken

    -- * Response
    , UpdateRecordsResponse
    -- ** Response constructor
    , updateRecordsResponse
    -- ** Response lenses
    , urrRecords
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CognitoSync.Types
import qualified GHC.Exts

data UpdateRecords = UpdateRecords
    { _urClientContext    :: Maybe Text
    , _urDatasetName      :: Text
    , _urDeviceId         :: Maybe Text
    , _urIdentityId       :: Text
    , _urIdentityPoolId   :: Text
    , _urRecordPatches    :: [RecordPatch]
    , _urSyncSessionToken :: Text
    } deriving (Eq, Show, Generic)

-- | 'UpdateRecords' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'urClientContext' @::@ 'Maybe' 'Text'
--
-- * 'urDatasetName' @::@ 'Text'
--
-- * 'urDeviceId' @::@ 'Maybe' 'Text'
--
-- * 'urIdentityId' @::@ 'Text'
--
-- * 'urIdentityPoolId' @::@ 'Text'
--
-- * 'urRecordPatches' @::@ ['RecordPatch']
--
-- * 'urSyncSessionToken' @::@ 'Text'
--
updateRecords :: Text -- ^ 'urIdentityPoolId'
              -> Text -- ^ 'urIdentityId'
              -> Text -- ^ 'urDatasetName'
              -> Text -- ^ 'urSyncSessionToken'
              -> UpdateRecords
updateRecords p1 p2 p3 p4 = UpdateRecords
    { _urIdentityPoolId   = p1
    , _urIdentityId       = p2
    , _urDatasetName      = p3
    , _urSyncSessionToken = p4
    , _urDeviceId         = Nothing
    , _urRecordPatches    = mempty
    , _urClientContext    = Nothing
    }

-- | Intended to supply a device ID that will populate the lastModifiedBy
-- field referenced in other methods. The ClientContext field is not yet
-- implemented.
urClientContext :: Lens' UpdateRecords (Maybe Text)
urClientContext = lens _urClientContext (\s a -> s { _urClientContext = a })

-- | A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9,
-- '_' (underscore), '-' (dash), and '.' (dot).
urDatasetName :: Lens' UpdateRecords Text
urDatasetName = lens _urDatasetName (\s a -> s { _urDatasetName = a })

-- | The unique ID generated for this device by Cognito.
urDeviceId :: Lens' UpdateRecords (Maybe Text)
urDeviceId = lens _urDeviceId (\s a -> s { _urDeviceId = a })

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
urIdentityId :: Lens' UpdateRecords Text
urIdentityId = lens _urIdentityId (\s a -> s { _urIdentityId = a })

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
urIdentityPoolId :: Lens' UpdateRecords Text
urIdentityPoolId = lens _urIdentityPoolId (\s a -> s { _urIdentityPoolId = a })

-- | A list of patch operations.
urRecordPatches :: Lens' UpdateRecords [RecordPatch]
urRecordPatches = lens _urRecordPatches (\s a -> s { _urRecordPatches = a })

-- | The SyncSessionToken returned by a previous call to ListRecords for this
-- dataset and identity.
urSyncSessionToken :: Lens' UpdateRecords Text
urSyncSessionToken =
    lens _urSyncSessionToken (\s a -> s { _urSyncSessionToken = a })

newtype UpdateRecordsResponse = UpdateRecordsResponse
    { _urrRecords :: [Record]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList UpdateRecordsResponse where
    type Item UpdateRecordsResponse = Record

    fromList = UpdateRecordsResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _urrRecords

-- | 'UpdateRecordsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'urrRecords' @::@ ['Record']
--
updateRecordsResponse :: UpdateRecordsResponse
updateRecordsResponse = UpdateRecordsResponse
    { _urrRecords = mempty
    }

-- | A list of records that have been updated.
urrRecords :: Lens' UpdateRecordsResponse [Record]
urrRecords = lens _urrRecords (\s a -> s { _urrRecords = a })

instance AWSRequest UpdateRecords where
    type Sv UpdateRecords = CognitoSync
    type Rs UpdateRecords = UpdateRecordsResponse

    request  = post
    response = jsonResponse

instance FromJSON UpdateRecordsResponse where
    parseJSON = genericParseJSON jsonOptions

instance ToPath UpdateRecords where
    toPath UpdateRecords{..} = mconcat
        [ "/identitypools/"
        , toText _urIdentityPoolId
        , "/identities/"
        , toText _urIdentityId
        , "/datasets/"
        , toText _urDatasetName
        ]

instance ToHeaders UpdateRecords where
    toHeaders UpdateRecords{..} = mconcat
        [ "x-amz-Client-Context" =: _urClientContext
        ]

instance ToQuery UpdateRecords where
    toQuery = const mempty

instance ToJSON UpdateRecords where
    toJSON = genericToJSON jsonOptions
