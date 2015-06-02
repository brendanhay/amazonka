{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.ImportSnapshot
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Imports a disk into an EBS snapshot.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ImportSnapshot.html>
module Network.AWS.EC2.ImportSnapshot
    (
    -- * Request
      ImportSnapshot
    -- ** Request constructor
    , importSnapshot
    -- ** Request lenses
    , isClientData
    , isClientToken
    , isDescription
    , isDiskContainer
    , isDryRun
    , isRoleName

    -- * Response
    , ImportSnapshotResponse
    -- ** Response constructor
    , importSnapshotResponse
    -- ** Response lenses
    , isrDescription
    , isrImportTaskId
    , isrSnapshotTaskDetail
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data ImportSnapshot = ImportSnapshot
    { _isClientData    :: Maybe ClientData
    , _isClientToken   :: Maybe Text
    , _isDescription   :: Maybe Text
    , _isDiskContainer :: Maybe SnapshotDiskContainer
    , _isDryRun        :: Maybe Bool
    , _isRoleName      :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'ImportSnapshot' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'isClientData' @::@ 'Maybe' 'ClientData'
--
-- * 'isClientToken' @::@ 'Maybe' 'Text'
--
-- * 'isDescription' @::@ 'Maybe' 'Text'
--
-- * 'isDiskContainer' @::@ 'Maybe' 'SnapshotDiskContainer'
--
-- * 'isDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'isRoleName' @::@ 'Maybe' 'Text'
--
importSnapshot :: ImportSnapshot
importSnapshot = ImportSnapshot
    { _isDryRun        = Nothing
    , _isDescription   = Nothing
    , _isDiskContainer = Nothing
    , _isClientData    = Nothing
    , _isClientToken   = Nothing
    , _isRoleName      = Nothing
    }

-- | The client-specific data.
isClientData :: Lens' ImportSnapshot (Maybe ClientData)
isClientData = lens _isClientData (\s a -> s { _isClientData = a })

-- | Token to enable idempotency for VM import requests.
isClientToken :: Lens' ImportSnapshot (Maybe Text)
isClientToken = lens _isClientToken (\s a -> s { _isClientToken = a })

-- | The description string for the import snapshot task.
isDescription :: Lens' ImportSnapshot (Maybe Text)
isDescription = lens _isDescription (\s a -> s { _isDescription = a })

-- | Information about the disk container.
isDiskContainer :: Lens' ImportSnapshot (Maybe SnapshotDiskContainer)
isDiskContainer = lens _isDiskContainer (\s a -> s { _isDiskContainer = a })

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have the
-- required permissions, the error response is 'DryRunOperation'. Otherwise, it is 'UnauthorizedOperation'.
isDryRun :: Lens' ImportSnapshot (Maybe Bool)
isDryRun = lens _isDryRun (\s a -> s { _isDryRun = a })

-- | The name of the role to use when not using the default role, 'vmimport'.
isRoleName :: Lens' ImportSnapshot (Maybe Text)
isRoleName = lens _isRoleName (\s a -> s { _isRoleName = a })

data ImportSnapshotResponse = ImportSnapshotResponse
    { _isrDescription        :: Maybe Text
    , _isrImportTaskId       :: Maybe Text
    , _isrSnapshotTaskDetail :: Maybe SnapshotTaskDetail
    } deriving (Eq, Read, Show)

-- | 'ImportSnapshotResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'isrDescription' @::@ 'Maybe' 'Text'
--
-- * 'isrImportTaskId' @::@ 'Maybe' 'Text'
--
-- * 'isrSnapshotTaskDetail' @::@ 'Maybe' 'SnapshotTaskDetail'
--
importSnapshotResponse :: ImportSnapshotResponse
importSnapshotResponse = ImportSnapshotResponse
    { _isrImportTaskId       = Nothing
    , _isrSnapshotTaskDetail = Nothing
    , _isrDescription        = Nothing
    }

-- | A description of the import snapshot task.
isrDescription :: Lens' ImportSnapshotResponse (Maybe Text)
isrDescription = lens _isrDescription (\s a -> s { _isrDescription = a })

-- | The ID of the import snapshot task.
isrImportTaskId :: Lens' ImportSnapshotResponse (Maybe Text)
isrImportTaskId = lens _isrImportTaskId (\s a -> s { _isrImportTaskId = a })

-- | Information about the import snapshot task.
isrSnapshotTaskDetail :: Lens' ImportSnapshotResponse (Maybe SnapshotTaskDetail)
isrSnapshotTaskDetail =
    lens _isrSnapshotTaskDetail (\s a -> s { _isrSnapshotTaskDetail = a })

instance ToPath ImportSnapshot where
    toPath = const "/"

instance ToQuery ImportSnapshot where
    toQuery ImportSnapshot{..} = mconcat
        [ "ClientData"    =? _isClientData
        , "ClientToken"   =? _isClientToken
        , "Description"   =? _isDescription
        , "DiskContainer" =? _isDiskContainer
        , "DryRun"        =? _isDryRun
        , "RoleName"      =? _isRoleName
        ]

instance ToHeaders ImportSnapshot

instance AWSRequest ImportSnapshot where
    type Sv ImportSnapshot = EC2
    type Rs ImportSnapshot = ImportSnapshotResponse

    request  = post "ImportSnapshot"
    response = xmlResponse

instance FromXML ImportSnapshotResponse where
    parseXML x = ImportSnapshotResponse
        <$> x .@? "description"
        <*> x .@? "importTaskId"
        <*> x .@? "snapshotTaskDetail"
