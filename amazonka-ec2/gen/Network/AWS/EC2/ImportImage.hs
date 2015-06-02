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

-- Module      : Network.AWS.EC2.ImportImage
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

-- | Import single or multi-volume disk images or EBS snapshots into an Amazon
-- Machine Image (AMI).
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ImportImage.html>
module Network.AWS.EC2.ImportImage
    (
    -- * Request
      ImportImage
    -- ** Request constructor
    , importImage
    -- ** Request lenses
    , ii1Architecture
    , ii1ClientData
    , ii1ClientToken
    , ii1Description
    , ii1DiskContainers
    , ii1DryRun
    , ii1Hypervisor
    , ii1LicenseType
    , ii1Platform
    , ii1RoleName

    -- * Response
    , ImportImageResponse
    -- ** Response constructor
    , importImageResponse
    -- ** Response lenses
    , iirArchitecture
    , iirDescription
    , iirHypervisor
    , iirImageId
    , iirImportTaskId
    , iirLicenseType
    , iirPlatform
    , iirProgress
    , iirSnapshotDetails
    , iirStatus
    , iirStatusMessage
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data ImportImage = ImportImage
    { _ii1Architecture   :: Maybe Text
    , _ii1ClientData     :: Maybe ClientData
    , _ii1ClientToken    :: Maybe Text
    , _ii1Description    :: Maybe Text
    , _ii1DiskContainers :: List "item" ImageDiskContainer
    , _ii1DryRun         :: Maybe Bool
    , _ii1Hypervisor     :: Maybe Text
    , _ii1LicenseType    :: Maybe Text
    , _ii1Platform       :: Maybe Text
    , _ii1RoleName       :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'ImportImage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ii1Architecture' @::@ 'Maybe' 'Text'
--
-- * 'ii1ClientData' @::@ 'Maybe' 'ClientData'
--
-- * 'ii1ClientToken' @::@ 'Maybe' 'Text'
--
-- * 'ii1Description' @::@ 'Maybe' 'Text'
--
-- * 'ii1DiskContainers' @::@ ['ImageDiskContainer']
--
-- * 'ii1DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'ii1Hypervisor' @::@ 'Maybe' 'Text'
--
-- * 'ii1LicenseType' @::@ 'Maybe' 'Text'
--
-- * 'ii1Platform' @::@ 'Maybe' 'Text'
--
-- * 'ii1RoleName' @::@ 'Maybe' 'Text'
--
importImage :: ImportImage
importImage = ImportImage
    { _ii1DryRun         = Nothing
    , _ii1Description    = Nothing
    , _ii1DiskContainers = mempty
    , _ii1LicenseType    = Nothing
    , _ii1Hypervisor     = Nothing
    , _ii1Architecture   = Nothing
    , _ii1Platform       = Nothing
    , _ii1ClientData     = Nothing
    , _ii1ClientToken    = Nothing
    , _ii1RoleName       = Nothing
    }

-- | The architecture of the virtual machine.
--
-- Valid values: 'i386' | 'x86_64'
ii1Architecture :: Lens' ImportImage (Maybe Text)
ii1Architecture = lens _ii1Architecture (\s a -> s { _ii1Architecture = a })

-- | The client-specific data.
ii1ClientData :: Lens' ImportImage (Maybe ClientData)
ii1ClientData = lens _ii1ClientData (\s a -> s { _ii1ClientData = a })

-- | The token to enable idempotency for VM import requests.
ii1ClientToken :: Lens' ImportImage (Maybe Text)
ii1ClientToken = lens _ii1ClientToken (\s a -> s { _ii1ClientToken = a })

-- | A description string for the import image task.
ii1Description :: Lens' ImportImage (Maybe Text)
ii1Description = lens _ii1Description (\s a -> s { _ii1Description = a })

-- | Information about the disk containers.
ii1DiskContainers :: Lens' ImportImage [ImageDiskContainer]
ii1DiskContainers =
    lens _ii1DiskContainers (\s a -> s { _ii1DiskContainers = a })
        . _List

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have the
-- required permissions, the error response is 'DryRunOperation'. Otherwise, it is 'UnauthorizedOperation'.
ii1DryRun :: Lens' ImportImage (Maybe Bool)
ii1DryRun = lens _ii1DryRun (\s a -> s { _ii1DryRun = a })

-- | The target hypervisor platform.
--
-- Valid values: 'xen'
ii1Hypervisor :: Lens' ImportImage (Maybe Text)
ii1Hypervisor = lens _ii1Hypervisor (\s a -> s { _ii1Hypervisor = a })

-- | The license type to be used for the Amazon Machine Image (AMI) after
-- importing.
--
-- Note: You may only use BYOL if you have existing licenses with rights to use
-- these licenses in a third party cloud like AWS. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/VMImportPrerequisites.html VMImport/Export Prerequisites> in the /Amazon Elastic Compute Cloud User Guide/.
--
-- Valid values: 'AWS' | 'BYOL'
ii1LicenseType :: Lens' ImportImage (Maybe Text)
ii1LicenseType = lens _ii1LicenseType (\s a -> s { _ii1LicenseType = a })

-- | The operating system of the virtual machine.
--
-- Valid values: 'Windows' | 'Linux'
ii1Platform :: Lens' ImportImage (Maybe Text)
ii1Platform = lens _ii1Platform (\s a -> s { _ii1Platform = a })

-- | The name of the role to use when not using the default role, 'vmimport'.
ii1RoleName :: Lens' ImportImage (Maybe Text)
ii1RoleName = lens _ii1RoleName (\s a -> s { _ii1RoleName = a })

data ImportImageResponse = ImportImageResponse
    { _iirArchitecture    :: Maybe Text
    , _iirDescription     :: Maybe Text
    , _iirHypervisor      :: Maybe Text
    , _iirImageId         :: Maybe Text
    , _iirImportTaskId    :: Maybe Text
    , _iirLicenseType     :: Maybe Text
    , _iirPlatform        :: Maybe Text
    , _iirProgress        :: Maybe Text
    , _iirSnapshotDetails :: List "item" SnapshotDetail
    , _iirStatus          :: Maybe Text
    , _iirStatusMessage   :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'ImportImageResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iirArchitecture' @::@ 'Maybe' 'Text'
--
-- * 'iirDescription' @::@ 'Maybe' 'Text'
--
-- * 'iirHypervisor' @::@ 'Maybe' 'Text'
--
-- * 'iirImageId' @::@ 'Maybe' 'Text'
--
-- * 'iirImportTaskId' @::@ 'Maybe' 'Text'
--
-- * 'iirLicenseType' @::@ 'Maybe' 'Text'
--
-- * 'iirPlatform' @::@ 'Maybe' 'Text'
--
-- * 'iirProgress' @::@ 'Maybe' 'Text'
--
-- * 'iirSnapshotDetails' @::@ ['SnapshotDetail']
--
-- * 'iirStatus' @::@ 'Maybe' 'Text'
--
-- * 'iirStatusMessage' @::@ 'Maybe' 'Text'
--
importImageResponse :: ImportImageResponse
importImageResponse = ImportImageResponse
    { _iirImportTaskId    = Nothing
    , _iirArchitecture    = Nothing
    , _iirLicenseType     = Nothing
    , _iirPlatform        = Nothing
    , _iirHypervisor      = Nothing
    , _iirDescription     = Nothing
    , _iirSnapshotDetails = mempty
    , _iirImageId         = Nothing
    , _iirProgress        = Nothing
    , _iirStatusMessage   = Nothing
    , _iirStatus          = Nothing
    }

-- | The architecture of the virtual machine.
iirArchitecture :: Lens' ImportImageResponse (Maybe Text)
iirArchitecture = lens _iirArchitecture (\s a -> s { _iirArchitecture = a })

-- | A description of the import task.
iirDescription :: Lens' ImportImageResponse (Maybe Text)
iirDescription = lens _iirDescription (\s a -> s { _iirDescription = a })

-- | The target hypervisor of the import task.
iirHypervisor :: Lens' ImportImageResponse (Maybe Text)
iirHypervisor = lens _iirHypervisor (\s a -> s { _iirHypervisor = a })

-- | The ID of the Amazon Machine Image (AMI) created by the import task.
iirImageId :: Lens' ImportImageResponse (Maybe Text)
iirImageId = lens _iirImageId (\s a -> s { _iirImageId = a })

-- | The task ID of the import image task.
iirImportTaskId :: Lens' ImportImageResponse (Maybe Text)
iirImportTaskId = lens _iirImportTaskId (\s a -> s { _iirImportTaskId = a })

-- | The license type of the virtual machine.
iirLicenseType :: Lens' ImportImageResponse (Maybe Text)
iirLicenseType = lens _iirLicenseType (\s a -> s { _iirLicenseType = a })

-- | The operating system of the virtual machine.
iirPlatform :: Lens' ImportImageResponse (Maybe Text)
iirPlatform = lens _iirPlatform (\s a -> s { _iirPlatform = a })

-- | The progress of the task.
iirProgress :: Lens' ImportImageResponse (Maybe Text)
iirProgress = lens _iirProgress (\s a -> s { _iirProgress = a })

-- | Information about the snapshots.
iirSnapshotDetails :: Lens' ImportImageResponse [SnapshotDetail]
iirSnapshotDetails =
    lens _iirSnapshotDetails (\s a -> s { _iirSnapshotDetails = a })
        . _List

-- | A brief status of the task.
iirStatus :: Lens' ImportImageResponse (Maybe Text)
iirStatus = lens _iirStatus (\s a -> s { _iirStatus = a })

-- | A detailed status message of the import task.
iirStatusMessage :: Lens' ImportImageResponse (Maybe Text)
iirStatusMessage = lens _iirStatusMessage (\s a -> s { _iirStatusMessage = a })

instance ToPath ImportImage where
    toPath = const "/"

instance ToQuery ImportImage where
    toQuery ImportImage{..} = mconcat
        [ "Architecture"  =? _ii1Architecture
        , "ClientData"    =? _ii1ClientData
        , "ClientToken"   =? _ii1ClientToken
        , "Description"   =? _ii1Description
        , "DiskContainer" `toQueryList` _ii1DiskContainers
        , "DryRun"        =? _ii1DryRun
        , "Hypervisor"    =? _ii1Hypervisor
        , "LicenseType"   =? _ii1LicenseType
        , "Platform"      =? _ii1Platform
        , "RoleName"      =? _ii1RoleName
        ]

instance ToHeaders ImportImage

instance AWSRequest ImportImage where
    type Sv ImportImage = EC2
    type Rs ImportImage = ImportImageResponse

    request  = post "ImportImage"
    response = xmlResponse

instance FromXML ImportImageResponse where
    parseXML x = ImportImageResponse
        <$> x .@? "architecture"
        <*> x .@? "description"
        <*> x .@? "hypervisor"
        <*> x .@? "imageId"
        <*> x .@? "importTaskId"
        <*> x .@? "licenseType"
        <*> x .@? "platform"
        <*> x .@? "progress"
        <*> x .@? "snapshotDetailSet" .!@ mempty
        <*> x .@? "status"
        <*> x .@? "statusMessage"
