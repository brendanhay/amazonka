{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.EC2.ImportImage
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Import single or multi-volume disk images or EBS snapshots into an
-- Amazon Machine Image (AMI).
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ImportImage.html>
module Network.AWS.EC2.ImportImage
    (
    -- * Request
      ImportImage
    -- ** Request constructor
    , importImage
    -- ** Request lenses
    , impHypervisor
    , impPlatform
    , impClientToken
    , impLicenseType
    , impRoleName
    , impArchitecture
    , impDryRun
    , impDescription
    , impClientData
    , impDiskContainers

    -- * Response
    , ImportImageResponse
    -- ** Response constructor
    , importImageResponse
    -- ** Response lenses
    , iirHypervisor
    , iirPlatform
    , iirProgress
    , iirLicenseType
    , iirSnapshotDetails
    , iirStatusMessage
    , iirAddressStatus
    , iirImageId
    , iirImportTaskId
    , iirArchitecture
    , iirDescription
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.EC2.Types

-- | /See:/ 'importImage' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'impHypervisor'
--
-- * 'impPlatform'
--
-- * 'impClientToken'
--
-- * 'impLicenseType'
--
-- * 'impRoleName'
--
-- * 'impArchitecture'
--
-- * 'impDryRun'
--
-- * 'impDescription'
--
-- * 'impClientData'
--
-- * 'impDiskContainers'
data ImportImage = ImportImage'{_impHypervisor :: Maybe Text, _impPlatform :: Maybe Text, _impClientToken :: Maybe Text, _impLicenseType :: Maybe Text, _impRoleName :: Maybe Text, _impArchitecture :: Maybe Text, _impDryRun :: Maybe Bool, _impDescription :: Maybe Text, _impClientData :: Maybe ClientData, _impDiskContainers :: [ImageDiskContainer]} deriving (Eq, Read, Show)

-- | 'ImportImage' smart constructor.
importImage :: ImportImage
importImage = ImportImage'{_impHypervisor = Nothing, _impPlatform = Nothing, _impClientToken = Nothing, _impLicenseType = Nothing, _impRoleName = Nothing, _impArchitecture = Nothing, _impDryRun = Nothing, _impDescription = Nothing, _impClientData = Nothing, _impDiskContainers = mempty};

-- | The target hypervisor platform.
--
-- Valid values: @xen@
impHypervisor :: Lens' ImportImage (Maybe Text)
impHypervisor = lens _impHypervisor (\ s a -> s{_impHypervisor = a});

-- | The operating system of the virtual machine.
--
-- Valid values: @Windows@ | @Linux@
impPlatform :: Lens' ImportImage (Maybe Text)
impPlatform = lens _impPlatform (\ s a -> s{_impPlatform = a});

-- | The token to enable idempotency for VM import requests.
impClientToken :: Lens' ImportImage (Maybe Text)
impClientToken = lens _impClientToken (\ s a -> s{_impClientToken = a});

-- | The license type to be used for the Amazon Machine Image (AMI) after
-- importing.
--
-- __Note:__ You may only use BYOL if you have existing licenses with
-- rights to use these licenses in a third party cloud like AWS. For more
-- information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/VMImportPrerequisites.html VM Import\/Export Prerequisites>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- Valid values: @AWS@ | @BYOL@
impLicenseType :: Lens' ImportImage (Maybe Text)
impLicenseType = lens _impLicenseType (\ s a -> s{_impLicenseType = a});

-- | The name of the role to use when not using the default role,
-- \'vmimport\'.
impRoleName :: Lens' ImportImage (Maybe Text)
impRoleName = lens _impRoleName (\ s a -> s{_impRoleName = a});

-- | The architecture of the virtual machine.
--
-- Valid values: @i386@ | @x86_64@
impArchitecture :: Lens' ImportImage (Maybe Text)
impArchitecture = lens _impArchitecture (\ s a -> s{_impArchitecture = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
impDryRun :: Lens' ImportImage (Maybe Bool)
impDryRun = lens _impDryRun (\ s a -> s{_impDryRun = a});

-- | A description string for the import image task.
impDescription :: Lens' ImportImage (Maybe Text)
impDescription = lens _impDescription (\ s a -> s{_impDescription = a});

-- | The client-specific data.
impClientData :: Lens' ImportImage (Maybe ClientData)
impClientData = lens _impClientData (\ s a -> s{_impClientData = a});

-- | Information about the disk containers.
impDiskContainers :: Lens' ImportImage [ImageDiskContainer]
impDiskContainers = lens _impDiskContainers (\ s a -> s{_impDiskContainers = a});

instance AWSRequest ImportImage where
        type Sv ImportImage = EC2
        type Rs ImportImage = ImportImageResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 ImportImageResponse' <$>
                   x .@? "hypervisor" <*> x .@? "platform" <*>
                     x .@? "progress"
                     <*> x .@? "licenseType"
                     <*> parseXMLList "item" x
                     <*> x .@? "statusMessage"
                     <*> x .@? "status"
                     <*> x .@? "imageId"
                     <*> x .@? "importTaskId"
                     <*> x .@? "architecture"
                     <*> x .@? "description")

instance ToHeaders ImportImage where
        toHeaders = const mempty

instance ToPath ImportImage where
        toPath = const "/"

instance ToQuery ImportImage where
        toQuery ImportImage'{..}
          = mconcat
              ["Action" =: ("ImportImage" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "Hypervisor" =: _impHypervisor,
               "Platform" =: _impPlatform,
               "ClientToken" =: _impClientToken,
               "LicenseType" =: _impLicenseType,
               "RoleName" =: _impRoleName,
               "Architecture" =: _impArchitecture,
               "DryRun" =: _impDryRun,
               "Description" =: _impDescription,
               "ClientData" =: _impClientData,
               "item" =: _impDiskContainers]

-- | /See:/ 'importImageResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iirHypervisor'
--
-- * 'iirPlatform'
--
-- * 'iirProgress'
--
-- * 'iirLicenseType'
--
-- * 'iirSnapshotDetails'
--
-- * 'iirStatusMessage'
--
-- * 'iirAddressStatus'
--
-- * 'iirImageId'
--
-- * 'iirImportTaskId'
--
-- * 'iirArchitecture'
--
-- * 'iirDescription'
data ImportImageResponse = ImportImageResponse'{_iirHypervisor :: Maybe Text, _iirPlatform :: Maybe Text, _iirProgress :: Maybe Text, _iirLicenseType :: Maybe Text, _iirSnapshotDetails :: [SnapshotDetail], _iirStatusMessage :: Maybe Text, _iirAddressStatus :: Maybe Text, _iirImageId :: Maybe Text, _iirImportTaskId :: Maybe Text, _iirArchitecture :: Maybe Text, _iirDescription :: Maybe Text} deriving (Eq, Read, Show)

-- | 'ImportImageResponse' smart constructor.
importImageResponse :: ImportImageResponse
importImageResponse = ImportImageResponse'{_iirHypervisor = Nothing, _iirPlatform = Nothing, _iirProgress = Nothing, _iirLicenseType = Nothing, _iirSnapshotDetails = mempty, _iirStatusMessage = Nothing, _iirAddressStatus = Nothing, _iirImageId = Nothing, _iirImportTaskId = Nothing, _iirArchitecture = Nothing, _iirDescription = Nothing};

-- | The target hypervisor of the import task.
iirHypervisor :: Lens' ImportImageResponse (Maybe Text)
iirHypervisor = lens _iirHypervisor (\ s a -> s{_iirHypervisor = a});

-- | The operating system of the virtual machine.
iirPlatform :: Lens' ImportImageResponse (Maybe Text)
iirPlatform = lens _iirPlatform (\ s a -> s{_iirPlatform = a});

-- | The progress of the task.
iirProgress :: Lens' ImportImageResponse (Maybe Text)
iirProgress = lens _iirProgress (\ s a -> s{_iirProgress = a});

-- | The license type of the virtual machine.
iirLicenseType :: Lens' ImportImageResponse (Maybe Text)
iirLicenseType = lens _iirLicenseType (\ s a -> s{_iirLicenseType = a});

-- | Information about the snapshots.
iirSnapshotDetails :: Lens' ImportImageResponse [SnapshotDetail]
iirSnapshotDetails = lens _iirSnapshotDetails (\ s a -> s{_iirSnapshotDetails = a});

-- | A detailed status message of the import task.
iirStatusMessage :: Lens' ImportImageResponse (Maybe Text)
iirStatusMessage = lens _iirStatusMessage (\ s a -> s{_iirStatusMessage = a});

-- | A brief status of the task.
iirAddressStatus :: Lens' ImportImageResponse (Maybe Text)
iirAddressStatus = lens _iirAddressStatus (\ s a -> s{_iirAddressStatus = a});

-- | The ID of the Amazon Machine Image (AMI) created by the import task.
iirImageId :: Lens' ImportImageResponse (Maybe Text)
iirImageId = lens _iirImageId (\ s a -> s{_iirImageId = a});

-- | The task ID of the import image task.
iirImportTaskId :: Lens' ImportImageResponse (Maybe Text)
iirImportTaskId = lens _iirImportTaskId (\ s a -> s{_iirImportTaskId = a});

-- | The architecture of the virtual machine.
iirArchitecture :: Lens' ImportImageResponse (Maybe Text)
iirArchitecture = lens _iirArchitecture (\ s a -> s{_iirArchitecture = a});

-- | A description of the import task.
iirDescription :: Lens' ImportImageResponse (Maybe Text)
iirDescription = lens _iirDescription (\ s a -> s{_iirDescription = a});
