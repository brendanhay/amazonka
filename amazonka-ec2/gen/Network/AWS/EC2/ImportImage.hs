{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

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
    , iHypervisor
    , iPlatform
    , iProgress
    , iLicenseType
    , iSnapshotDetails
    , iStatusMessage
    , iImageId
    , iImportTaskId
    , iArchitecture
    , iDescription
    , iStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

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
data ImportImage = ImportImage'
    { _impHypervisor     :: !(Maybe Text)
    , _impPlatform       :: !(Maybe Text)
    , _impClientToken    :: !(Maybe Text)
    , _impLicenseType    :: !(Maybe Text)
    , _impRoleName       :: !(Maybe Text)
    , _impArchitecture   :: !(Maybe Text)
    , _impDryRun         :: !(Maybe Bool)
    , _impDescription    :: !(Maybe Text)
    , _impClientData     :: !(Maybe ClientData)
    , _impDiskContainers :: !(Maybe [ImageDiskContainer])
    } deriving (Eq,Read,Show)

-- | 'ImportImage' smart constructor.
importImage :: ImportImage
importImage =
    ImportImage'
    { _impHypervisor = Nothing
    , _impPlatform = Nothing
    , _impClientToken = Nothing
    , _impLicenseType = Nothing
    , _impRoleName = Nothing
    , _impArchitecture = Nothing
    , _impDryRun = Nothing
    , _impDescription = Nothing
    , _impClientData = Nothing
    , _impDiskContainers = Nothing
    }

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
impDiskContainers = lens _impDiskContainers (\ s a -> s{_impDiskContainers = a}) . _Default;

instance AWSRequest ImportImage where
        type Sv ImportImage = EC2
        type Rs ImportImage = ImportImageResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 ImportImageResponse' <$>
                   (x .@? "hypervisor") <*> (x .@? "platform") <*>
                     (x .@? "progress")
                     <*> (x .@? "licenseType")
                     <*>
                     (x .@? "snapshotDetailSet" .!@ mempty >>=
                        may (parseXMLList "item"))
                     <*> (x .@? "statusMessage")
                     <*> (x .@? "imageId")
                     <*> (x .@? "importTaskId")
                     <*> (x .@? "architecture")
                     <*> (x .@? "description")
                     <*> (pure (fromEnum s)))

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
               toQuery (toQueryList "item" <$> _impDiskContainers)]

-- | /See:/ 'importImageResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iHypervisor'
--
-- * 'iPlatform'
--
-- * 'iProgress'
--
-- * 'iLicenseType'
--
-- * 'iSnapshotDetails'
--
-- * 'iStatusMessage'
--
-- * 'iImageId'
--
-- * 'iImportTaskId'
--
-- * 'iArchitecture'
--
-- * 'iDescription'
--
-- * 'iStatus'
data ImportImageResponse = ImportImageResponse'
    { _iHypervisor      :: !(Maybe Text)
    , _iPlatform        :: !(Maybe Text)
    , _iProgress        :: !(Maybe Text)
    , _iLicenseType     :: !(Maybe Text)
    , _iSnapshotDetails :: !(Maybe [SnapshotDetail])
    , _iStatusMessage   :: !(Maybe Text)
    , _iImageId         :: !(Maybe Text)
    , _iImportTaskId    :: !(Maybe Text)
    , _iArchitecture    :: !(Maybe Text)
    , _iDescription     :: !(Maybe Text)
    , _iStatus          :: !Int
    } deriving (Eq,Read,Show)

-- | 'ImportImageResponse' smart constructor.
importImageResponse :: Int -> ImportImageResponse
importImageResponse pStatus =
    ImportImageResponse'
    { _iHypervisor = Nothing
    , _iPlatform = Nothing
    , _iProgress = Nothing
    , _iLicenseType = Nothing
    , _iSnapshotDetails = Nothing
    , _iStatusMessage = Nothing
    , _iImageId = Nothing
    , _iImportTaskId = Nothing
    , _iArchitecture = Nothing
    , _iDescription = Nothing
    , _iStatus = pStatus
    }

-- | The target hypervisor of the import task.
iHypervisor :: Lens' ImportImageResponse (Maybe Text)
iHypervisor = lens _iHypervisor (\ s a -> s{_iHypervisor = a});

-- | The operating system of the virtual machine.
iPlatform :: Lens' ImportImageResponse (Maybe Text)
iPlatform = lens _iPlatform (\ s a -> s{_iPlatform = a});

-- | The progress of the task.
iProgress :: Lens' ImportImageResponse (Maybe Text)
iProgress = lens _iProgress (\ s a -> s{_iProgress = a});

-- | The license type of the virtual machine.
iLicenseType :: Lens' ImportImageResponse (Maybe Text)
iLicenseType = lens _iLicenseType (\ s a -> s{_iLicenseType = a});

-- | Information about the snapshots.
iSnapshotDetails :: Lens' ImportImageResponse [SnapshotDetail]
iSnapshotDetails = lens _iSnapshotDetails (\ s a -> s{_iSnapshotDetails = a}) . _Default;

-- | A detailed status message of the import task.
iStatusMessage :: Lens' ImportImageResponse (Maybe Text)
iStatusMessage = lens _iStatusMessage (\ s a -> s{_iStatusMessage = a});

-- | The ID of the Amazon Machine Image (AMI) created by the import task.
iImageId :: Lens' ImportImageResponse (Maybe Text)
iImageId = lens _iImageId (\ s a -> s{_iImageId = a});

-- | The task ID of the import image task.
iImportTaskId :: Lens' ImportImageResponse (Maybe Text)
iImportTaskId = lens _iImportTaskId (\ s a -> s{_iImportTaskId = a});

-- | The architecture of the virtual machine.
iArchitecture :: Lens' ImportImageResponse (Maybe Text)
iArchitecture = lens _iArchitecture (\ s a -> s{_iArchitecture = a});

-- | A description of the import task.
iDescription :: Lens' ImportImageResponse (Maybe Text)
iDescription = lens _iDescription (\ s a -> s{_iDescription = a});

-- | FIXME: Undocumented member.
iStatus :: Lens' ImportImageResponse Int
iStatus = lens _iStatus (\ s a -> s{_iStatus = a});
