{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ImportImage
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Import single or multi-volume disk images or EBS snapshots into an
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
    , irqHypervisor
    , irqPlatform
    , irqClientToken
    , irqLicenseType
    , irqRoleName
    , irqArchitecture
    , irqDryRun
    , irqDescription
    , irqClientData
    , irqDiskContainers

    -- * Response
    , ImportImageResponse
    -- ** Response constructor
    , importImageResponse
    -- ** Response lenses
    , irsHypervisor
    , irsPlatform
    , irsProgress
    , irsLicenseType
    , irsSnapshotDetails
    , irsStatusMessage
    , irsImageId
    , irsImportTaskId
    , irsArchitecture
    , irsDescription
    , irsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'importImage' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'irqHypervisor'
--
-- * 'irqPlatform'
--
-- * 'irqClientToken'
--
-- * 'irqLicenseType'
--
-- * 'irqRoleName'
--
-- * 'irqArchitecture'
--
-- * 'irqDryRun'
--
-- * 'irqDescription'
--
-- * 'irqClientData'
--
-- * 'irqDiskContainers'
data ImportImage = ImportImage'
    { _irqHypervisor     :: !(Maybe Text)
    , _irqPlatform       :: !(Maybe Text)
    , _irqClientToken    :: !(Maybe Text)
    , _irqLicenseType    :: !(Maybe Text)
    , _irqRoleName       :: !(Maybe Text)
    , _irqArchitecture   :: !(Maybe Text)
    , _irqDryRun         :: !(Maybe Bool)
    , _irqDescription    :: !(Maybe Text)
    , _irqClientData     :: !(Maybe ClientData)
    , _irqDiskContainers :: !(Maybe [ImageDiskContainer])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ImportImage' smart constructor.
importImage :: ImportImage
importImage =
    ImportImage'
    { _irqHypervisor = Nothing
    , _irqPlatform = Nothing
    , _irqClientToken = Nothing
    , _irqLicenseType = Nothing
    , _irqRoleName = Nothing
    , _irqArchitecture = Nothing
    , _irqDryRun = Nothing
    , _irqDescription = Nothing
    , _irqClientData = Nothing
    , _irqDiskContainers = Nothing
    }

-- | The target hypervisor platform.
--
-- Valid values: @xen@
irqHypervisor :: Lens' ImportImage (Maybe Text)
irqHypervisor = lens _irqHypervisor (\ s a -> s{_irqHypervisor = a});

-- | The operating system of the virtual machine.
--
-- Valid values: @Windows@ | @Linux@
irqPlatform :: Lens' ImportImage (Maybe Text)
irqPlatform = lens _irqPlatform (\ s a -> s{_irqPlatform = a});

-- | The token to enable idempotency for VM import requests.
irqClientToken :: Lens' ImportImage (Maybe Text)
irqClientToken = lens _irqClientToken (\ s a -> s{_irqClientToken = a});

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
irqLicenseType :: Lens' ImportImage (Maybe Text)
irqLicenseType = lens _irqLicenseType (\ s a -> s{_irqLicenseType = a});

-- | The name of the role to use when not using the default role,
-- \'vmimport\'.
irqRoleName :: Lens' ImportImage (Maybe Text)
irqRoleName = lens _irqRoleName (\ s a -> s{_irqRoleName = a});

-- | The architecture of the virtual machine.
--
-- Valid values: @i386@ | @x86_64@
irqArchitecture :: Lens' ImportImage (Maybe Text)
irqArchitecture = lens _irqArchitecture (\ s a -> s{_irqArchitecture = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
irqDryRun :: Lens' ImportImage (Maybe Bool)
irqDryRun = lens _irqDryRun (\ s a -> s{_irqDryRun = a});

-- | A description string for the import image task.
irqDescription :: Lens' ImportImage (Maybe Text)
irqDescription = lens _irqDescription (\ s a -> s{_irqDescription = a});

-- | The client-specific data.
irqClientData :: Lens' ImportImage (Maybe ClientData)
irqClientData = lens _irqClientData (\ s a -> s{_irqClientData = a});

-- | Information about the disk containers.
irqDiskContainers :: Lens' ImportImage [ImageDiskContainer]
irqDiskContainers = lens _irqDiskContainers (\ s a -> s{_irqDiskContainers = a}) . _Default;

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
               "Hypervisor" =: _irqHypervisor,
               "Platform" =: _irqPlatform,
               "ClientToken" =: _irqClientToken,
               "LicenseType" =: _irqLicenseType,
               "RoleName" =: _irqRoleName,
               "Architecture" =: _irqArchitecture,
               "DryRun" =: _irqDryRun,
               "Description" =: _irqDescription,
               "ClientData" =: _irqClientData,
               toQuery (toQueryList "item" <$> _irqDiskContainers)]

-- | /See:/ 'importImageResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'irsHypervisor'
--
-- * 'irsPlatform'
--
-- * 'irsProgress'
--
-- * 'irsLicenseType'
--
-- * 'irsSnapshotDetails'
--
-- * 'irsStatusMessage'
--
-- * 'irsImageId'
--
-- * 'irsImportTaskId'
--
-- * 'irsArchitecture'
--
-- * 'irsDescription'
--
-- * 'irsStatus'
data ImportImageResponse = ImportImageResponse'
    { _irsHypervisor      :: !(Maybe Text)
    , _irsPlatform        :: !(Maybe Text)
    , _irsProgress        :: !(Maybe Text)
    , _irsLicenseType     :: !(Maybe Text)
    , _irsSnapshotDetails :: !(Maybe [SnapshotDetail])
    , _irsStatusMessage   :: !(Maybe Text)
    , _irsImageId         :: !(Maybe Text)
    , _irsImportTaskId    :: !(Maybe Text)
    , _irsArchitecture    :: !(Maybe Text)
    , _irsDescription     :: !(Maybe Text)
    , _irsStatus          :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ImportImageResponse' smart constructor.
importImageResponse :: Int -> ImportImageResponse
importImageResponse pStatus =
    ImportImageResponse'
    { _irsHypervisor = Nothing
    , _irsPlatform = Nothing
    , _irsProgress = Nothing
    , _irsLicenseType = Nothing
    , _irsSnapshotDetails = Nothing
    , _irsStatusMessage = Nothing
    , _irsImageId = Nothing
    , _irsImportTaskId = Nothing
    , _irsArchitecture = Nothing
    , _irsDescription = Nothing
    , _irsStatus = pStatus
    }

-- | The target hypervisor of the import task.
irsHypervisor :: Lens' ImportImageResponse (Maybe Text)
irsHypervisor = lens _irsHypervisor (\ s a -> s{_irsHypervisor = a});

-- | The operating system of the virtual machine.
irsPlatform :: Lens' ImportImageResponse (Maybe Text)
irsPlatform = lens _irsPlatform (\ s a -> s{_irsPlatform = a});

-- | The progress of the task.
irsProgress :: Lens' ImportImageResponse (Maybe Text)
irsProgress = lens _irsProgress (\ s a -> s{_irsProgress = a});

-- | The license type of the virtual machine.
irsLicenseType :: Lens' ImportImageResponse (Maybe Text)
irsLicenseType = lens _irsLicenseType (\ s a -> s{_irsLicenseType = a});

-- | Information about the snapshots.
irsSnapshotDetails :: Lens' ImportImageResponse [SnapshotDetail]
irsSnapshotDetails = lens _irsSnapshotDetails (\ s a -> s{_irsSnapshotDetails = a}) . _Default;

-- | A detailed status message of the import task.
irsStatusMessage :: Lens' ImportImageResponse (Maybe Text)
irsStatusMessage = lens _irsStatusMessage (\ s a -> s{_irsStatusMessage = a});

-- | The ID of the Amazon Machine Image (AMI) created by the import task.
irsImageId :: Lens' ImportImageResponse (Maybe Text)
irsImageId = lens _irsImageId (\ s a -> s{_irsImageId = a});

-- | The task ID of the import image task.
irsImportTaskId :: Lens' ImportImageResponse (Maybe Text)
irsImportTaskId = lens _irsImportTaskId (\ s a -> s{_irsImportTaskId = a});

-- | The architecture of the virtual machine.
irsArchitecture :: Lens' ImportImageResponse (Maybe Text)
irsArchitecture = lens _irsArchitecture (\ s a -> s{_irsArchitecture = a});

-- | A description of the import task.
irsDescription :: Lens' ImportImageResponse (Maybe Text)
irsDescription = lens _irsDescription (\ s a -> s{_irsDescription = a});

-- | FIXME: Undocumented member.
irsStatus :: Lens' ImportImageResponse Int
irsStatus = lens _irsStatus (\ s a -> s{_irsStatus = a});
