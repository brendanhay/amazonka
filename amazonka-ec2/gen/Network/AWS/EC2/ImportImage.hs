{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ImportImage
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Import single or multi-volume disk images or EBS snapshots into an Amazon Machine Image (AMI). For more information, see <http://docs.aws.amazon.com/vm-import/latest/userguide/vmimport-image-import.html Importing a VM as an Image Using VM Import/Export> in the /VM Import\/Export User Guide/ .
--
--
module Network.AWS.EC2.ImportImage
    (
    -- * Creating a Request
      importImage
    , ImportImage
    -- * Request Lenses
    , impHypervisor
    , impPlatform
    , impClientToken
    , impLicenseType
    , impRoleName
    , impArchitecture
    , impDescription
    , impDryRun
    , impClientData
    , impDiskContainers

    -- * Destructuring the Response
    , importImageResponse
    , ImportImageResponse
    -- * Response Lenses
    , irsStatus
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
    , irsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for ImportImage.
--
--
--
-- /See:/ 'importImage' smart constructor.
data ImportImage = ImportImage'
  { _impHypervisor     :: !(Maybe Text)
  , _impPlatform       :: !(Maybe Text)
  , _impClientToken    :: !(Maybe Text)
  , _impLicenseType    :: !(Maybe Text)
  , _impRoleName       :: !(Maybe Text)
  , _impArchitecture   :: !(Maybe Text)
  , _impDescription    :: !(Maybe Text)
  , _impDryRun         :: !(Maybe Bool)
  , _impClientData     :: !(Maybe ClientData)
  , _impDiskContainers :: !(Maybe [ImageDiskContainer])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ImportImage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'impHypervisor' - The target hypervisor platform. Valid values: @xen@
--
-- * 'impPlatform' - The operating system of the virtual machine. Valid values: @Windows@ | @Linux@
--
-- * 'impClientToken' - The token to enable idempotency for VM import requests.
--
-- * 'impLicenseType' - The license type to be used for the Amazon Machine Image (AMI) after importing. __Note:__ You may only use BYOL if you have existing licenses with rights to use these licenses in a third party cloud like AWS. For more information, see <http://docs.aws.amazon.com/vm-import/latest/userguide/vmimport-image-import.html#prerequisites-image Prerequisites> in the VM Import/Export User Guide. Valid values: @AWS@ | @BYOL@
--
-- * 'impRoleName' - The name of the role to use when not using the default role, 'vmimport'.
--
-- * 'impArchitecture' - The architecture of the virtual machine. Valid values: @i386@ | @x86_64@
--
-- * 'impDescription' - A description string for the import image task.
--
-- * 'impDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'impClientData' - The client-specific data.
--
-- * 'impDiskContainers' - Information about the disk containers.
importImage
    :: ImportImage
importImage =
  ImportImage'
    { _impHypervisor = Nothing
    , _impPlatform = Nothing
    , _impClientToken = Nothing
    , _impLicenseType = Nothing
    , _impRoleName = Nothing
    , _impArchitecture = Nothing
    , _impDescription = Nothing
    , _impDryRun = Nothing
    , _impClientData = Nothing
    , _impDiskContainers = Nothing
    }


-- | The target hypervisor platform. Valid values: @xen@
impHypervisor :: Lens' ImportImage (Maybe Text)
impHypervisor = lens _impHypervisor (\ s a -> s{_impHypervisor = a})

-- | The operating system of the virtual machine. Valid values: @Windows@ | @Linux@
impPlatform :: Lens' ImportImage (Maybe Text)
impPlatform = lens _impPlatform (\ s a -> s{_impPlatform = a})

-- | The token to enable idempotency for VM import requests.
impClientToken :: Lens' ImportImage (Maybe Text)
impClientToken = lens _impClientToken (\ s a -> s{_impClientToken = a})

-- | The license type to be used for the Amazon Machine Image (AMI) after importing. __Note:__ You may only use BYOL if you have existing licenses with rights to use these licenses in a third party cloud like AWS. For more information, see <http://docs.aws.amazon.com/vm-import/latest/userguide/vmimport-image-import.html#prerequisites-image Prerequisites> in the VM Import/Export User Guide. Valid values: @AWS@ | @BYOL@
impLicenseType :: Lens' ImportImage (Maybe Text)
impLicenseType = lens _impLicenseType (\ s a -> s{_impLicenseType = a})

-- | The name of the role to use when not using the default role, 'vmimport'.
impRoleName :: Lens' ImportImage (Maybe Text)
impRoleName = lens _impRoleName (\ s a -> s{_impRoleName = a})

-- | The architecture of the virtual machine. Valid values: @i386@ | @x86_64@
impArchitecture :: Lens' ImportImage (Maybe Text)
impArchitecture = lens _impArchitecture (\ s a -> s{_impArchitecture = a})

-- | A description string for the import image task.
impDescription :: Lens' ImportImage (Maybe Text)
impDescription = lens _impDescription (\ s a -> s{_impDescription = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
impDryRun :: Lens' ImportImage (Maybe Bool)
impDryRun = lens _impDryRun (\ s a -> s{_impDryRun = a})

-- | The client-specific data.
impClientData :: Lens' ImportImage (Maybe ClientData)
impClientData = lens _impClientData (\ s a -> s{_impClientData = a})

-- | Information about the disk containers.
impDiskContainers :: Lens' ImportImage [ImageDiskContainer]
impDiskContainers = lens _impDiskContainers (\ s a -> s{_impDiskContainers = a}) . _Default . _Coerce

instance AWSRequest ImportImage where
        type Rs ImportImage = ImportImageResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 ImportImageResponse' <$>
                   (x .@? "status") <*> (x .@? "hypervisor") <*>
                     (x .@? "platform")
                     <*> (x .@? "progress")
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

instance Hashable ImportImage where

instance NFData ImportImage where

instance ToHeaders ImportImage where
        toHeaders = const mempty

instance ToPath ImportImage where
        toPath = const "/"

instance ToQuery ImportImage where
        toQuery ImportImage'{..}
          = mconcat
              ["Action" =: ("ImportImage" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "Hypervisor" =: _impHypervisor,
               "Platform" =: _impPlatform,
               "ClientToken" =: _impClientToken,
               "LicenseType" =: _impLicenseType,
               "RoleName" =: _impRoleName,
               "Architecture" =: _impArchitecture,
               "Description" =: _impDescription,
               "DryRun" =: _impDryRun,
               "ClientData" =: _impClientData,
               toQuery
                 (toQueryList "DiskContainer" <$> _impDiskContainers)]

-- | Contains the output for ImportImage.
--
--
--
-- /See:/ 'importImageResponse' smart constructor.
data ImportImageResponse = ImportImageResponse'
  { _irsStatus          :: !(Maybe Text)
  , _irsHypervisor      :: !(Maybe Text)
  , _irsPlatform        :: !(Maybe Text)
  , _irsProgress        :: !(Maybe Text)
  , _irsLicenseType     :: !(Maybe Text)
  , _irsSnapshotDetails :: !(Maybe [SnapshotDetail])
  , _irsStatusMessage   :: !(Maybe Text)
  , _irsImageId         :: !(Maybe Text)
  , _irsImportTaskId    :: !(Maybe Text)
  , _irsArchitecture    :: !(Maybe Text)
  , _irsDescription     :: !(Maybe Text)
  , _irsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ImportImageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'irsStatus' - A brief status of the task.
--
-- * 'irsHypervisor' - The target hypervisor of the import task.
--
-- * 'irsPlatform' - The operating system of the virtual machine.
--
-- * 'irsProgress' - The progress of the task.
--
-- * 'irsLicenseType' - The license type of the virtual machine.
--
-- * 'irsSnapshotDetails' - Information about the snapshots.
--
-- * 'irsStatusMessage' - A detailed status message of the import task.
--
-- * 'irsImageId' - The ID of the Amazon Machine Image (AMI) created by the import task.
--
-- * 'irsImportTaskId' - The task ID of the import image task.
--
-- * 'irsArchitecture' - The architecture of the virtual machine.
--
-- * 'irsDescription' - A description of the import task.
--
-- * 'irsResponseStatus' - -- | The response status code.
importImageResponse
    :: Int -- ^ 'irsResponseStatus'
    -> ImportImageResponse
importImageResponse pResponseStatus_ =
  ImportImageResponse'
    { _irsStatus = Nothing
    , _irsHypervisor = Nothing
    , _irsPlatform = Nothing
    , _irsProgress = Nothing
    , _irsLicenseType = Nothing
    , _irsSnapshotDetails = Nothing
    , _irsStatusMessage = Nothing
    , _irsImageId = Nothing
    , _irsImportTaskId = Nothing
    , _irsArchitecture = Nothing
    , _irsDescription = Nothing
    , _irsResponseStatus = pResponseStatus_
    }


-- | A brief status of the task.
irsStatus :: Lens' ImportImageResponse (Maybe Text)
irsStatus = lens _irsStatus (\ s a -> s{_irsStatus = a})

-- | The target hypervisor of the import task.
irsHypervisor :: Lens' ImportImageResponse (Maybe Text)
irsHypervisor = lens _irsHypervisor (\ s a -> s{_irsHypervisor = a})

-- | The operating system of the virtual machine.
irsPlatform :: Lens' ImportImageResponse (Maybe Text)
irsPlatform = lens _irsPlatform (\ s a -> s{_irsPlatform = a})

-- | The progress of the task.
irsProgress :: Lens' ImportImageResponse (Maybe Text)
irsProgress = lens _irsProgress (\ s a -> s{_irsProgress = a})

-- | The license type of the virtual machine.
irsLicenseType :: Lens' ImportImageResponse (Maybe Text)
irsLicenseType = lens _irsLicenseType (\ s a -> s{_irsLicenseType = a})

-- | Information about the snapshots.
irsSnapshotDetails :: Lens' ImportImageResponse [SnapshotDetail]
irsSnapshotDetails = lens _irsSnapshotDetails (\ s a -> s{_irsSnapshotDetails = a}) . _Default . _Coerce

-- | A detailed status message of the import task.
irsStatusMessage :: Lens' ImportImageResponse (Maybe Text)
irsStatusMessage = lens _irsStatusMessage (\ s a -> s{_irsStatusMessage = a})

-- | The ID of the Amazon Machine Image (AMI) created by the import task.
irsImageId :: Lens' ImportImageResponse (Maybe Text)
irsImageId = lens _irsImageId (\ s a -> s{_irsImageId = a})

-- | The task ID of the import image task.
irsImportTaskId :: Lens' ImportImageResponse (Maybe Text)
irsImportTaskId = lens _irsImportTaskId (\ s a -> s{_irsImportTaskId = a})

-- | The architecture of the virtual machine.
irsArchitecture :: Lens' ImportImageResponse (Maybe Text)
irsArchitecture = lens _irsArchitecture (\ s a -> s{_irsArchitecture = a})

-- | A description of the import task.
irsDescription :: Lens' ImportImageResponse (Maybe Text)
irsDescription = lens _irsDescription (\ s a -> s{_irsDescription = a})

-- | -- | The response status code.
irsResponseStatus :: Lens' ImportImageResponse Int
irsResponseStatus = lens _irsResponseStatus (\ s a -> s{_irsResponseStatus = a})

instance NFData ImportImageResponse where
