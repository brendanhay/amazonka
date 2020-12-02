{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ImportImageTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ImportImageTask where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ImportImageLicenseConfigurationResponse
import Network.AWS.EC2.Types.SnapshotDetail
import Network.AWS.EC2.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an import image task.
--
--
--
-- /See:/ 'importImageTask' smart constructor.
data ImportImageTask = ImportImageTask'
  { _iitStatus ::
      !(Maybe Text),
    _iitHypervisor :: !(Maybe Text),
    _iitPlatform :: !(Maybe Text),
    _iitProgress :: !(Maybe Text),
    _iitLicenseSpecifications ::
      !(Maybe [ImportImageLicenseConfigurationResponse]),
    _iitLicenseType :: !(Maybe Text),
    _iitSnapshotDetails :: !(Maybe [SnapshotDetail]),
    _iitEncrypted :: !(Maybe Bool),
    _iitKMSKeyId :: !(Maybe Text),
    _iitStatusMessage :: !(Maybe Text),
    _iitImageId :: !(Maybe Text),
    _iitImportTaskId :: !(Maybe Text),
    _iitArchitecture :: !(Maybe Text),
    _iitDescription :: !(Maybe Text),
    _iitTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ImportImageTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iitStatus' - A brief status for the import image task.
--
-- * 'iitHypervisor' - The target hypervisor for the import task. Valid values: @xen@
--
-- * 'iitPlatform' - The description string for the import image task.
--
-- * 'iitProgress' - The percentage of progress of the import image task.
--
-- * 'iitLicenseSpecifications' - The ARNs of the license configurations that are associated with the import image task.
--
-- * 'iitLicenseType' - The license type of the virtual machine.
--
-- * 'iitSnapshotDetails' - Information about the snapshots.
--
-- * 'iitEncrypted' - Indicates whether the image is encrypted.
--
-- * 'iitKMSKeyId' - The identifier for the AWS Key Management Service (AWS KMS) customer master key (CMK) that was used to create the encrypted image.
--
-- * 'iitStatusMessage' - A descriptive status message for the import image task.
--
-- * 'iitImageId' - The ID of the Amazon Machine Image (AMI) of the imported virtual machine.
--
-- * 'iitImportTaskId' - The ID of the import image task.
--
-- * 'iitArchitecture' - The architecture of the virtual machine. Valid values: @i386@ | @x86_64@ | @arm64@
--
-- * 'iitDescription' - A description of the import task.
--
-- * 'iitTags' - The tags for the import image task.
importImageTask ::
  ImportImageTask
importImageTask =
  ImportImageTask'
    { _iitStatus = Nothing,
      _iitHypervisor = Nothing,
      _iitPlatform = Nothing,
      _iitProgress = Nothing,
      _iitLicenseSpecifications = Nothing,
      _iitLicenseType = Nothing,
      _iitSnapshotDetails = Nothing,
      _iitEncrypted = Nothing,
      _iitKMSKeyId = Nothing,
      _iitStatusMessage = Nothing,
      _iitImageId = Nothing,
      _iitImportTaskId = Nothing,
      _iitArchitecture = Nothing,
      _iitDescription = Nothing,
      _iitTags = Nothing
    }

-- | A brief status for the import image task.
iitStatus :: Lens' ImportImageTask (Maybe Text)
iitStatus = lens _iitStatus (\s a -> s {_iitStatus = a})

-- | The target hypervisor for the import task. Valid values: @xen@
iitHypervisor :: Lens' ImportImageTask (Maybe Text)
iitHypervisor = lens _iitHypervisor (\s a -> s {_iitHypervisor = a})

-- | The description string for the import image task.
iitPlatform :: Lens' ImportImageTask (Maybe Text)
iitPlatform = lens _iitPlatform (\s a -> s {_iitPlatform = a})

-- | The percentage of progress of the import image task.
iitProgress :: Lens' ImportImageTask (Maybe Text)
iitProgress = lens _iitProgress (\s a -> s {_iitProgress = a})

-- | The ARNs of the license configurations that are associated with the import image task.
iitLicenseSpecifications :: Lens' ImportImageTask [ImportImageLicenseConfigurationResponse]
iitLicenseSpecifications = lens _iitLicenseSpecifications (\s a -> s {_iitLicenseSpecifications = a}) . _Default . _Coerce

-- | The license type of the virtual machine.
iitLicenseType :: Lens' ImportImageTask (Maybe Text)
iitLicenseType = lens _iitLicenseType (\s a -> s {_iitLicenseType = a})

-- | Information about the snapshots.
iitSnapshotDetails :: Lens' ImportImageTask [SnapshotDetail]
iitSnapshotDetails = lens _iitSnapshotDetails (\s a -> s {_iitSnapshotDetails = a}) . _Default . _Coerce

-- | Indicates whether the image is encrypted.
iitEncrypted :: Lens' ImportImageTask (Maybe Bool)
iitEncrypted = lens _iitEncrypted (\s a -> s {_iitEncrypted = a})

-- | The identifier for the AWS Key Management Service (AWS KMS) customer master key (CMK) that was used to create the encrypted image.
iitKMSKeyId :: Lens' ImportImageTask (Maybe Text)
iitKMSKeyId = lens _iitKMSKeyId (\s a -> s {_iitKMSKeyId = a})

-- | A descriptive status message for the import image task.
iitStatusMessage :: Lens' ImportImageTask (Maybe Text)
iitStatusMessage = lens _iitStatusMessage (\s a -> s {_iitStatusMessage = a})

-- | The ID of the Amazon Machine Image (AMI) of the imported virtual machine.
iitImageId :: Lens' ImportImageTask (Maybe Text)
iitImageId = lens _iitImageId (\s a -> s {_iitImageId = a})

-- | The ID of the import image task.
iitImportTaskId :: Lens' ImportImageTask (Maybe Text)
iitImportTaskId = lens _iitImportTaskId (\s a -> s {_iitImportTaskId = a})

-- | The architecture of the virtual machine. Valid values: @i386@ | @x86_64@ | @arm64@
iitArchitecture :: Lens' ImportImageTask (Maybe Text)
iitArchitecture = lens _iitArchitecture (\s a -> s {_iitArchitecture = a})

-- | A description of the import task.
iitDescription :: Lens' ImportImageTask (Maybe Text)
iitDescription = lens _iitDescription (\s a -> s {_iitDescription = a})

-- | The tags for the import image task.
iitTags :: Lens' ImportImageTask [Tag]
iitTags = lens _iitTags (\s a -> s {_iitTags = a}) . _Default . _Coerce

instance FromXML ImportImageTask where
  parseXML x =
    ImportImageTask'
      <$> (x .@? "status")
      <*> (x .@? "hypervisor")
      <*> (x .@? "platform")
      <*> (x .@? "progress")
      <*> ( x .@? "licenseSpecifications" .!@ mempty
              >>= may (parseXMLList "item")
          )
      <*> (x .@? "licenseType")
      <*> ( x .@? "snapshotDetailSet" .!@ mempty
              >>= may (parseXMLList "item")
          )
      <*> (x .@? "encrypted")
      <*> (x .@? "kmsKeyId")
      <*> (x .@? "statusMessage")
      <*> (x .@? "imageId")
      <*> (x .@? "importTaskId")
      <*> (x .@? "architecture")
      <*> (x .@? "description")
      <*> (x .@? "tagSet" .!@ mempty >>= may (parseXMLList "item"))

instance Hashable ImportImageTask

instance NFData ImportImageTask
