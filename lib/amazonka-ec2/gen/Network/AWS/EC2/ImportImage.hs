{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ImportImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Import single or multi-volume disk images or EBS snapshots into an Amazon Machine Image (AMI). For more information, see <https://docs.aws.amazon.com/vm-import/latest/userguide/vmimport-image-import.html Importing a VM as an Image Using VM Import/Export> in the /VM Import\/Export User Guide/ .
module Network.AWS.EC2.ImportImage
  ( -- * Creating a Request
    importImage,
    ImportImage,

    -- * Request Lenses
    impHypervisor,
    impPlatform,
    impClientToken,
    impLicenseSpecifications,
    impLicenseType,
    impRoleName,
    impEncrypted,
    impTagSpecifications,
    impKMSKeyId,
    impArchitecture,
    impDescription,
    impDryRun,
    impClientData,
    impDiskContainers,

    -- * Destructuring the Response
    importImageResponse,
    ImportImageResponse,

    -- * Response Lenses
    irsStatus,
    irsHypervisor,
    irsPlatform,
    irsProgress,
    irsLicenseSpecifications,
    irsLicenseType,
    irsSnapshotDetails,
    irsEncrypted,
    irsKMSKeyId,
    irsStatusMessage,
    irsImageId,
    irsImportTaskId,
    irsArchitecture,
    irsDescription,
    irsTags,
    irsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'importImage' smart constructor.
data ImportImage = ImportImage'
  { _impHypervisor :: !(Maybe Text),
    _impPlatform :: !(Maybe Text),
    _impClientToken :: !(Maybe Text),
    _impLicenseSpecifications ::
      !(Maybe [ImportImageLicenseConfigurationRequest]),
    _impLicenseType :: !(Maybe Text),
    _impRoleName :: !(Maybe Text),
    _impEncrypted :: !(Maybe Bool),
    _impTagSpecifications :: !(Maybe [TagSpecification]),
    _impKMSKeyId :: !(Maybe Text),
    _impArchitecture :: !(Maybe Text),
    _impDescription :: !(Maybe Text),
    _impDryRun :: !(Maybe Bool),
    _impClientData :: !(Maybe ClientData),
    _impDiskContainers :: !(Maybe [ImageDiskContainer])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

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
-- * 'impLicenseSpecifications' - The ARNs of the license configurations.
--
-- * 'impLicenseType' - The license type to be used for the Amazon Machine Image (AMI) after importing. By default, we detect the source-system operating system (OS) and apply the appropriate license. Specify @AWS@ to replace the source-system license with an AWS license, if appropriate. Specify @BYOL@ to retain the source-system license, if appropriate. To use @BYOL@ , you must have existing licenses with rights to use these licenses in a third party cloud, such as AWS. For more information, see <https://docs.aws.amazon.com/vm-import/latest/userguide/vmimport-image-import.html#prerequisites-image Prerequisites> in the VM Import/Export User Guide.
--
-- * 'impRoleName' - The name of the role to use when not using the default role, 'vmimport'.
--
-- * 'impEncrypted' - Specifies whether the destination AMI of the imported image should be encrypted. The default CMK for EBS is used unless you specify a non-default AWS Key Management Service (AWS KMS) CMK using @KmsKeyId@ . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- * 'impTagSpecifications' - The tags to apply to the image being imported.
--
-- * 'impKMSKeyId' - An identifier for the symmetric AWS Key Management Service (AWS KMS) customer master key (CMK) to use when creating the encrypted AMI. This parameter is only required if you want to use a non-default CMK; if this parameter is not specified, the default CMK for EBS is used. If a @KmsKeyId@ is specified, the @Encrypted@ flag must also be set.  The CMK identifier may be provided in any of the following formats:      * Key ID     * Key alias. The alias ARN contains the @arn:aws:kms@ namespace, followed by the Region of the CMK, the AWS account ID of the CMK owner, the @alias@ namespace, and then the CMK alias. For example, arn:aws:kms:/us-east-1/ :/012345678910/ :alias//ExampleAlias/ .     * ARN using key ID. The ID ARN contains the @arn:aws:kms@ namespace, followed by the Region of the CMK, the AWS account ID of the CMK owner, the @key@ namespace, and then the CMK ID. For example, arn:aws:kms:/us-east-1/ :/012345678910/ :key//abcd1234-a123-456a-a12b-a123b4cd56ef/ .     * ARN using key alias. The alias ARN contains the @arn:aws:kms@ namespace, followed by the Region of the CMK, the AWS account ID of the CMK owner, the @alias@ namespace, and then the CMK alias. For example, arn:aws:kms:/us-east-1/ :/012345678910/ :alias//ExampleAlias/ .  AWS parses @KmsKeyId@ asynchronously, meaning that the action you call may appear to complete even though you provided an invalid identifier. This action will eventually report failure.  The specified CMK must exist in the Region that the AMI is being copied to. Amazon EBS does not support asymmetric CMKs.
--
-- * 'impArchitecture' - The architecture of the virtual machine. Valid values: @i386@ | @x86_64@ | @arm64@
--
-- * 'impDescription' - A description string for the import image task.
--
-- * 'impDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'impClientData' - The client-specific data.
--
-- * 'impDiskContainers' - Information about the disk containers.
importImage ::
  ImportImage
importImage =
  ImportImage'
    { _impHypervisor = Nothing,
      _impPlatform = Nothing,
      _impClientToken = Nothing,
      _impLicenseSpecifications = Nothing,
      _impLicenseType = Nothing,
      _impRoleName = Nothing,
      _impEncrypted = Nothing,
      _impTagSpecifications = Nothing,
      _impKMSKeyId = Nothing,
      _impArchitecture = Nothing,
      _impDescription = Nothing,
      _impDryRun = Nothing,
      _impClientData = Nothing,
      _impDiskContainers = Nothing
    }

-- | The target hypervisor platform. Valid values: @xen@
impHypervisor :: Lens' ImportImage (Maybe Text)
impHypervisor = lens _impHypervisor (\s a -> s {_impHypervisor = a})

-- | The operating system of the virtual machine. Valid values: @Windows@ | @Linux@
impPlatform :: Lens' ImportImage (Maybe Text)
impPlatform = lens _impPlatform (\s a -> s {_impPlatform = a})

-- | The token to enable idempotency for VM import requests.
impClientToken :: Lens' ImportImage (Maybe Text)
impClientToken = lens _impClientToken (\s a -> s {_impClientToken = a})

-- | The ARNs of the license configurations.
impLicenseSpecifications :: Lens' ImportImage [ImportImageLicenseConfigurationRequest]
impLicenseSpecifications = lens _impLicenseSpecifications (\s a -> s {_impLicenseSpecifications = a}) . _Default . _Coerce

-- | The license type to be used for the Amazon Machine Image (AMI) after importing. By default, we detect the source-system operating system (OS) and apply the appropriate license. Specify @AWS@ to replace the source-system license with an AWS license, if appropriate. Specify @BYOL@ to retain the source-system license, if appropriate. To use @BYOL@ , you must have existing licenses with rights to use these licenses in a third party cloud, such as AWS. For more information, see <https://docs.aws.amazon.com/vm-import/latest/userguide/vmimport-image-import.html#prerequisites-image Prerequisites> in the VM Import/Export User Guide.
impLicenseType :: Lens' ImportImage (Maybe Text)
impLicenseType = lens _impLicenseType (\s a -> s {_impLicenseType = a})

-- | The name of the role to use when not using the default role, 'vmimport'.
impRoleName :: Lens' ImportImage (Maybe Text)
impRoleName = lens _impRoleName (\s a -> s {_impRoleName = a})

-- | Specifies whether the destination AMI of the imported image should be encrypted. The default CMK for EBS is used unless you specify a non-default AWS Key Management Service (AWS KMS) CMK using @KmsKeyId@ . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
impEncrypted :: Lens' ImportImage (Maybe Bool)
impEncrypted = lens _impEncrypted (\s a -> s {_impEncrypted = a})

-- | The tags to apply to the image being imported.
impTagSpecifications :: Lens' ImportImage [TagSpecification]
impTagSpecifications = lens _impTagSpecifications (\s a -> s {_impTagSpecifications = a}) . _Default . _Coerce

-- | An identifier for the symmetric AWS Key Management Service (AWS KMS) customer master key (CMK) to use when creating the encrypted AMI. This parameter is only required if you want to use a non-default CMK; if this parameter is not specified, the default CMK for EBS is used. If a @KmsKeyId@ is specified, the @Encrypted@ flag must also be set.  The CMK identifier may be provided in any of the following formats:      * Key ID     * Key alias. The alias ARN contains the @arn:aws:kms@ namespace, followed by the Region of the CMK, the AWS account ID of the CMK owner, the @alias@ namespace, and then the CMK alias. For example, arn:aws:kms:/us-east-1/ :/012345678910/ :alias//ExampleAlias/ .     * ARN using key ID. The ID ARN contains the @arn:aws:kms@ namespace, followed by the Region of the CMK, the AWS account ID of the CMK owner, the @key@ namespace, and then the CMK ID. For example, arn:aws:kms:/us-east-1/ :/012345678910/ :key//abcd1234-a123-456a-a12b-a123b4cd56ef/ .     * ARN using key alias. The alias ARN contains the @arn:aws:kms@ namespace, followed by the Region of the CMK, the AWS account ID of the CMK owner, the @alias@ namespace, and then the CMK alias. For example, arn:aws:kms:/us-east-1/ :/012345678910/ :alias//ExampleAlias/ .  AWS parses @KmsKeyId@ asynchronously, meaning that the action you call may appear to complete even though you provided an invalid identifier. This action will eventually report failure.  The specified CMK must exist in the Region that the AMI is being copied to. Amazon EBS does not support asymmetric CMKs.
impKMSKeyId :: Lens' ImportImage (Maybe Text)
impKMSKeyId = lens _impKMSKeyId (\s a -> s {_impKMSKeyId = a})

-- | The architecture of the virtual machine. Valid values: @i386@ | @x86_64@ | @arm64@
impArchitecture :: Lens' ImportImage (Maybe Text)
impArchitecture = lens _impArchitecture (\s a -> s {_impArchitecture = a})

-- | A description string for the import image task.
impDescription :: Lens' ImportImage (Maybe Text)
impDescription = lens _impDescription (\s a -> s {_impDescription = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
impDryRun :: Lens' ImportImage (Maybe Bool)
impDryRun = lens _impDryRun (\s a -> s {_impDryRun = a})

-- | The client-specific data.
impClientData :: Lens' ImportImage (Maybe ClientData)
impClientData = lens _impClientData (\s a -> s {_impClientData = a})

-- | Information about the disk containers.
impDiskContainers :: Lens' ImportImage [ImageDiskContainer]
impDiskContainers = lens _impDiskContainers (\s a -> s {_impDiskContainers = a}) . _Default . _Coerce

instance AWSRequest ImportImage where
  type Rs ImportImage = ImportImageResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          ImportImageResponse'
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
            <*> (pure (fromEnum s))
      )

instance Hashable ImportImage

instance NFData ImportImage

instance ToHeaders ImportImage where
  toHeaders = const mempty

instance ToPath ImportImage where
  toPath = const "/"

instance ToQuery ImportImage where
  toQuery ImportImage' {..} =
    mconcat
      [ "Action" =: ("ImportImage" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "Hypervisor" =: _impHypervisor,
        "Platform" =: _impPlatform,
        "ClientToken" =: _impClientToken,
        toQuery
          ( toQueryList "LicenseSpecifications"
              <$> _impLicenseSpecifications
          ),
        "LicenseType" =: _impLicenseType,
        "RoleName" =: _impRoleName,
        "Encrypted" =: _impEncrypted,
        toQuery (toQueryList "TagSpecification" <$> _impTagSpecifications),
        "KmsKeyId" =: _impKMSKeyId,
        "Architecture" =: _impArchitecture,
        "Description" =: _impDescription,
        "DryRun" =: _impDryRun,
        "ClientData" =: _impClientData,
        toQuery (toQueryList "DiskContainer" <$> _impDiskContainers)
      ]

-- | /See:/ 'importImageResponse' smart constructor.
data ImportImageResponse = ImportImageResponse'
  { _irsStatus ::
      !(Maybe Text),
    _irsHypervisor :: !(Maybe Text),
    _irsPlatform :: !(Maybe Text),
    _irsProgress :: !(Maybe Text),
    _irsLicenseSpecifications ::
      !(Maybe [ImportImageLicenseConfigurationResponse]),
    _irsLicenseType :: !(Maybe Text),
    _irsSnapshotDetails :: !(Maybe [SnapshotDetail]),
    _irsEncrypted :: !(Maybe Bool),
    _irsKMSKeyId :: !(Maybe Text),
    _irsStatusMessage :: !(Maybe Text),
    _irsImageId :: !(Maybe Text),
    _irsImportTaskId :: !(Maybe Text),
    _irsArchitecture :: !(Maybe Text),
    _irsDescription :: !(Maybe Text),
    _irsTags :: !(Maybe [Tag]),
    _irsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

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
-- * 'irsLicenseSpecifications' - The ARNs of the license configurations.
--
-- * 'irsLicenseType' - The license type of the virtual machine.
--
-- * 'irsSnapshotDetails' - Information about the snapshots.
--
-- * 'irsEncrypted' - Indicates whether the AMI is encrypted.
--
-- * 'irsKMSKeyId' - The identifier for the symmetric AWS Key Management Service (AWS KMS) customer master key (CMK) that was used to create the encrypted AMI.
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
-- * 'irsTags' - Any tags assigned to the image being imported.
--
-- * 'irsResponseStatus' - -- | The response status code.
importImageResponse ::
  -- | 'irsResponseStatus'
  Int ->
  ImportImageResponse
importImageResponse pResponseStatus_ =
  ImportImageResponse'
    { _irsStatus = Nothing,
      _irsHypervisor = Nothing,
      _irsPlatform = Nothing,
      _irsProgress = Nothing,
      _irsLicenseSpecifications = Nothing,
      _irsLicenseType = Nothing,
      _irsSnapshotDetails = Nothing,
      _irsEncrypted = Nothing,
      _irsKMSKeyId = Nothing,
      _irsStatusMessage = Nothing,
      _irsImageId = Nothing,
      _irsImportTaskId = Nothing,
      _irsArchitecture = Nothing,
      _irsDescription = Nothing,
      _irsTags = Nothing,
      _irsResponseStatus = pResponseStatus_
    }

-- | A brief status of the task.
irsStatus :: Lens' ImportImageResponse (Maybe Text)
irsStatus = lens _irsStatus (\s a -> s {_irsStatus = a})

-- | The target hypervisor of the import task.
irsHypervisor :: Lens' ImportImageResponse (Maybe Text)
irsHypervisor = lens _irsHypervisor (\s a -> s {_irsHypervisor = a})

-- | The operating system of the virtual machine.
irsPlatform :: Lens' ImportImageResponse (Maybe Text)
irsPlatform = lens _irsPlatform (\s a -> s {_irsPlatform = a})

-- | The progress of the task.
irsProgress :: Lens' ImportImageResponse (Maybe Text)
irsProgress = lens _irsProgress (\s a -> s {_irsProgress = a})

-- | The ARNs of the license configurations.
irsLicenseSpecifications :: Lens' ImportImageResponse [ImportImageLicenseConfigurationResponse]
irsLicenseSpecifications = lens _irsLicenseSpecifications (\s a -> s {_irsLicenseSpecifications = a}) . _Default . _Coerce

-- | The license type of the virtual machine.
irsLicenseType :: Lens' ImportImageResponse (Maybe Text)
irsLicenseType = lens _irsLicenseType (\s a -> s {_irsLicenseType = a})

-- | Information about the snapshots.
irsSnapshotDetails :: Lens' ImportImageResponse [SnapshotDetail]
irsSnapshotDetails = lens _irsSnapshotDetails (\s a -> s {_irsSnapshotDetails = a}) . _Default . _Coerce

-- | Indicates whether the AMI is encrypted.
irsEncrypted :: Lens' ImportImageResponse (Maybe Bool)
irsEncrypted = lens _irsEncrypted (\s a -> s {_irsEncrypted = a})

-- | The identifier for the symmetric AWS Key Management Service (AWS KMS) customer master key (CMK) that was used to create the encrypted AMI.
irsKMSKeyId :: Lens' ImportImageResponse (Maybe Text)
irsKMSKeyId = lens _irsKMSKeyId (\s a -> s {_irsKMSKeyId = a})

-- | A detailed status message of the import task.
irsStatusMessage :: Lens' ImportImageResponse (Maybe Text)
irsStatusMessage = lens _irsStatusMessage (\s a -> s {_irsStatusMessage = a})

-- | The ID of the Amazon Machine Image (AMI) created by the import task.
irsImageId :: Lens' ImportImageResponse (Maybe Text)
irsImageId = lens _irsImageId (\s a -> s {_irsImageId = a})

-- | The task ID of the import image task.
irsImportTaskId :: Lens' ImportImageResponse (Maybe Text)
irsImportTaskId = lens _irsImportTaskId (\s a -> s {_irsImportTaskId = a})

-- | The architecture of the virtual machine.
irsArchitecture :: Lens' ImportImageResponse (Maybe Text)
irsArchitecture = lens _irsArchitecture (\s a -> s {_irsArchitecture = a})

-- | A description of the import task.
irsDescription :: Lens' ImportImageResponse (Maybe Text)
irsDescription = lens _irsDescription (\s a -> s {_irsDescription = a})

-- | Any tags assigned to the image being imported.
irsTags :: Lens' ImportImageResponse [Tag]
irsTags = lens _irsTags (\s a -> s {_irsTags = a}) . _Default . _Coerce

-- | -- | The response status code.
irsResponseStatus :: Lens' ImportImageResponse Int
irsResponseStatus = lens _irsResponseStatus (\s a -> s {_irsResponseStatus = a})

instance NFData ImportImageResponse
