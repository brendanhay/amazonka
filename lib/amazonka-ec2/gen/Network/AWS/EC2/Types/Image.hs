{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Image
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Image where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ArchitectureValues
import Network.AWS.EC2.Types.BlockDeviceMapping
import Network.AWS.EC2.Types.DeviceType
import Network.AWS.EC2.Types.HypervisorType
import Network.AWS.EC2.Types.ImageState
import Network.AWS.EC2.Types.ImageTypeValues
import Network.AWS.EC2.Types.PlatformValues
import Network.AWS.EC2.Types.ProductCode
import Network.AWS.EC2.Types.StateReason
import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.VirtualizationType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an image.
--
--
--
-- /See:/ 'image' smart constructor.
data Image = Image'
  { _iPlatform :: !(Maybe PlatformValues),
    _iPlatformDetails :: !(Maybe Text),
    _iEnaSupport :: !(Maybe Bool),
    _iImageOwnerAlias :: !(Maybe Text),
    _iUsageOperation :: !(Maybe Text),
    _iRAMDiskId :: !(Maybe Text),
    _iKernelId :: !(Maybe Text),
    _iRootDeviceName :: !(Maybe Text),
    _iSRIOVNetSupport :: !(Maybe Text),
    _iName :: !(Maybe Text),
    _iCreationDate :: !(Maybe Text),
    _iProductCodes :: !(Maybe [ProductCode]),
    _iStateReason :: !(Maybe StateReason),
    _iDescription :: !(Maybe Text),
    _iBlockDeviceMappings :: !(Maybe [BlockDeviceMapping]),
    _iTags :: !(Maybe [Tag]),
    _iImageId :: !Text,
    _iImageLocation :: !Text,
    _iState :: !ImageState,
    _iOwnerId :: !Text,
    _iPublic :: !Bool,
    _iArchitecture :: !ArchitectureValues,
    _iImageType :: !ImageTypeValues,
    _iRootDeviceType :: !DeviceType,
    _iVirtualizationType :: !VirtualizationType,
    _iHypervisor :: !HypervisorType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Image' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iPlatform' - This value is set to @windows@ for Windows AMIs; otherwise, it is blank.
--
-- * 'iPlatformDetails' - The platform details associated with the billing code of the AMI. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ami-billing-info.html Obtaining Billing Information> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- * 'iEnaSupport' - Specifies whether enhanced networking with ENA is enabled.
--
-- * 'iImageOwnerAlias' - The AWS account alias (for example, @amazon@ , @self@ ) or the AWS account ID of the AMI owner.
--
-- * 'iUsageOperation' - The operation of the Amazon EC2 instance and the billing code that is associated with the AMI. @usageOperation@ corresponds to the <https://docs.aws.amazon.com/cur/latest/userguide/Lineitem-columns.html#Lineitem-details-O-Operation lineitem/Operation> column on your AWS Cost and Usage Report and in the <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/price-changes.html AWS Price List API> . For the list of @UsageOperation@ codes, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ami-billing-info.html#billing-info Platform Details and Usage Operation Billing Codes> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- * 'iRAMDiskId' - The RAM disk associated with the image, if any. Only applicable for machine images.
--
-- * 'iKernelId' - The kernel associated with the image, if any. Only applicable for machine images.
--
-- * 'iRootDeviceName' - The device name of the root device volume (for example, @/dev/sda1@ ).
--
-- * 'iSRIOVNetSupport' - Specifies whether enhanced networking with the Intel 82599 Virtual Function interface is enabled.
--
-- * 'iName' - The name of the AMI that was provided during image creation.
--
-- * 'iCreationDate' - The date and time the image was created.
--
-- * 'iProductCodes' - Any product codes associated with the AMI.
--
-- * 'iStateReason' - The reason for the state change.
--
-- * 'iDescription' - The description of the AMI that was provided during image creation.
--
-- * 'iBlockDeviceMappings' - Any block device mapping entries.
--
-- * 'iTags' - Any tags assigned to the image.
--
-- * 'iImageId' - The ID of the AMI.
--
-- * 'iImageLocation' - The location of the AMI.
--
-- * 'iState' - The current state of the AMI. If the state is @available@ , the image is successfully registered and can be used to launch an instance.
--
-- * 'iOwnerId' - The AWS account ID of the image owner.
--
-- * 'iPublic' - Indicates whether the image has public launch permissions. The value is @true@ if this image has public launch permissions or @false@ if it has only implicit and explicit launch permissions.
--
-- * 'iArchitecture' - The architecture of the image.
--
-- * 'iImageType' - The type of image.
--
-- * 'iRootDeviceType' - The type of root device used by the AMI. The AMI can use an EBS volume or an instance store volume.
--
-- * 'iVirtualizationType' - The type of virtualization of the AMI.
--
-- * 'iHypervisor' - The hypervisor type of the image.
image ::
  -- | 'iImageId'
  Text ->
  -- | 'iImageLocation'
  Text ->
  -- | 'iState'
  ImageState ->
  -- | 'iOwnerId'
  Text ->
  -- | 'iPublic'
  Bool ->
  -- | 'iArchitecture'
  ArchitectureValues ->
  -- | 'iImageType'
  ImageTypeValues ->
  -- | 'iRootDeviceType'
  DeviceType ->
  -- | 'iVirtualizationType'
  VirtualizationType ->
  -- | 'iHypervisor'
  HypervisorType ->
  Image
image
  pImageId_
  pImageLocation_
  pState_
  pOwnerId_
  pPublic_
  pArchitecture_
  pImageType_
  pRootDeviceType_
  pVirtualizationType_
  pHypervisor_ =
    Image'
      { _iPlatform = Nothing,
        _iPlatformDetails = Nothing,
        _iEnaSupport = Nothing,
        _iImageOwnerAlias = Nothing,
        _iUsageOperation = Nothing,
        _iRAMDiskId = Nothing,
        _iKernelId = Nothing,
        _iRootDeviceName = Nothing,
        _iSRIOVNetSupport = Nothing,
        _iName = Nothing,
        _iCreationDate = Nothing,
        _iProductCodes = Nothing,
        _iStateReason = Nothing,
        _iDescription = Nothing,
        _iBlockDeviceMappings = Nothing,
        _iTags = Nothing,
        _iImageId = pImageId_,
        _iImageLocation = pImageLocation_,
        _iState = pState_,
        _iOwnerId = pOwnerId_,
        _iPublic = pPublic_,
        _iArchitecture = pArchitecture_,
        _iImageType = pImageType_,
        _iRootDeviceType = pRootDeviceType_,
        _iVirtualizationType = pVirtualizationType_,
        _iHypervisor = pHypervisor_
      }

-- | This value is set to @windows@ for Windows AMIs; otherwise, it is blank.
iPlatform :: Lens' Image (Maybe PlatformValues)
iPlatform = lens _iPlatform (\s a -> s {_iPlatform = a})

-- | The platform details associated with the billing code of the AMI. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ami-billing-info.html Obtaining Billing Information> in the /Amazon Elastic Compute Cloud User Guide/ .
iPlatformDetails :: Lens' Image (Maybe Text)
iPlatformDetails = lens _iPlatformDetails (\s a -> s {_iPlatformDetails = a})

-- | Specifies whether enhanced networking with ENA is enabled.
iEnaSupport :: Lens' Image (Maybe Bool)
iEnaSupport = lens _iEnaSupport (\s a -> s {_iEnaSupport = a})

-- | The AWS account alias (for example, @amazon@ , @self@ ) or the AWS account ID of the AMI owner.
iImageOwnerAlias :: Lens' Image (Maybe Text)
iImageOwnerAlias = lens _iImageOwnerAlias (\s a -> s {_iImageOwnerAlias = a})

-- | The operation of the Amazon EC2 instance and the billing code that is associated with the AMI. @usageOperation@ corresponds to the <https://docs.aws.amazon.com/cur/latest/userguide/Lineitem-columns.html#Lineitem-details-O-Operation lineitem/Operation> column on your AWS Cost and Usage Report and in the <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/price-changes.html AWS Price List API> . For the list of @UsageOperation@ codes, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ami-billing-info.html#billing-info Platform Details and Usage Operation Billing Codes> in the /Amazon Elastic Compute Cloud User Guide/ .
iUsageOperation :: Lens' Image (Maybe Text)
iUsageOperation = lens _iUsageOperation (\s a -> s {_iUsageOperation = a})

-- | The RAM disk associated with the image, if any. Only applicable for machine images.
iRAMDiskId :: Lens' Image (Maybe Text)
iRAMDiskId = lens _iRAMDiskId (\s a -> s {_iRAMDiskId = a})

-- | The kernel associated with the image, if any. Only applicable for machine images.
iKernelId :: Lens' Image (Maybe Text)
iKernelId = lens _iKernelId (\s a -> s {_iKernelId = a})

-- | The device name of the root device volume (for example, @/dev/sda1@ ).
iRootDeviceName :: Lens' Image (Maybe Text)
iRootDeviceName = lens _iRootDeviceName (\s a -> s {_iRootDeviceName = a})

-- | Specifies whether enhanced networking with the Intel 82599 Virtual Function interface is enabled.
iSRIOVNetSupport :: Lens' Image (Maybe Text)
iSRIOVNetSupport = lens _iSRIOVNetSupport (\s a -> s {_iSRIOVNetSupport = a})

-- | The name of the AMI that was provided during image creation.
iName :: Lens' Image (Maybe Text)
iName = lens _iName (\s a -> s {_iName = a})

-- | The date and time the image was created.
iCreationDate :: Lens' Image (Maybe Text)
iCreationDate = lens _iCreationDate (\s a -> s {_iCreationDate = a})

-- | Any product codes associated with the AMI.
iProductCodes :: Lens' Image [ProductCode]
iProductCodes = lens _iProductCodes (\s a -> s {_iProductCodes = a}) . _Default . _Coerce

-- | The reason for the state change.
iStateReason :: Lens' Image (Maybe StateReason)
iStateReason = lens _iStateReason (\s a -> s {_iStateReason = a})

-- | The description of the AMI that was provided during image creation.
iDescription :: Lens' Image (Maybe Text)
iDescription = lens _iDescription (\s a -> s {_iDescription = a})

-- | Any block device mapping entries.
iBlockDeviceMappings :: Lens' Image [BlockDeviceMapping]
iBlockDeviceMappings = lens _iBlockDeviceMappings (\s a -> s {_iBlockDeviceMappings = a}) . _Default . _Coerce

-- | Any tags assigned to the image.
iTags :: Lens' Image [Tag]
iTags = lens _iTags (\s a -> s {_iTags = a}) . _Default . _Coerce

-- | The ID of the AMI.
iImageId :: Lens' Image Text
iImageId = lens _iImageId (\s a -> s {_iImageId = a})

-- | The location of the AMI.
iImageLocation :: Lens' Image Text
iImageLocation = lens _iImageLocation (\s a -> s {_iImageLocation = a})

-- | The current state of the AMI. If the state is @available@ , the image is successfully registered and can be used to launch an instance.
iState :: Lens' Image ImageState
iState = lens _iState (\s a -> s {_iState = a})

-- | The AWS account ID of the image owner.
iOwnerId :: Lens' Image Text
iOwnerId = lens _iOwnerId (\s a -> s {_iOwnerId = a})

-- | Indicates whether the image has public launch permissions. The value is @true@ if this image has public launch permissions or @false@ if it has only implicit and explicit launch permissions.
iPublic :: Lens' Image Bool
iPublic = lens _iPublic (\s a -> s {_iPublic = a})

-- | The architecture of the image.
iArchitecture :: Lens' Image ArchitectureValues
iArchitecture = lens _iArchitecture (\s a -> s {_iArchitecture = a})

-- | The type of image.
iImageType :: Lens' Image ImageTypeValues
iImageType = lens _iImageType (\s a -> s {_iImageType = a})

-- | The type of root device used by the AMI. The AMI can use an EBS volume or an instance store volume.
iRootDeviceType :: Lens' Image DeviceType
iRootDeviceType = lens _iRootDeviceType (\s a -> s {_iRootDeviceType = a})

-- | The type of virtualization of the AMI.
iVirtualizationType :: Lens' Image VirtualizationType
iVirtualizationType = lens _iVirtualizationType (\s a -> s {_iVirtualizationType = a})

-- | The hypervisor type of the image.
iHypervisor :: Lens' Image HypervisorType
iHypervisor = lens _iHypervisor (\s a -> s {_iHypervisor = a})

instance FromXML Image where
  parseXML x =
    Image'
      <$> (x .@? "platform")
      <*> (x .@? "platformDetails")
      <*> (x .@? "enaSupport")
      <*> (x .@? "imageOwnerAlias")
      <*> (x .@? "usageOperation")
      <*> (x .@? "ramdiskId")
      <*> (x .@? "kernelId")
      <*> (x .@? "rootDeviceName")
      <*> (x .@? "sriovNetSupport")
      <*> (x .@? "name")
      <*> (x .@? "creationDate")
      <*> (x .@? "productCodes" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@? "stateReason")
      <*> (x .@? "description")
      <*> ( x .@? "blockDeviceMapping" .!@ mempty
              >>= may (parseXMLList "item")
          )
      <*> (x .@? "tagSet" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@ "imageId")
      <*> (x .@ "imageLocation")
      <*> (x .@ "imageState")
      <*> (x .@ "imageOwnerId")
      <*> (x .@ "isPublic")
      <*> (x .@ "architecture")
      <*> (x .@ "imageType")
      <*> (x .@ "rootDeviceType")
      <*> (x .@ "virtualizationType")
      <*> (x .@ "hypervisor")

instance Hashable Image

instance NFData Image
