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
-- Module      : Network.AWS.EC2.ModifyInstanceAttribute
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified attribute of the specified instance. You can specify only one attribute at a time.
--
--
-- __Note: __ Using this action to change the security groups associated with an elastic network interface (ENI) attached to an instance in a VPC can result in an error if the instance has more than one ENI. To change the security groups associated with an ENI attached to an instance that has multiple ENIs, we recommend that you use the 'ModifyNetworkInterfaceAttribute' action.
--
-- To modify some attributes, the instance must be stopped. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_ChangingAttributesWhileInstanceStopped.html Modifying Attributes of a Stopped Instance> in the /Amazon Elastic Compute Cloud User Guide/ .
--
module Network.AWS.EC2.ModifyInstanceAttribute
    (
    -- * Creating a Request
      modifyInstanceAttribute
    , ModifyInstanceAttribute
    -- * Request Lenses
    , mGroups
    , mAttribute
    , mEnaSupport
    , mSourceDestCheck
    , mDisableAPITermination
    , mKernel
    , mRAMDisk
    , mValue
    , mInstanceType
    , mSRIOVNetSupport
    , mEBSOptimized
    , mUserData
    , mInstanceInitiatedShutdownBehavior
    , mBlockDeviceMappings
    , mDryRun
    , mInstanceId

    -- * Destructuring the Response
    , modifyInstanceAttributeResponse
    , ModifyInstanceAttributeResponse
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for ModifyInstanceAttribute.
--
--
--
-- /See:/ 'modifyInstanceAttribute' smart constructor.
data ModifyInstanceAttribute = ModifyInstanceAttribute'
  { _mGroups :: !(Maybe [Text])
  , _mAttribute :: !(Maybe InstanceAttributeName)
  , _mEnaSupport :: !(Maybe AttributeBooleanValue)
  , _mSourceDestCheck :: !(Maybe AttributeBooleanValue)
  , _mDisableAPITermination :: !(Maybe AttributeBooleanValue)
  , _mKernel :: !(Maybe AttributeValue)
  , _mRAMDisk :: !(Maybe AttributeValue)
  , _mValue :: !(Maybe Text)
  , _mInstanceType :: !(Maybe AttributeValue)
  , _mSRIOVNetSupport :: !(Maybe AttributeValue)
  , _mEBSOptimized :: !(Maybe AttributeBooleanValue)
  , _mUserData :: !(Maybe BlobAttributeValue)
  , _mInstanceInitiatedShutdownBehavior :: !(Maybe AttributeValue)
  , _mBlockDeviceMappings :: !(Maybe [InstanceBlockDeviceMappingSpecification])
  , _mDryRun :: !(Maybe Bool)
  , _mInstanceId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyInstanceAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mGroups' - [EC2-VPC] Changes the security groups of the instance. You must specify at least one security group, even if it's just the default security group for the VPC. You must specify the security group ID, not the security group name.
--
-- * 'mAttribute' - The name of the attribute.
--
-- * 'mEnaSupport' - Set to @true@ to enable enhanced networking with ENA for the instance. This option is supported only for HVM instances. Specifying this option with a PV instance can make it unreachable.
--
-- * 'mSourceDestCheck' - Specifies whether source/destination checking is enabled. A value of @true@ means that checking is enabled, and @false@ means that checking is disabled. This value must be @false@ for a NAT instance to perform NAT.
--
-- * 'mDisableAPITermination' - If the value is @true@ , you can't terminate the instance using the Amazon EC2 console, CLI, or API; otherwise, you can. You cannot use this parameter for Spot Instances.
--
-- * 'mKernel' - Changes the instance's kernel to the specified value. We recommend that you use PV-GRUB instead of kernels and RAM disks. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedKernels.html PV-GRUB> .
--
-- * 'mRAMDisk' - Changes the instance's RAM disk to the specified value. We recommend that you use PV-GRUB instead of kernels and RAM disks. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedKernels.html PV-GRUB> .
--
-- * 'mValue' - A new value for the attribute. Use only with the @kernel@ , @ramdisk@ , @userData@ , @disableApiTermination@ , or @instanceInitiatedShutdownBehavior@ attribute.
--
-- * 'mInstanceType' - Changes the instance type to the specified value. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types> . If the instance type is not valid, the error returned is @InvalidInstanceAttributeValue@ .
--
-- * 'mSRIOVNetSupport' - Set to @simple@ to enable enhanced networking with the Intel 82599 Virtual Function interface for the instance. There is no way to disable enhanced networking with the Intel 82599 Virtual Function interface at this time. This option is supported only for HVM instances. Specifying this option with a PV instance can make it unreachable.
--
-- * 'mEBSOptimized' - Specifies whether the instance is optimized for Amazon EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal EBS I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS Optimized instance.
--
-- * 'mUserData' - Changes the instance's user data to the specified value. If you are using an AWS SDK or command line tool, base64-encoding is performed for you, and you can load the text from a file. Otherwise, you must provide base64-encoded text.
--
-- * 'mInstanceInitiatedShutdownBehavior' - Specifies whether an instance stops or terminates when you initiate shutdown from the instance (using the operating system command for system shutdown).
--
-- * 'mBlockDeviceMappings' - Modifies the @DeleteOnTermination@ attribute for volumes that are currently attached. The volume must be owned by the caller. If no value is specified for @DeleteOnTermination@ , the default is @true@ and the volume is deleted when the instance is terminated. To add instance store volumes to an Amazon EBS-backed instance, you must add them when you launch the instance. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/block-device-mapping-concepts.html#Using_OverridingAMIBDM Updating the Block Device Mapping when Launching an Instance> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- * 'mDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'mInstanceId' - The ID of the instance.
modifyInstanceAttribute
    :: Text -- ^ 'mInstanceId'
    -> ModifyInstanceAttribute
modifyInstanceAttribute pInstanceId_ =
  ModifyInstanceAttribute'
    { _mGroups = Nothing
    , _mAttribute = Nothing
    , _mEnaSupport = Nothing
    , _mSourceDestCheck = Nothing
    , _mDisableAPITermination = Nothing
    , _mKernel = Nothing
    , _mRAMDisk = Nothing
    , _mValue = Nothing
    , _mInstanceType = Nothing
    , _mSRIOVNetSupport = Nothing
    , _mEBSOptimized = Nothing
    , _mUserData = Nothing
    , _mInstanceInitiatedShutdownBehavior = Nothing
    , _mBlockDeviceMappings = Nothing
    , _mDryRun = Nothing
    , _mInstanceId = pInstanceId_
    }


-- | [EC2-VPC] Changes the security groups of the instance. You must specify at least one security group, even if it's just the default security group for the VPC. You must specify the security group ID, not the security group name.
mGroups :: Lens' ModifyInstanceAttribute [Text]
mGroups = lens _mGroups (\ s a -> s{_mGroups = a}) . _Default . _Coerce

-- | The name of the attribute.
mAttribute :: Lens' ModifyInstanceAttribute (Maybe InstanceAttributeName)
mAttribute = lens _mAttribute (\ s a -> s{_mAttribute = a})

-- | Set to @true@ to enable enhanced networking with ENA for the instance. This option is supported only for HVM instances. Specifying this option with a PV instance can make it unreachable.
mEnaSupport :: Lens' ModifyInstanceAttribute (Maybe AttributeBooleanValue)
mEnaSupport = lens _mEnaSupport (\ s a -> s{_mEnaSupport = a})

-- | Specifies whether source/destination checking is enabled. A value of @true@ means that checking is enabled, and @false@ means that checking is disabled. This value must be @false@ for a NAT instance to perform NAT.
mSourceDestCheck :: Lens' ModifyInstanceAttribute (Maybe AttributeBooleanValue)
mSourceDestCheck = lens _mSourceDestCheck (\ s a -> s{_mSourceDestCheck = a})

-- | If the value is @true@ , you can't terminate the instance using the Amazon EC2 console, CLI, or API; otherwise, you can. You cannot use this parameter for Spot Instances.
mDisableAPITermination :: Lens' ModifyInstanceAttribute (Maybe AttributeBooleanValue)
mDisableAPITermination = lens _mDisableAPITermination (\ s a -> s{_mDisableAPITermination = a})

-- | Changes the instance's kernel to the specified value. We recommend that you use PV-GRUB instead of kernels and RAM disks. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedKernels.html PV-GRUB> .
mKernel :: Lens' ModifyInstanceAttribute (Maybe AttributeValue)
mKernel = lens _mKernel (\ s a -> s{_mKernel = a})

-- | Changes the instance's RAM disk to the specified value. We recommend that you use PV-GRUB instead of kernels and RAM disks. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedKernels.html PV-GRUB> .
mRAMDisk :: Lens' ModifyInstanceAttribute (Maybe AttributeValue)
mRAMDisk = lens _mRAMDisk (\ s a -> s{_mRAMDisk = a})

-- | A new value for the attribute. Use only with the @kernel@ , @ramdisk@ , @userData@ , @disableApiTermination@ , or @instanceInitiatedShutdownBehavior@ attribute.
mValue :: Lens' ModifyInstanceAttribute (Maybe Text)
mValue = lens _mValue (\ s a -> s{_mValue = a})

-- | Changes the instance type to the specified value. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types> . If the instance type is not valid, the error returned is @InvalidInstanceAttributeValue@ .
mInstanceType :: Lens' ModifyInstanceAttribute (Maybe AttributeValue)
mInstanceType = lens _mInstanceType (\ s a -> s{_mInstanceType = a})

-- | Set to @simple@ to enable enhanced networking with the Intel 82599 Virtual Function interface for the instance. There is no way to disable enhanced networking with the Intel 82599 Virtual Function interface at this time. This option is supported only for HVM instances. Specifying this option with a PV instance can make it unreachable.
mSRIOVNetSupport :: Lens' ModifyInstanceAttribute (Maybe AttributeValue)
mSRIOVNetSupport = lens _mSRIOVNetSupport (\ s a -> s{_mSRIOVNetSupport = a})

-- | Specifies whether the instance is optimized for Amazon EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal EBS I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS Optimized instance.
mEBSOptimized :: Lens' ModifyInstanceAttribute (Maybe AttributeBooleanValue)
mEBSOptimized = lens _mEBSOptimized (\ s a -> s{_mEBSOptimized = a})

-- | Changes the instance's user data to the specified value. If you are using an AWS SDK or command line tool, base64-encoding is performed for you, and you can load the text from a file. Otherwise, you must provide base64-encoded text.
mUserData :: Lens' ModifyInstanceAttribute (Maybe BlobAttributeValue)
mUserData = lens _mUserData (\ s a -> s{_mUserData = a})

-- | Specifies whether an instance stops or terminates when you initiate shutdown from the instance (using the operating system command for system shutdown).
mInstanceInitiatedShutdownBehavior :: Lens' ModifyInstanceAttribute (Maybe AttributeValue)
mInstanceInitiatedShutdownBehavior = lens _mInstanceInitiatedShutdownBehavior (\ s a -> s{_mInstanceInitiatedShutdownBehavior = a})

-- | Modifies the @DeleteOnTermination@ attribute for volumes that are currently attached. The volume must be owned by the caller. If no value is specified for @DeleteOnTermination@ , the default is @true@ and the volume is deleted when the instance is terminated. To add instance store volumes to an Amazon EBS-backed instance, you must add them when you launch the instance. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/block-device-mapping-concepts.html#Using_OverridingAMIBDM Updating the Block Device Mapping when Launching an Instance> in the /Amazon Elastic Compute Cloud User Guide/ .
mBlockDeviceMappings :: Lens' ModifyInstanceAttribute [InstanceBlockDeviceMappingSpecification]
mBlockDeviceMappings = lens _mBlockDeviceMappings (\ s a -> s{_mBlockDeviceMappings = a}) . _Default . _Coerce

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mDryRun :: Lens' ModifyInstanceAttribute (Maybe Bool)
mDryRun = lens _mDryRun (\ s a -> s{_mDryRun = a})

-- | The ID of the instance.
mInstanceId :: Lens' ModifyInstanceAttribute Text
mInstanceId = lens _mInstanceId (\ s a -> s{_mInstanceId = a})

instance AWSRequest ModifyInstanceAttribute where
        type Rs ModifyInstanceAttribute =
             ModifyInstanceAttributeResponse
        request = postQuery ec2
        response
          = receiveNull ModifyInstanceAttributeResponse'

instance Hashable ModifyInstanceAttribute where

instance NFData ModifyInstanceAttribute where

instance ToHeaders ModifyInstanceAttribute where
        toHeaders = const mempty

instance ToPath ModifyInstanceAttribute where
        toPath = const "/"

instance ToQuery ModifyInstanceAttribute where
        toQuery ModifyInstanceAttribute'{..}
          = mconcat
              ["Action" =:
                 ("ModifyInstanceAttribute" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery (toQueryList "GroupId" <$> _mGroups),
               "Attribute" =: _mAttribute,
               "EnaSupport" =: _mEnaSupport,
               "SourceDestCheck" =: _mSourceDestCheck,
               "DisableApiTermination" =: _mDisableAPITermination,
               "Kernel" =: _mKernel, "Ramdisk" =: _mRAMDisk,
               "Value" =: _mValue, "InstanceType" =: _mInstanceType,
               "SriovNetSupport" =: _mSRIOVNetSupport,
               "EbsOptimized" =: _mEBSOptimized,
               "UserData" =: _mUserData,
               "InstanceInitiatedShutdownBehavior" =:
                 _mInstanceInitiatedShutdownBehavior,
               toQuery
                 (toQueryList "BlockDeviceMapping" <$>
                    _mBlockDeviceMappings),
               "DryRun" =: _mDryRun, "InstanceId" =: _mInstanceId]

-- | /See:/ 'modifyInstanceAttributeResponse' smart constructor.
data ModifyInstanceAttributeResponse =
  ModifyInstanceAttributeResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyInstanceAttributeResponse' with the minimum fields required to make a request.
--
modifyInstanceAttributeResponse
    :: ModifyInstanceAttributeResponse
modifyInstanceAttributeResponse = ModifyInstanceAttributeResponse'


instance NFData ModifyInstanceAttributeResponse where
