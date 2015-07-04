{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.EC2.ModifyInstanceAttribute
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Modifies the specified attribute of the specified instance. You can
-- specify only one attribute at a time.
--
-- To modify some attributes, the instance must be stopped. For more
-- information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_ChangingAttributesWhileInstanceStopped.html Modifying Attributes of a Stopped Instance>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ModifyInstanceAttribute.html>
module Network.AWS.EC2.ModifyInstanceAttribute
    (
    -- * Request
      ModifyInstanceAttribute
    -- ** Request constructor
    , modifyInstanceAttribute
    -- ** Request lenses
    , modAttribute
    , modGroups
    , modSourceDestCheck
    , modDisableAPITermination
    , modRAMDisk
    , modValue
    , modKernel
    , modInstanceType
    , modEBSOptimized
    , modUserData
    , modSRIOVNetSupport
    , modInstanceInitiatedShutdownBehavior
    , modBlockDeviceMappings
    , modDryRun
    , modInstanceId

    -- * Response
    , ModifyInstanceAttributeResponse
    -- ** Response constructor
    , modifyInstanceAttributeResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'modifyInstanceAttribute' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'modAttribute'
--
-- * 'modGroups'
--
-- * 'modSourceDestCheck'
--
-- * 'modDisableAPITermination'
--
-- * 'modRAMDisk'
--
-- * 'modValue'
--
-- * 'modKernel'
--
-- * 'modInstanceType'
--
-- * 'modEBSOptimized'
--
-- * 'modUserData'
--
-- * 'modSRIOVNetSupport'
--
-- * 'modInstanceInitiatedShutdownBehavior'
--
-- * 'modBlockDeviceMappings'
--
-- * 'modDryRun'
--
-- * 'modInstanceId'
data ModifyInstanceAttribute = ModifyInstanceAttribute'
    { _modAttribute                         :: !(Maybe InstanceAttributeName)
    , _modGroups                            :: !(Maybe [Text])
    , _modSourceDestCheck                   :: !(Maybe AttributeBooleanValue)
    , _modDisableAPITermination             :: !(Maybe AttributeBooleanValue)
    , _modRAMDisk                           :: !(Maybe AttributeValue)
    , _modValue                             :: !(Maybe Text)
    , _modKernel                            :: !(Maybe AttributeValue)
    , _modInstanceType                      :: !(Maybe AttributeValue)
    , _modEBSOptimized                      :: !(Maybe AttributeBooleanValue)
    , _modUserData                          :: !(Maybe BlobAttributeValue)
    , _modSRIOVNetSupport                   :: !(Maybe AttributeValue)
    , _modInstanceInitiatedShutdownBehavior :: !(Maybe AttributeValue)
    , _modBlockDeviceMappings               :: !(Maybe [InstanceBlockDeviceMappingSpecification])
    , _modDryRun                            :: !(Maybe Bool)
    , _modInstanceId                        :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ModifyInstanceAttribute' smart constructor.
modifyInstanceAttribute :: Text -> ModifyInstanceAttribute
modifyInstanceAttribute pInstanceId =
    ModifyInstanceAttribute'
    { _modAttribute = Nothing
    , _modGroups = Nothing
    , _modSourceDestCheck = Nothing
    , _modDisableAPITermination = Nothing
    , _modRAMDisk = Nothing
    , _modValue = Nothing
    , _modKernel = Nothing
    , _modInstanceType = Nothing
    , _modEBSOptimized = Nothing
    , _modUserData = Nothing
    , _modSRIOVNetSupport = Nothing
    , _modInstanceInitiatedShutdownBehavior = Nothing
    , _modBlockDeviceMappings = Nothing
    , _modDryRun = Nothing
    , _modInstanceId = pInstanceId
    }

-- | The name of the attribute.
modAttribute :: Lens' ModifyInstanceAttribute (Maybe InstanceAttributeName)
modAttribute = lens _modAttribute (\ s a -> s{_modAttribute = a});

-- | [EC2-VPC] Changes the security groups of the instance. You must specify
-- at least one security group, even if it\'s just the default security
-- group for the VPC. You must specify the security group ID, not the
-- security group name.
modGroups :: Lens' ModifyInstanceAttribute [Text]
modGroups = lens _modGroups (\ s a -> s{_modGroups = a}) . _Default;

-- | Specifies whether source\/destination checking is enabled. A value of
-- @true@ means that checking is enabled, and @false@ means checking is
-- disabled. This value must be @false@ for a NAT instance to perform NAT.
modSourceDestCheck :: Lens' ModifyInstanceAttribute (Maybe AttributeBooleanValue)
modSourceDestCheck = lens _modSourceDestCheck (\ s a -> s{_modSourceDestCheck = a});

-- | If the value is @true@, you can\'t terminate the instance using the
-- Amazon EC2 console, CLI, or API; otherwise, you can.
modDisableAPITermination :: Lens' ModifyInstanceAttribute (Maybe AttributeBooleanValue)
modDisableAPITermination = lens _modDisableAPITermination (\ s a -> s{_modDisableAPITermination = a});

-- | Changes the instance\'s RAM disk to the specified value. We recommend
-- that you use PV-GRUB instead of kernels and RAM disks. For more
-- information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedKernels.html PV-GRUB>.
modRAMDisk :: Lens' ModifyInstanceAttribute (Maybe AttributeValue)
modRAMDisk = lens _modRAMDisk (\ s a -> s{_modRAMDisk = a});

-- | A new value for the attribute. Use only with the @kernel@, @ramdisk@,
-- @userData@, @disableApiTermination@, or
-- @intanceInitiateShutdownBehavior@ attribute.
modValue :: Lens' ModifyInstanceAttribute (Maybe Text)
modValue = lens _modValue (\ s a -> s{_modValue = a});

-- | Changes the instance\'s kernel to the specified value. We recommend that
-- you use PV-GRUB instead of kernels and RAM disks. For more information,
-- see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedKernels.html PV-GRUB>.
modKernel :: Lens' ModifyInstanceAttribute (Maybe AttributeValue)
modKernel = lens _modKernel (\ s a -> s{_modKernel = a});

-- | Changes the instance type to the specified value. For more information,
-- see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types>.
-- If the instance type is not valid, the error returned is
-- @InvalidInstanceAttributeValue@.
modInstanceType :: Lens' ModifyInstanceAttribute (Maybe AttributeValue)
modInstanceType = lens _modInstanceType (\ s a -> s{_modInstanceType = a});

-- | Specifies whether the instance is optimized for EBS I\/O. This
-- optimization provides dedicated throughput to Amazon EBS and an
-- optimized configuration stack to provide optimal EBS I\/O performance.
-- This optimization isn\'t available with all instance types. Additional
-- usage charges apply when using an EBS Optimized instance.
modEBSOptimized :: Lens' ModifyInstanceAttribute (Maybe AttributeBooleanValue)
modEBSOptimized = lens _modEBSOptimized (\ s a -> s{_modEBSOptimized = a});

-- | Changes the instance\'s user data to the specified value.
modUserData :: Lens' ModifyInstanceAttribute (Maybe BlobAttributeValue)
modUserData = lens _modUserData (\ s a -> s{_modUserData = a});

-- | Set to @simple@ to enable enhanced networking for the instance.
--
-- There is no way to disable enhanced networking at this time.
--
-- This option is supported only for HVM instances. Specifying this option
-- with a PV instance can make it unreachable.
modSRIOVNetSupport :: Lens' ModifyInstanceAttribute (Maybe AttributeValue)
modSRIOVNetSupport = lens _modSRIOVNetSupport (\ s a -> s{_modSRIOVNetSupport = a});

-- | Specifies whether an instance stops or terminates when you initiate
-- shutdown from the instance (using the operating system command for
-- system shutdown).
modInstanceInitiatedShutdownBehavior :: Lens' ModifyInstanceAttribute (Maybe AttributeValue)
modInstanceInitiatedShutdownBehavior = lens _modInstanceInitiatedShutdownBehavior (\ s a -> s{_modInstanceInitiatedShutdownBehavior = a});

-- | Modifies the @DeleteOnTermination@ attribute for volumes that are
-- currently attached. The volume must be owned by the caller. If no value
-- is specified for @DeleteOnTermination@, the default is @true@ and the
-- volume is deleted when the instance is terminated.
--
-- To add instance store volumes to an Amazon EBS-backed instance, you must
-- add them when you launch the instance. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/block-device-mapping-concepts.html#Using_OverridingAMIBDM Updating the Block Device Mapping when Launching an Instance>
-- in the /Amazon Elastic Compute Cloud User Guide/.
modBlockDeviceMappings :: Lens' ModifyInstanceAttribute [InstanceBlockDeviceMappingSpecification]
modBlockDeviceMappings = lens _modBlockDeviceMappings (\ s a -> s{_modBlockDeviceMappings = a}) . _Default;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modDryRun :: Lens' ModifyInstanceAttribute (Maybe Bool)
modDryRun = lens _modDryRun (\ s a -> s{_modDryRun = a});

-- | The ID of the instance.
modInstanceId :: Lens' ModifyInstanceAttribute Text
modInstanceId = lens _modInstanceId (\ s a -> s{_modInstanceId = a});

instance AWSRequest ModifyInstanceAttribute where
        type Sv ModifyInstanceAttribute = EC2
        type Rs ModifyInstanceAttribute =
             ModifyInstanceAttributeResponse
        request = post
        response
          = receiveNull ModifyInstanceAttributeResponse'

instance ToHeaders ModifyInstanceAttribute where
        toHeaders = const mempty

instance ToPath ModifyInstanceAttribute where
        toPath = const "/"

instance ToQuery ModifyInstanceAttribute where
        toQuery ModifyInstanceAttribute'{..}
          = mconcat
              ["Action" =:
                 ("ModifyInstanceAttribute" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "Attribute" =: _modAttribute,
               toQuery (toQueryList "groupId" <$> _modGroups),
               "SourceDestCheck" =: _modSourceDestCheck,
               "DisableApiTermination" =: _modDisableAPITermination,
               "Ramdisk" =: _modRAMDisk, "Value" =: _modValue,
               "Kernel" =: _modKernel,
               "InstanceType" =: _modInstanceType,
               "EbsOptimized" =: _modEBSOptimized,
               "UserData" =: _modUserData,
               "SriovNetSupport" =: _modSRIOVNetSupport,
               "InstanceInitiatedShutdownBehavior" =:
                 _modInstanceInitiatedShutdownBehavior,
               toQuery
                 (toQueryList "item" <$> _modBlockDeviceMappings),
               "DryRun" =: _modDryRun,
               "InstanceId" =: _modInstanceId]

-- | /See:/ 'modifyInstanceAttributeResponse' smart constructor.
data ModifyInstanceAttributeResponse =
    ModifyInstanceAttributeResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ModifyInstanceAttributeResponse' smart constructor.
modifyInstanceAttributeResponse :: ModifyInstanceAttributeResponse
modifyInstanceAttributeResponse = ModifyInstanceAttributeResponse'
