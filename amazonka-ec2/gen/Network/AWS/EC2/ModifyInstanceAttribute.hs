{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyInstanceAttribute
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified attribute of the specified instance. You can
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
    , mrqAttribute
    , mrqGroups
    , mrqSourceDestCheck
    , mrqDisableAPITermination
    , mrqRAMDisk
    , mrqValue
    , mrqKernel
    , mrqInstanceType
    , mrqEBSOptimized
    , mrqUserData
    , mrqSRIOVNetSupport
    , mrqInstanceInitiatedShutdownBehavior
    , mrqBlockDeviceMappings
    , mrqDryRun
    , mrqInstanceId

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
-- * 'mrqAttribute'
--
-- * 'mrqGroups'
--
-- * 'mrqSourceDestCheck'
--
-- * 'mrqDisableAPITermination'
--
-- * 'mrqRAMDisk'
--
-- * 'mrqValue'
--
-- * 'mrqKernel'
--
-- * 'mrqInstanceType'
--
-- * 'mrqEBSOptimized'
--
-- * 'mrqUserData'
--
-- * 'mrqSRIOVNetSupport'
--
-- * 'mrqInstanceInitiatedShutdownBehavior'
--
-- * 'mrqBlockDeviceMappings'
--
-- * 'mrqDryRun'
--
-- * 'mrqInstanceId'
data ModifyInstanceAttribute = ModifyInstanceAttribute'
    { _mrqAttribute                         :: !(Maybe InstanceAttributeName)
    , _mrqGroups                            :: !(Maybe [Text])
    , _mrqSourceDestCheck                   :: !(Maybe AttributeBooleanValue)
    , _mrqDisableAPITermination             :: !(Maybe AttributeBooleanValue)
    , _mrqRAMDisk                           :: !(Maybe AttributeValue)
    , _mrqValue                             :: !(Maybe Text)
    , _mrqKernel                            :: !(Maybe AttributeValue)
    , _mrqInstanceType                      :: !(Maybe AttributeValue)
    , _mrqEBSOptimized                      :: !(Maybe AttributeBooleanValue)
    , _mrqUserData                          :: !(Maybe BlobAttributeValue)
    , _mrqSRIOVNetSupport                   :: !(Maybe AttributeValue)
    , _mrqInstanceInitiatedShutdownBehavior :: !(Maybe AttributeValue)
    , _mrqBlockDeviceMappings               :: !(Maybe [InstanceBlockDeviceMappingSpecification])
    , _mrqDryRun                            :: !(Maybe Bool)
    , _mrqInstanceId                        :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ModifyInstanceAttribute' smart constructor.
modifyInstanceAttribute :: Text -> ModifyInstanceAttribute
modifyInstanceAttribute pInstanceId_ =
    ModifyInstanceAttribute'
    { _mrqAttribute = Nothing
    , _mrqGroups = Nothing
    , _mrqSourceDestCheck = Nothing
    , _mrqDisableAPITermination = Nothing
    , _mrqRAMDisk = Nothing
    , _mrqValue = Nothing
    , _mrqKernel = Nothing
    , _mrqInstanceType = Nothing
    , _mrqEBSOptimized = Nothing
    , _mrqUserData = Nothing
    , _mrqSRIOVNetSupport = Nothing
    , _mrqInstanceInitiatedShutdownBehavior = Nothing
    , _mrqBlockDeviceMappings = Nothing
    , _mrqDryRun = Nothing
    , _mrqInstanceId = pInstanceId_
    }

-- | The name of the attribute.
mrqAttribute :: Lens' ModifyInstanceAttribute (Maybe InstanceAttributeName)
mrqAttribute = lens _mrqAttribute (\ s a -> s{_mrqAttribute = a});

-- | [EC2-VPC] Changes the security groups of the instance. You must specify
-- at least one security group, even if it\'s just the default security
-- group for the VPC. You must specify the security group ID, not the
-- security group name.
mrqGroups :: Lens' ModifyInstanceAttribute [Text]
mrqGroups = lens _mrqGroups (\ s a -> s{_mrqGroups = a}) . _Default;

-- | Specifies whether source\/destination checking is enabled. A value of
-- @true@ means that checking is enabled, and @false@ means checking is
-- disabled. This value must be @false@ for a NAT instance to perform NAT.
mrqSourceDestCheck :: Lens' ModifyInstanceAttribute (Maybe AttributeBooleanValue)
mrqSourceDestCheck = lens _mrqSourceDestCheck (\ s a -> s{_mrqSourceDestCheck = a});

-- | If the value is @true@, you can\'t terminate the instance using the
-- Amazon EC2 console, CLI, or API; otherwise, you can.
mrqDisableAPITermination :: Lens' ModifyInstanceAttribute (Maybe AttributeBooleanValue)
mrqDisableAPITermination = lens _mrqDisableAPITermination (\ s a -> s{_mrqDisableAPITermination = a});

-- | Changes the instance\'s RAM disk to the specified value. We recommend
-- that you use PV-GRUB instead of kernels and RAM disks. For more
-- information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedKernels.html PV-GRUB>.
mrqRAMDisk :: Lens' ModifyInstanceAttribute (Maybe AttributeValue)
mrqRAMDisk = lens _mrqRAMDisk (\ s a -> s{_mrqRAMDisk = a});

-- | A new value for the attribute. Use only with the @kernel@, @ramdisk@,
-- @userData@, @disableApiTermination@, or
-- @intanceInitiateShutdownBehavior@ attribute.
mrqValue :: Lens' ModifyInstanceAttribute (Maybe Text)
mrqValue = lens _mrqValue (\ s a -> s{_mrqValue = a});

-- | Changes the instance\'s kernel to the specified value. We recommend that
-- you use PV-GRUB instead of kernels and RAM disks. For more information,
-- see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedKernels.html PV-GRUB>.
mrqKernel :: Lens' ModifyInstanceAttribute (Maybe AttributeValue)
mrqKernel = lens _mrqKernel (\ s a -> s{_mrqKernel = a});

-- | Changes the instance type to the specified value. For more information,
-- see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types>.
-- If the instance type is not valid, the error returned is
-- @InvalidInstanceAttributeValue@.
mrqInstanceType :: Lens' ModifyInstanceAttribute (Maybe AttributeValue)
mrqInstanceType = lens _mrqInstanceType (\ s a -> s{_mrqInstanceType = a});

-- | Specifies whether the instance is optimized for EBS I\/O. This
-- optimization provides dedicated throughput to Amazon EBS and an
-- optimized configuration stack to provide optimal EBS I\/O performance.
-- This optimization isn\'t available with all instance types. Additional
-- usage charges apply when using an EBS Optimized instance.
mrqEBSOptimized :: Lens' ModifyInstanceAttribute (Maybe AttributeBooleanValue)
mrqEBSOptimized = lens _mrqEBSOptimized (\ s a -> s{_mrqEBSOptimized = a});

-- | Changes the instance\'s user data to the specified value.
mrqUserData :: Lens' ModifyInstanceAttribute (Maybe BlobAttributeValue)
mrqUserData = lens _mrqUserData (\ s a -> s{_mrqUserData = a});

-- | Set to @simple@ to enable enhanced networking for the instance.
--
-- There is no way to disable enhanced networking at this time.
--
-- This option is supported only for HVM instances. Specifying this option
-- with a PV instance can make it unreachable.
mrqSRIOVNetSupport :: Lens' ModifyInstanceAttribute (Maybe AttributeValue)
mrqSRIOVNetSupport = lens _mrqSRIOVNetSupport (\ s a -> s{_mrqSRIOVNetSupport = a});

-- | Specifies whether an instance stops or terminates when you initiate
-- shutdown from the instance (using the operating system command for
-- system shutdown).
mrqInstanceInitiatedShutdownBehavior :: Lens' ModifyInstanceAttribute (Maybe AttributeValue)
mrqInstanceInitiatedShutdownBehavior = lens _mrqInstanceInitiatedShutdownBehavior (\ s a -> s{_mrqInstanceInitiatedShutdownBehavior = a});

-- | Modifies the @DeleteOnTermination@ attribute for volumes that are
-- currently attached. The volume must be owned by the caller. If no value
-- is specified for @DeleteOnTermination@, the default is @true@ and the
-- volume is deleted when the instance is terminated.
--
-- To add instance store volumes to an Amazon EBS-backed instance, you must
-- add them when you launch the instance. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/block-device-mapping-concepts.html#Using_OverridingAMIBDM Updating the Block Device Mapping when Launching an Instance>
-- in the /Amazon Elastic Compute Cloud User Guide/.
mrqBlockDeviceMappings :: Lens' ModifyInstanceAttribute [InstanceBlockDeviceMappingSpecification]
mrqBlockDeviceMappings = lens _mrqBlockDeviceMappings (\ s a -> s{_mrqBlockDeviceMappings = a}) . _Default;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
mrqDryRun :: Lens' ModifyInstanceAttribute (Maybe Bool)
mrqDryRun = lens _mrqDryRun (\ s a -> s{_mrqDryRun = a});

-- | The ID of the instance.
mrqInstanceId :: Lens' ModifyInstanceAttribute Text
mrqInstanceId = lens _mrqInstanceId (\ s a -> s{_mrqInstanceId = a});

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
               "Attribute" =: _mrqAttribute,
               toQuery (toQueryList "groupId" <$> _mrqGroups),
               "SourceDestCheck" =: _mrqSourceDestCheck,
               "DisableApiTermination" =: _mrqDisableAPITermination,
               "Ramdisk" =: _mrqRAMDisk, "Value" =: _mrqValue,
               "Kernel" =: _mrqKernel,
               "InstanceType" =: _mrqInstanceType,
               "EbsOptimized" =: _mrqEBSOptimized,
               "UserData" =: _mrqUserData,
               "SriovNetSupport" =: _mrqSRIOVNetSupport,
               "InstanceInitiatedShutdownBehavior" =:
                 _mrqInstanceInitiatedShutdownBehavior,
               toQuery
                 (toQueryList "item" <$> _mrqBlockDeviceMappings),
               "DryRun" =: _mrqDryRun,
               "InstanceId" =: _mrqInstanceId]

-- | /See:/ 'modifyInstanceAttributeResponse' smart constructor.
data ModifyInstanceAttributeResponse =
    ModifyInstanceAttributeResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ModifyInstanceAttributeResponse' smart constructor.
modifyInstanceAttributeResponse :: ModifyInstanceAttributeResponse
modifyInstanceAttributeResponse = ModifyInstanceAttributeResponse'
