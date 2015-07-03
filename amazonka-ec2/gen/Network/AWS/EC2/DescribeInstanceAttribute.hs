{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.DescribeInstanceAttribute
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

-- | Describes the specified attribute of the specified instance. You can
-- specify only one attribute at a time. Valid attribute values are:
-- @instanceType@ | @kernel@ | @ramdisk@ | @userData@ |
-- @disableApiTermination@ | @instanceInitiatedShutdownBehavior@ |
-- @rootDeviceName@ | @blockDeviceMapping@ | @productCodes@ |
-- @sourceDestCheck@ | @groupSet@ | @ebsOptimized@ | @sriovNetSupport@
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeInstanceAttribute.html>
module Network.AWS.EC2.DescribeInstanceAttribute
    (
    -- * Request
      DescribeInstanceAttribute
    -- ** Request constructor
    , describeInstanceAttribute
    -- ** Request lenses
    , diaDryRun
    , diaInstanceId
    , diaAttribute

    -- * Response
    , DescribeInstanceAttributeResponse
    -- ** Response constructor
    , describeInstanceAttributeResponse
    -- ** Response lenses
    , desInstanceId
    , desGroups
    , desSourceDestCheck
    , desDisableAPITermination
    , desRAMDiskId
    , desKernelId
    , desInstanceType
    , desRootDeviceName
    , desEBSOptimized
    , desUserData
    , desSRIOVNetSupport
    , desInstanceInitiatedShutdownBehavior
    , desProductCodes
    , desBlockDeviceMappings
    , desStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeInstanceAttribute' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'diaDryRun'
--
-- * 'diaInstanceId'
--
-- * 'diaAttribute'
data DescribeInstanceAttribute = DescribeInstanceAttribute'
    { _diaDryRun     :: !(Maybe Bool)
    , _diaInstanceId :: !Text
    , _diaAttribute  :: !InstanceAttributeName
    } deriving (Eq,Read,Show)

-- | 'DescribeInstanceAttribute' smart constructor.
describeInstanceAttribute :: Text -> InstanceAttributeName -> DescribeInstanceAttribute
describeInstanceAttribute pInstanceId pAttribute =
    DescribeInstanceAttribute'
    { _diaDryRun = Nothing
    , _diaInstanceId = pInstanceId
    , _diaAttribute = pAttribute
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
diaDryRun :: Lens' DescribeInstanceAttribute (Maybe Bool)
diaDryRun = lens _diaDryRun (\ s a -> s{_diaDryRun = a});

-- | The ID of the instance.
diaInstanceId :: Lens' DescribeInstanceAttribute Text
diaInstanceId = lens _diaInstanceId (\ s a -> s{_diaInstanceId = a});

-- | The instance attribute.
diaAttribute :: Lens' DescribeInstanceAttribute InstanceAttributeName
diaAttribute = lens _diaAttribute (\ s a -> s{_diaAttribute = a});

instance AWSRequest DescribeInstanceAttribute where
        type Sv DescribeInstanceAttribute = EC2
        type Rs DescribeInstanceAttribute =
             DescribeInstanceAttributeResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DescribeInstanceAttributeResponse' <$>
                   (x .@? "instanceId") <*>
                     (x .@? "groupSet" .!@ mempty >>=
                        may (parseXMLList "item"))
                     <*> (x .@? "sourceDestCheck")
                     <*> (x .@? "disableApiTermination")
                     <*> (x .@? "ramdisk")
                     <*> (x .@? "kernel")
                     <*> (x .@? "instanceType")
                     <*> (x .@? "rootDeviceName")
                     <*> (x .@? "ebsOptimized")
                     <*> (x .@? "userData")
                     <*> (x .@? "sriovNetSupport")
                     <*> (x .@? "instanceInitiatedShutdownBehavior")
                     <*>
                     (x .@? "productCodes" .!@ mempty >>=
                        may (parseXMLList "item"))
                     <*>
                     (x .@? "blockDeviceMapping" .!@ mempty >>=
                        may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeInstanceAttribute where
        toHeaders = const mempty

instance ToPath DescribeInstanceAttribute where
        toPath = const "/"

instance ToQuery DescribeInstanceAttribute where
        toQuery DescribeInstanceAttribute'{..}
          = mconcat
              ["Action" =:
                 ("DescribeInstanceAttribute" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _diaDryRun,
               "InstanceId" =: _diaInstanceId,
               "Attribute" =: _diaAttribute]

-- | Describes an instance attribute.
--
-- /See:/ 'describeInstanceAttributeResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'desInstanceId'
--
-- * 'desGroups'
--
-- * 'desSourceDestCheck'
--
-- * 'desDisableAPITermination'
--
-- * 'desRAMDiskId'
--
-- * 'desKernelId'
--
-- * 'desInstanceType'
--
-- * 'desRootDeviceName'
--
-- * 'desEBSOptimized'
--
-- * 'desUserData'
--
-- * 'desSRIOVNetSupport'
--
-- * 'desInstanceInitiatedShutdownBehavior'
--
-- * 'desProductCodes'
--
-- * 'desBlockDeviceMappings'
--
-- * 'desStatus'
data DescribeInstanceAttributeResponse = DescribeInstanceAttributeResponse'
    { _desInstanceId                        :: !(Maybe Text)
    , _desGroups                            :: !(Maybe [GroupIdentifier])
    , _desSourceDestCheck                   :: !(Maybe AttributeBooleanValue)
    , _desDisableAPITermination             :: !(Maybe AttributeBooleanValue)
    , _desRAMDiskId                         :: !(Maybe AttributeValue)
    , _desKernelId                          :: !(Maybe AttributeValue)
    , _desInstanceType                      :: !(Maybe AttributeValue)
    , _desRootDeviceName                    :: !(Maybe AttributeValue)
    , _desEBSOptimized                      :: !(Maybe AttributeBooleanValue)
    , _desUserData                          :: !(Maybe AttributeValue)
    , _desSRIOVNetSupport                   :: !(Maybe AttributeValue)
    , _desInstanceInitiatedShutdownBehavior :: !(Maybe AttributeValue)
    , _desProductCodes                      :: !(Maybe [ProductCode])
    , _desBlockDeviceMappings               :: !(Maybe [InstanceBlockDeviceMapping])
    , _desStatus                            :: !Int
    } deriving (Eq,Read,Show)

-- | 'DescribeInstanceAttributeResponse' smart constructor.
describeInstanceAttributeResponse :: Int -> DescribeInstanceAttributeResponse
describeInstanceAttributeResponse pStatus =
    DescribeInstanceAttributeResponse'
    { _desInstanceId = Nothing
    , _desGroups = Nothing
    , _desSourceDestCheck = Nothing
    , _desDisableAPITermination = Nothing
    , _desRAMDiskId = Nothing
    , _desKernelId = Nothing
    , _desInstanceType = Nothing
    , _desRootDeviceName = Nothing
    , _desEBSOptimized = Nothing
    , _desUserData = Nothing
    , _desSRIOVNetSupport = Nothing
    , _desInstanceInitiatedShutdownBehavior = Nothing
    , _desProductCodes = Nothing
    , _desBlockDeviceMappings = Nothing
    , _desStatus = pStatus
    }

-- | The ID of the instance.
desInstanceId :: Lens' DescribeInstanceAttributeResponse (Maybe Text)
desInstanceId = lens _desInstanceId (\ s a -> s{_desInstanceId = a});

-- | The security groups associated with the instance.
desGroups :: Lens' DescribeInstanceAttributeResponse [GroupIdentifier]
desGroups = lens _desGroups (\ s a -> s{_desGroups = a}) . _Default;

-- | Indicates whether source\/destination checking is enabled. A value of
-- @true@ means checking is enabled, and @false@ means checking is
-- disabled. This value must be @false@ for a NAT instance to perform NAT.
desSourceDestCheck :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeBooleanValue)
desSourceDestCheck = lens _desSourceDestCheck (\ s a -> s{_desSourceDestCheck = a});

-- | If the value is @true@, you can\'t terminate the instance through the
-- Amazon EC2 console, CLI, or API; otherwise, you can.
desDisableAPITermination :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeBooleanValue)
desDisableAPITermination = lens _desDisableAPITermination (\ s a -> s{_desDisableAPITermination = a});

-- | The RAM disk ID.
desRAMDiskId :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeValue)
desRAMDiskId = lens _desRAMDiskId (\ s a -> s{_desRAMDiskId = a});

-- | The kernel ID.
desKernelId :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeValue)
desKernelId = lens _desKernelId (\ s a -> s{_desKernelId = a});

-- | The instance type.
desInstanceType :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeValue)
desInstanceType = lens _desInstanceType (\ s a -> s{_desInstanceType = a});

-- | The name of the root device (for example, @\/dev\/sda1@ or
-- @\/dev\/xvda@).
desRootDeviceName :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeValue)
desRootDeviceName = lens _desRootDeviceName (\ s a -> s{_desRootDeviceName = a});

-- | Indicates whether the instance is optimized for EBS I\/O.
desEBSOptimized :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeBooleanValue)
desEBSOptimized = lens _desEBSOptimized (\ s a -> s{_desEBSOptimized = a});

-- | The Base64-encoded MIME user data.
desUserData :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeValue)
desUserData = lens _desUserData (\ s a -> s{_desUserData = a});

-- | FIXME: Undocumented member.
desSRIOVNetSupport :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeValue)
desSRIOVNetSupport = lens _desSRIOVNetSupport (\ s a -> s{_desSRIOVNetSupport = a});

-- | Indicates whether an instance stops or terminates when you initiate
-- shutdown from the instance (using the operating system command for
-- system shutdown).
desInstanceInitiatedShutdownBehavior :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeValue)
desInstanceInitiatedShutdownBehavior = lens _desInstanceInitiatedShutdownBehavior (\ s a -> s{_desInstanceInitiatedShutdownBehavior = a});

-- | A list of product codes.
desProductCodes :: Lens' DescribeInstanceAttributeResponse [ProductCode]
desProductCodes = lens _desProductCodes (\ s a -> s{_desProductCodes = a}) . _Default;

-- | The block device mapping of the instance.
desBlockDeviceMappings :: Lens' DescribeInstanceAttributeResponse [InstanceBlockDeviceMapping]
desBlockDeviceMappings = lens _desBlockDeviceMappings (\ s a -> s{_desBlockDeviceMappings = a}) . _Default;

-- | FIXME: Undocumented member.
desStatus :: Lens' DescribeInstanceAttributeResponse Int
desStatus = lens _desStatus (\ s a -> s{_desStatus = a});
