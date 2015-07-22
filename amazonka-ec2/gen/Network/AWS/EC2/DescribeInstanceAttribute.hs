{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeInstanceAttribute
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified attribute of the specified instance. You can
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
    , diarqDryRun
    , diarqInstanceId
    , diarqAttribute

    -- * Response
    , DescribeInstanceAttributeResponse
    -- ** Response constructor
    , describeInstanceAttributeResponse
    -- ** Response lenses
    , diarsInstanceId
    , diarsGroups
    , diarsSourceDestCheck
    , diarsDisableAPITermination
    , diarsRAMDiskId
    , diarsKernelId
    , diarsInstanceType
    , diarsRootDeviceName
    , diarsEBSOptimized
    , diarsUserData
    , diarsSRIOVNetSupport
    , diarsInstanceInitiatedShutdownBehavior
    , diarsProductCodes
    , diarsBlockDeviceMappings
    , diarsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeInstanceAttribute' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'diarqDryRun'
--
-- * 'diarqInstanceId'
--
-- * 'diarqAttribute'
data DescribeInstanceAttribute = DescribeInstanceAttribute'
    { _diarqDryRun     :: !(Maybe Bool)
    , _diarqInstanceId :: !Text
    , _diarqAttribute  :: !InstanceAttributeName
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeInstanceAttribute' smart constructor.
describeInstanceAttribute :: Text -> InstanceAttributeName -> DescribeInstanceAttribute
describeInstanceAttribute pInstanceId pAttribute =
    DescribeInstanceAttribute'
    { _diarqDryRun = Nothing
    , _diarqInstanceId = pInstanceId
    , _diarqAttribute = pAttribute
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
diarqDryRun :: Lens' DescribeInstanceAttribute (Maybe Bool)
diarqDryRun = lens _diarqDryRun (\ s a -> s{_diarqDryRun = a});

-- | The ID of the instance.
diarqInstanceId :: Lens' DescribeInstanceAttribute Text
diarqInstanceId = lens _diarqInstanceId (\ s a -> s{_diarqInstanceId = a});

-- | The instance attribute.
diarqAttribute :: Lens' DescribeInstanceAttribute InstanceAttributeName
diarqAttribute = lens _diarqAttribute (\ s a -> s{_diarqAttribute = a});

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
               "DryRun" =: _diarqDryRun,
               "InstanceId" =: _diarqInstanceId,
               "Attribute" =: _diarqAttribute]

-- | Describes an instance attribute.
--
-- /See:/ 'describeInstanceAttributeResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'diarsInstanceId'
--
-- * 'diarsGroups'
--
-- * 'diarsSourceDestCheck'
--
-- * 'diarsDisableAPITermination'
--
-- * 'diarsRAMDiskId'
--
-- * 'diarsKernelId'
--
-- * 'diarsInstanceType'
--
-- * 'diarsRootDeviceName'
--
-- * 'diarsEBSOptimized'
--
-- * 'diarsUserData'
--
-- * 'diarsSRIOVNetSupport'
--
-- * 'diarsInstanceInitiatedShutdownBehavior'
--
-- * 'diarsProductCodes'
--
-- * 'diarsBlockDeviceMappings'
--
-- * 'diarsStatus'
data DescribeInstanceAttributeResponse = DescribeInstanceAttributeResponse'
    { _diarsInstanceId                        :: !(Maybe Text)
    , _diarsGroups                            :: !(Maybe [GroupIdentifier])
    , _diarsSourceDestCheck                   :: !(Maybe AttributeBooleanValue)
    , _diarsDisableAPITermination             :: !(Maybe AttributeBooleanValue)
    , _diarsRAMDiskId                         :: !(Maybe AttributeValue)
    , _diarsKernelId                          :: !(Maybe AttributeValue)
    , _diarsInstanceType                      :: !(Maybe AttributeValue)
    , _diarsRootDeviceName                    :: !(Maybe AttributeValue)
    , _diarsEBSOptimized                      :: !(Maybe AttributeBooleanValue)
    , _diarsUserData                          :: !(Maybe AttributeValue)
    , _diarsSRIOVNetSupport                   :: !(Maybe AttributeValue)
    , _diarsInstanceInitiatedShutdownBehavior :: !(Maybe AttributeValue)
    , _diarsProductCodes                      :: !(Maybe [ProductCode])
    , _diarsBlockDeviceMappings               :: !(Maybe [InstanceBlockDeviceMapping])
    , _diarsStatus                            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeInstanceAttributeResponse' smart constructor.
describeInstanceAttributeResponse :: Int -> DescribeInstanceAttributeResponse
describeInstanceAttributeResponse pStatus =
    DescribeInstanceAttributeResponse'
    { _diarsInstanceId = Nothing
    , _diarsGroups = Nothing
    , _diarsSourceDestCheck = Nothing
    , _diarsDisableAPITermination = Nothing
    , _diarsRAMDiskId = Nothing
    , _diarsKernelId = Nothing
    , _diarsInstanceType = Nothing
    , _diarsRootDeviceName = Nothing
    , _diarsEBSOptimized = Nothing
    , _diarsUserData = Nothing
    , _diarsSRIOVNetSupport = Nothing
    , _diarsInstanceInitiatedShutdownBehavior = Nothing
    , _diarsProductCodes = Nothing
    , _diarsBlockDeviceMappings = Nothing
    , _diarsStatus = pStatus
    }

-- | The ID of the instance.
diarsInstanceId :: Lens' DescribeInstanceAttributeResponse (Maybe Text)
diarsInstanceId = lens _diarsInstanceId (\ s a -> s{_diarsInstanceId = a});

-- | The security groups associated with the instance.
diarsGroups :: Lens' DescribeInstanceAttributeResponse [GroupIdentifier]
diarsGroups = lens _diarsGroups (\ s a -> s{_diarsGroups = a}) . _Default;

-- | Indicates whether source\/destination checking is enabled. A value of
-- @true@ means checking is enabled, and @false@ means checking is
-- disabled. This value must be @false@ for a NAT instance to perform NAT.
diarsSourceDestCheck :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeBooleanValue)
diarsSourceDestCheck = lens _diarsSourceDestCheck (\ s a -> s{_diarsSourceDestCheck = a});

-- | If the value is @true@, you can\'t terminate the instance through the
-- Amazon EC2 console, CLI, or API; otherwise, you can.
diarsDisableAPITermination :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeBooleanValue)
diarsDisableAPITermination = lens _diarsDisableAPITermination (\ s a -> s{_diarsDisableAPITermination = a});

-- | The RAM disk ID.
diarsRAMDiskId :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeValue)
diarsRAMDiskId = lens _diarsRAMDiskId (\ s a -> s{_diarsRAMDiskId = a});

-- | The kernel ID.
diarsKernelId :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeValue)
diarsKernelId = lens _diarsKernelId (\ s a -> s{_diarsKernelId = a});

-- | The instance type.
diarsInstanceType :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeValue)
diarsInstanceType = lens _diarsInstanceType (\ s a -> s{_diarsInstanceType = a});

-- | The name of the root device (for example, @\/dev\/sda1@ or
-- @\/dev\/xvda@).
diarsRootDeviceName :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeValue)
diarsRootDeviceName = lens _diarsRootDeviceName (\ s a -> s{_diarsRootDeviceName = a});

-- | Indicates whether the instance is optimized for EBS I\/O.
diarsEBSOptimized :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeBooleanValue)
diarsEBSOptimized = lens _diarsEBSOptimized (\ s a -> s{_diarsEBSOptimized = a});

-- | The Base64-encoded MIME user data.
diarsUserData :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeValue)
diarsUserData = lens _diarsUserData (\ s a -> s{_diarsUserData = a});

-- | FIXME: Undocumented member.
diarsSRIOVNetSupport :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeValue)
diarsSRIOVNetSupport = lens _diarsSRIOVNetSupport (\ s a -> s{_diarsSRIOVNetSupport = a});

-- | Indicates whether an instance stops or terminates when you initiate
-- shutdown from the instance (using the operating system command for
-- system shutdown).
diarsInstanceInitiatedShutdownBehavior :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeValue)
diarsInstanceInitiatedShutdownBehavior = lens _diarsInstanceInitiatedShutdownBehavior (\ s a -> s{_diarsInstanceInitiatedShutdownBehavior = a});

-- | A list of product codes.
diarsProductCodes :: Lens' DescribeInstanceAttributeResponse [ProductCode]
diarsProductCodes = lens _diarsProductCodes (\ s a -> s{_diarsProductCodes = a}) . _Default;

-- | The block device mapping of the instance.
diarsBlockDeviceMappings :: Lens' DescribeInstanceAttributeResponse [InstanceBlockDeviceMapping]
diarsBlockDeviceMappings = lens _diarsBlockDeviceMappings (\ s a -> s{_diarsBlockDeviceMappings = a}) . _Default;

-- | FIXME: Undocumented member.
diarsStatus :: Lens' DescribeInstanceAttributeResponse Int
diarsStatus = lens _diarsStatus (\ s a -> s{_diarsStatus = a});
