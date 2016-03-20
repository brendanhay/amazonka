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
-- Module      : Network.AWS.EC2.DescribeInstanceAttribute
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified attribute of the specified instance. You can
-- specify only one attribute at a time. Valid attribute values are:
-- 'instanceType' | 'kernel' | 'ramdisk' | 'userData' |
-- 'disableApiTermination' | 'instanceInitiatedShutdownBehavior' |
-- 'rootDeviceName' | 'blockDeviceMapping' | 'productCodes' |
-- 'sourceDestCheck' | 'groupSet' | 'ebsOptimized' | 'sriovNetSupport'
module Network.AWS.EC2.DescribeInstanceAttribute
    (
    -- * Creating a Request
      describeInstanceAttribute
    , DescribeInstanceAttribute
    -- * Request Lenses
    , diaDryRun
    , diaInstanceId
    , diaAttribute

    -- * Destructuring the Response
    , describeInstanceAttributeResponse
    , DescribeInstanceAttributeResponse
    -- * Response Lenses
    , drsInstanceId
    , drsGroups
    , drsSourceDestCheck
    , drsDisableAPITermination
    , drsRAMDiskId
    , drsKernelId
    , drsRootDeviceName
    , drsInstanceType
    , drsSRIOVNetSupport
    , drsEBSOptimized
    , drsUserData
    , drsInstanceInitiatedShutdownBehavior
    , drsProductCodes
    , drsBlockDeviceMappings
    , drsResponseStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeInstanceAttribute' smart constructor.
data DescribeInstanceAttribute = DescribeInstanceAttribute'
    { _diaDryRun     :: !(Maybe Bool)
    , _diaInstanceId :: !Text
    , _diaAttribute  :: !InstanceAttributeName
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeInstanceAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diaDryRun'
--
-- * 'diaInstanceId'
--
-- * 'diaAttribute'
describeInstanceAttribute
    :: Text -- ^ 'diaInstanceId'
    -> InstanceAttributeName -- ^ 'diaAttribute'
    -> DescribeInstanceAttribute
describeInstanceAttribute pInstanceId_ pAttribute_ =
    DescribeInstanceAttribute'
    { _diaDryRun = Nothing
    , _diaInstanceId = pInstanceId_
    , _diaAttribute = pAttribute_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is 'DryRunOperation'.
-- Otherwise, it is 'UnauthorizedOperation'.
diaDryRun :: Lens' DescribeInstanceAttribute (Maybe Bool)
diaDryRun = lens _diaDryRun (\ s a -> s{_diaDryRun = a});

-- | The ID of the instance.
diaInstanceId :: Lens' DescribeInstanceAttribute Text
diaInstanceId = lens _diaInstanceId (\ s a -> s{_diaInstanceId = a});

-- | The instance attribute.
diaAttribute :: Lens' DescribeInstanceAttribute InstanceAttributeName
diaAttribute = lens _diaAttribute (\ s a -> s{_diaAttribute = a});

instance AWSRequest DescribeInstanceAttribute where
        type Rs DescribeInstanceAttribute =
             DescribeInstanceAttributeResponse
        request = postQuery ec2
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
                     <*> (x .@? "rootDeviceName")
                     <*> (x .@? "instanceType")
                     <*> (x .@? "sriovNetSupport")
                     <*> (x .@? "ebsOptimized")
                     <*> (x .@? "userData")
                     <*> (x .@? "instanceInitiatedShutdownBehavior")
                     <*>
                     (x .@? "productCodes" .!@ mempty >>=
                        may (parseXMLList "item"))
                     <*>
                     (x .@? "blockDeviceMapping" .!@ mempty >>=
                        may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeInstanceAttribute

instance ToHeaders DescribeInstanceAttribute where
        toHeaders = const mempty

instance ToPath DescribeInstanceAttribute where
        toPath = const "/"

instance ToQuery DescribeInstanceAttribute where
        toQuery DescribeInstanceAttribute'{..}
          = mconcat
              ["Action" =:
                 ("DescribeInstanceAttribute" :: ByteString),
               "Version" =: ("2015-10-01" :: ByteString),
               "DryRun" =: _diaDryRun,
               "InstanceId" =: _diaInstanceId,
               "Attribute" =: _diaAttribute]

-- | Describes an instance attribute.
--
-- /See:/ 'describeInstanceAttributeResponse' smart constructor.
data DescribeInstanceAttributeResponse = DescribeInstanceAttributeResponse'
    { _drsInstanceId                        :: !(Maybe Text)
    , _drsGroups                            :: !(Maybe [GroupIdentifier])
    , _drsSourceDestCheck                   :: !(Maybe AttributeBooleanValue)
    , _drsDisableAPITermination             :: !(Maybe AttributeBooleanValue)
    , _drsRAMDiskId                         :: !(Maybe AttributeValue)
    , _drsKernelId                          :: !(Maybe AttributeValue)
    , _drsRootDeviceName                    :: !(Maybe AttributeValue)
    , _drsInstanceType                      :: !(Maybe AttributeValue)
    , _drsSRIOVNetSupport                   :: !(Maybe AttributeValue)
    , _drsEBSOptimized                      :: !(Maybe AttributeBooleanValue)
    , _drsUserData                          :: !(Maybe AttributeValue)
    , _drsInstanceInitiatedShutdownBehavior :: !(Maybe AttributeValue)
    , _drsProductCodes                      :: !(Maybe [ProductCode])
    , _drsBlockDeviceMappings               :: !(Maybe [InstanceBlockDeviceMapping])
    , _drsResponseStatus                    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeInstanceAttributeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsInstanceId'
--
-- * 'drsGroups'
--
-- * 'drsSourceDestCheck'
--
-- * 'drsDisableAPITermination'
--
-- * 'drsRAMDiskId'
--
-- * 'drsKernelId'
--
-- * 'drsRootDeviceName'
--
-- * 'drsInstanceType'
--
-- * 'drsSRIOVNetSupport'
--
-- * 'drsEBSOptimized'
--
-- * 'drsUserData'
--
-- * 'drsInstanceInitiatedShutdownBehavior'
--
-- * 'drsProductCodes'
--
-- * 'drsBlockDeviceMappings'
--
-- * 'drsResponseStatus'
describeInstanceAttributeResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DescribeInstanceAttributeResponse
describeInstanceAttributeResponse pResponseStatus_ =
    DescribeInstanceAttributeResponse'
    { _drsInstanceId = Nothing
    , _drsGroups = Nothing
    , _drsSourceDestCheck = Nothing
    , _drsDisableAPITermination = Nothing
    , _drsRAMDiskId = Nothing
    , _drsKernelId = Nothing
    , _drsRootDeviceName = Nothing
    , _drsInstanceType = Nothing
    , _drsSRIOVNetSupport = Nothing
    , _drsEBSOptimized = Nothing
    , _drsUserData = Nothing
    , _drsInstanceInitiatedShutdownBehavior = Nothing
    , _drsProductCodes = Nothing
    , _drsBlockDeviceMappings = Nothing
    , _drsResponseStatus = pResponseStatus_
    }

-- | The ID of the instance.
drsInstanceId :: Lens' DescribeInstanceAttributeResponse (Maybe Text)
drsInstanceId = lens _drsInstanceId (\ s a -> s{_drsInstanceId = a});

-- | The security groups associated with the instance.
drsGroups :: Lens' DescribeInstanceAttributeResponse [GroupIdentifier]
drsGroups = lens _drsGroups (\ s a -> s{_drsGroups = a}) . _Default . _Coerce;

-- | Indicates whether source\/destination checking is enabled. A value of
-- 'true' means checking is enabled, and 'false' means checking is
-- disabled. This value must be 'false' for a NAT instance to perform NAT.
drsSourceDestCheck :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeBooleanValue)
drsSourceDestCheck = lens _drsSourceDestCheck (\ s a -> s{_drsSourceDestCheck = a});

-- | If the value is 'true', you can\'t terminate the instance through the
-- Amazon EC2 console, CLI, or API; otherwise, you can.
drsDisableAPITermination :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeBooleanValue)
drsDisableAPITermination = lens _drsDisableAPITermination (\ s a -> s{_drsDisableAPITermination = a});

-- | The RAM disk ID.
drsRAMDiskId :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeValue)
drsRAMDiskId = lens _drsRAMDiskId (\ s a -> s{_drsRAMDiskId = a});

-- | The kernel ID.
drsKernelId :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeValue)
drsKernelId = lens _drsKernelId (\ s a -> s{_drsKernelId = a});

-- | The name of the root device (for example, '\/dev\/sda1' or
-- '\/dev\/xvda').
drsRootDeviceName :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeValue)
drsRootDeviceName = lens _drsRootDeviceName (\ s a -> s{_drsRootDeviceName = a});

-- | The instance type.
drsInstanceType :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeValue)
drsInstanceType = lens _drsInstanceType (\ s a -> s{_drsInstanceType = a});

-- | Undocumented member.
drsSRIOVNetSupport :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeValue)
drsSRIOVNetSupport = lens _drsSRIOVNetSupport (\ s a -> s{_drsSRIOVNetSupport = a});

-- | Indicates whether the instance is optimized for EBS I\/O.
drsEBSOptimized :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeBooleanValue)
drsEBSOptimized = lens _drsEBSOptimized (\ s a -> s{_drsEBSOptimized = a});

-- | The Base64-encoded MIME user data.
drsUserData :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeValue)
drsUserData = lens _drsUserData (\ s a -> s{_drsUserData = a});

-- | Indicates whether an instance stops or terminates when you initiate
-- shutdown from the instance (using the operating system command for
-- system shutdown).
drsInstanceInitiatedShutdownBehavior :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeValue)
drsInstanceInitiatedShutdownBehavior = lens _drsInstanceInitiatedShutdownBehavior (\ s a -> s{_drsInstanceInitiatedShutdownBehavior = a});

-- | A list of product codes.
drsProductCodes :: Lens' DescribeInstanceAttributeResponse [ProductCode]
drsProductCodes = lens _drsProductCodes (\ s a -> s{_drsProductCodes = a}) . _Default . _Coerce;

-- | The block device mapping of the instance.
drsBlockDeviceMappings :: Lens' DescribeInstanceAttributeResponse [InstanceBlockDeviceMapping]
drsBlockDeviceMappings = lens _drsBlockDeviceMappings (\ s a -> s{_drsBlockDeviceMappings = a}) . _Default . _Coerce;

-- | The response status code.
drsResponseStatus :: Lens' DescribeInstanceAttributeResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a});
