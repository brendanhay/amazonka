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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified attribute of the specified instance. You can specify only one attribute at a time. Valid attribute values are: @instanceType@ | @kernel@ | @ramdisk@ | @userData@ | @disableApiTermination@ | @instanceInitiatedShutdownBehavior@ | @rootDeviceName@ | @blockDeviceMapping@ | @productCodes@ | @sourceDestCheck@ | @groupSet@ | @ebsOptimized@ | @sriovNetSupport@
--
--
module Network.AWS.EC2.DescribeInstanceAttribute
    (
    -- * Creating a Request
      describeInstanceAttribute
    , DescribeInstanceAttribute
    -- * Request Lenses
    , diaDryRun
    , diaAttribute
    , diaInstanceId

    -- * Destructuring the Response
    , describeInstanceAttributeResponse
    , DescribeInstanceAttributeResponse
    -- * Response Lenses
    , desrsInstanceId
    , desrsGroups
    , desrsEnaSupport
    , desrsSourceDestCheck
    , desrsDisableAPITermination
    , desrsRAMDiskId
    , desrsKernelId
    , desrsRootDeviceName
    , desrsInstanceType
    , desrsSRIOVNetSupport
    , desrsEBSOptimized
    , desrsUserData
    , desrsInstanceInitiatedShutdownBehavior
    , desrsProductCodes
    , desrsBlockDeviceMappings
    , desrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeInstanceAttribute' smart constructor.
data DescribeInstanceAttribute = DescribeInstanceAttribute'
  { _diaDryRun     :: !(Maybe Bool)
  , _diaAttribute  :: !InstanceAttributeName
  , _diaInstanceId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeInstanceAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diaDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'diaAttribute' - The instance attribute. Note: The @enaSupport@ attribute is not supported at this time.
--
-- * 'diaInstanceId' - The ID of the instance.
describeInstanceAttribute
    :: InstanceAttributeName -- ^ 'diaAttribute'
    -> Text -- ^ 'diaInstanceId'
    -> DescribeInstanceAttribute
describeInstanceAttribute pAttribute_ pInstanceId_ =
  DescribeInstanceAttribute'
    { _diaDryRun = Nothing
    , _diaAttribute = pAttribute_
    , _diaInstanceId = pInstanceId_
    }


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
diaDryRun :: Lens' DescribeInstanceAttribute (Maybe Bool)
diaDryRun = lens _diaDryRun (\ s a -> s{_diaDryRun = a})

-- | The instance attribute. Note: The @enaSupport@ attribute is not supported at this time.
diaAttribute :: Lens' DescribeInstanceAttribute InstanceAttributeName
diaAttribute = lens _diaAttribute (\ s a -> s{_diaAttribute = a})

-- | The ID of the instance.
diaInstanceId :: Lens' DescribeInstanceAttribute Text
diaInstanceId = lens _diaInstanceId (\ s a -> s{_diaInstanceId = a})

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
                     <*> (x .@? "enaSupport")
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

instance Hashable DescribeInstanceAttribute where

instance NFData DescribeInstanceAttribute where

instance ToHeaders DescribeInstanceAttribute where
        toHeaders = const mempty

instance ToPath DescribeInstanceAttribute where
        toPath = const "/"

instance ToQuery DescribeInstanceAttribute where
        toQuery DescribeInstanceAttribute'{..}
          = mconcat
              ["Action" =:
                 ("DescribeInstanceAttribute" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _diaDryRun, "Attribute" =: _diaAttribute,
               "InstanceId" =: _diaInstanceId]

-- | Describes an instance attribute.
--
--
--
-- /See:/ 'describeInstanceAttributeResponse' smart constructor.
data DescribeInstanceAttributeResponse = DescribeInstanceAttributeResponse'
  { _desrsInstanceId :: !(Maybe Text)
  , _desrsGroups :: !(Maybe [GroupIdentifier])
  , _desrsEnaSupport :: !(Maybe AttributeBooleanValue)
  , _desrsSourceDestCheck :: !(Maybe AttributeBooleanValue)
  , _desrsDisableAPITermination :: !(Maybe AttributeBooleanValue)
  , _desrsRAMDiskId :: !(Maybe AttributeValue)
  , _desrsKernelId :: !(Maybe AttributeValue)
  , _desrsRootDeviceName :: !(Maybe AttributeValue)
  , _desrsInstanceType :: !(Maybe AttributeValue)
  , _desrsSRIOVNetSupport :: !(Maybe AttributeValue)
  , _desrsEBSOptimized :: !(Maybe AttributeBooleanValue)
  , _desrsUserData :: !(Maybe AttributeValue)
  , _desrsInstanceInitiatedShutdownBehavior :: !(Maybe AttributeValue)
  , _desrsProductCodes :: !(Maybe [ProductCode])
  , _desrsBlockDeviceMappings :: !(Maybe [InstanceBlockDeviceMapping])
  , _desrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeInstanceAttributeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desrsInstanceId' - The ID of the instance.
--
-- * 'desrsGroups' - The security groups associated with the instance.
--
-- * 'desrsEnaSupport' - Indicates whether enhanced networking with ENA is enabled.
--
-- * 'desrsSourceDestCheck' - Indicates whether source/destination checking is enabled. A value of @true@ means that checking is enabled, and @false@ means that checking is disabled. This value must be @false@ for a NAT instance to perform NAT.
--
-- * 'desrsDisableAPITermination' - If the value is @true@ , you can't terminate the instance through the Amazon EC2 console, CLI, or API; otherwise, you can.
--
-- * 'desrsRAMDiskId' - The RAM disk ID.
--
-- * 'desrsKernelId' - The kernel ID.
--
-- * 'desrsRootDeviceName' - The device name of the root device volume (for example, @/dev/sda1@ ).
--
-- * 'desrsInstanceType' - The instance type.
--
-- * 'desrsSRIOVNetSupport' - Indicates whether enhanced networking with the Intel 82599 Virtual Function interface is enabled.
--
-- * 'desrsEBSOptimized' - Indicates whether the instance is optimized for Amazon EBS I/O.
--
-- * 'desrsUserData' - The user data.
--
-- * 'desrsInstanceInitiatedShutdownBehavior' - Indicates whether an instance stops or terminates when you initiate shutdown from the instance (using the operating system command for system shutdown).
--
-- * 'desrsProductCodes' - A list of product codes.
--
-- * 'desrsBlockDeviceMappings' - The block device mapping of the instance.
--
-- * 'desrsResponseStatus' - -- | The response status code.
describeInstanceAttributeResponse
    :: Int -- ^ 'desrsResponseStatus'
    -> DescribeInstanceAttributeResponse
describeInstanceAttributeResponse pResponseStatus_ =
  DescribeInstanceAttributeResponse'
    { _desrsInstanceId = Nothing
    , _desrsGroups = Nothing
    , _desrsEnaSupport = Nothing
    , _desrsSourceDestCheck = Nothing
    , _desrsDisableAPITermination = Nothing
    , _desrsRAMDiskId = Nothing
    , _desrsKernelId = Nothing
    , _desrsRootDeviceName = Nothing
    , _desrsInstanceType = Nothing
    , _desrsSRIOVNetSupport = Nothing
    , _desrsEBSOptimized = Nothing
    , _desrsUserData = Nothing
    , _desrsInstanceInitiatedShutdownBehavior = Nothing
    , _desrsProductCodes = Nothing
    , _desrsBlockDeviceMappings = Nothing
    , _desrsResponseStatus = pResponseStatus_
    }


-- | The ID of the instance.
desrsInstanceId :: Lens' DescribeInstanceAttributeResponse (Maybe Text)
desrsInstanceId = lens _desrsInstanceId (\ s a -> s{_desrsInstanceId = a})

-- | The security groups associated with the instance.
desrsGroups :: Lens' DescribeInstanceAttributeResponse [GroupIdentifier]
desrsGroups = lens _desrsGroups (\ s a -> s{_desrsGroups = a}) . _Default . _Coerce

-- | Indicates whether enhanced networking with ENA is enabled.
desrsEnaSupport :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeBooleanValue)
desrsEnaSupport = lens _desrsEnaSupport (\ s a -> s{_desrsEnaSupport = a})

-- | Indicates whether source/destination checking is enabled. A value of @true@ means that checking is enabled, and @false@ means that checking is disabled. This value must be @false@ for a NAT instance to perform NAT.
desrsSourceDestCheck :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeBooleanValue)
desrsSourceDestCheck = lens _desrsSourceDestCheck (\ s a -> s{_desrsSourceDestCheck = a})

-- | If the value is @true@ , you can't terminate the instance through the Amazon EC2 console, CLI, or API; otherwise, you can.
desrsDisableAPITermination :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeBooleanValue)
desrsDisableAPITermination = lens _desrsDisableAPITermination (\ s a -> s{_desrsDisableAPITermination = a})

-- | The RAM disk ID.
desrsRAMDiskId :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeValue)
desrsRAMDiskId = lens _desrsRAMDiskId (\ s a -> s{_desrsRAMDiskId = a})

-- | The kernel ID.
desrsKernelId :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeValue)
desrsKernelId = lens _desrsKernelId (\ s a -> s{_desrsKernelId = a})

-- | The device name of the root device volume (for example, @/dev/sda1@ ).
desrsRootDeviceName :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeValue)
desrsRootDeviceName = lens _desrsRootDeviceName (\ s a -> s{_desrsRootDeviceName = a})

-- | The instance type.
desrsInstanceType :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeValue)
desrsInstanceType = lens _desrsInstanceType (\ s a -> s{_desrsInstanceType = a})

-- | Indicates whether enhanced networking with the Intel 82599 Virtual Function interface is enabled.
desrsSRIOVNetSupport :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeValue)
desrsSRIOVNetSupport = lens _desrsSRIOVNetSupport (\ s a -> s{_desrsSRIOVNetSupport = a})

-- | Indicates whether the instance is optimized for Amazon EBS I/O.
desrsEBSOptimized :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeBooleanValue)
desrsEBSOptimized = lens _desrsEBSOptimized (\ s a -> s{_desrsEBSOptimized = a})

-- | The user data.
desrsUserData :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeValue)
desrsUserData = lens _desrsUserData (\ s a -> s{_desrsUserData = a})

-- | Indicates whether an instance stops or terminates when you initiate shutdown from the instance (using the operating system command for system shutdown).
desrsInstanceInitiatedShutdownBehavior :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeValue)
desrsInstanceInitiatedShutdownBehavior = lens _desrsInstanceInitiatedShutdownBehavior (\ s a -> s{_desrsInstanceInitiatedShutdownBehavior = a})

-- | A list of product codes.
desrsProductCodes :: Lens' DescribeInstanceAttributeResponse [ProductCode]
desrsProductCodes = lens _desrsProductCodes (\ s a -> s{_desrsProductCodes = a}) . _Default . _Coerce

-- | The block device mapping of the instance.
desrsBlockDeviceMappings :: Lens' DescribeInstanceAttributeResponse [InstanceBlockDeviceMapping]
desrsBlockDeviceMappings = lens _desrsBlockDeviceMappings (\ s a -> s{_desrsBlockDeviceMappings = a}) . _Default . _Coerce

-- | -- | The response status code.
desrsResponseStatus :: Lens' DescribeInstanceAttributeResponse Int
desrsResponseStatus = lens _desrsResponseStatus (\ s a -> s{_desrsResponseStatus = a})

instance NFData DescribeInstanceAttributeResponse
         where
