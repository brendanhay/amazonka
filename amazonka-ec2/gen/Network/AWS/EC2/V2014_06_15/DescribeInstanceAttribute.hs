{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DescribeInstanceAttribute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes the specified attribute of the specified instance. You can
-- specify only one attribute at a time. Example 1 This example lists the
-- instance type of the specified instance.
-- https://ec2.amazonaws.com/?Action=DescribeInstanceAttribute
-- &amp;InstanceId=i-10a64379 &amp;Attribute=instanceType &amp;AUTHPARAMS
-- &lt;DescribeInstanceAttributeResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;instanceId&gt;i-10a64379&lt;/instanceId&gt; &lt;instanceType&gt;
-- &lt;value&gt;t1.micro&lt;/value&gt; &lt;/instanceType&gt;
-- &lt;/DescribeInstanceAttributeResponse&gt; Example 2 This example lists the
-- current value of the InstanceInitiatedShutdownBehavior attribute for the
-- specified instance.
-- https://ec2.amazonaws.com/?Action=DescribeInstanceAttribute
-- &amp;InstanceId=i-10a64379 &amp;Attribute=instanceInitiatedShutdownBehavior
-- &amp;AUTHPARAMS &lt;DescribeInstanceAttributeResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;instanceId&gt;i-10a64379&lt;/instanceId&gt;
-- &lt;instanceInitiatedShutdownBehavior&gt; &lt;value&gt;stop&lt;/value&gt;
-- &lt;/instanceInitiatedShutdownBehavior&gt;
-- &lt;/DescribeInstanceAttributeResponse&gt; Example 3 This example lists the
-- current value of the DisableApiTermination attribute for the specified
-- instance. https://ec2.amazonaws.com/?Action=DescribeInstanceAttribute
-- &amp;InstanceId=i-10a64379 &amp;Attribute=disableApiTermination
-- &amp;AUTHPARAMS &lt;DescribeInstanceAttributeResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;instanceId&gt;i-10a64379&lt;/instanceId&gt;
-- &lt;disableApiTermination&gt; &lt;value&gt;false&lt;/value&gt;
-- &lt;/disableApiTermination&gt; &lt;/DescribeInstanceAttributeResponse&gt;.
module Network.AWS.EC2.V2014_06_15.DescribeInstanceAttribute
    (
    -- * Request
      DescribeInstanceAttribute
    -- ** Request constructor
    , mkDescribeInstanceAttribute
    -- ** Request lenses
    , dia1InstanceId
    , dia1Attribute

    -- * Response
    , DescribeInstanceAttributeResponse
    -- ** Response lenses
    , diarsrsInstanceId
    , diarsrsInstanceType
    , diarsrsKernelId
    , diarsrsRamdiskId
    , diarsrsUserData
    , diarsrsDisableApiTermination
    , diarsrsInstanceInitiatedShutdownBehavior
    , diarsrsRootDeviceName
    , diarsrsBlockDeviceMappings
    , diarsrsProductCodes
    , diarsrsEbsOptimized
    , diarsrsSriovNetSupport
    , diarsrsSourceDestCheck
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | 
data DescribeInstanceAttribute = DescribeInstanceAttribute
    { _dia1InstanceId :: Text
    , _dia1Attribute :: InstanceAttributeName
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeInstanceAttribute' request.
mkDescribeInstanceAttribute :: Text -- ^ 'dia1InstanceId'
                            -> InstanceAttributeName -- ^ 'dia1Attribute'
                            -> DescribeInstanceAttribute
mkDescribeInstanceAttribute p1 p2 = DescribeInstanceAttribute
    { _dia1InstanceId = p1
    , _dia1Attribute = p2
    }

-- | The ID of the instance.
dia1InstanceId :: Lens' DescribeInstanceAttribute Text
dia1InstanceId = lens _dia1InstanceId (\s a -> s { _dia1InstanceId = a })

-- | The instance attribute.
dia1Attribute :: Lens' DescribeInstanceAttribute InstanceAttributeName
dia1Attribute = lens _dia1Attribute (\s a -> s { _dia1Attribute = a })

instance ToQuery DescribeInstanceAttribute where
    toQuery = genericQuery def

-- | 
data DescribeInstanceAttributeResponse = DescribeInstanceAttributeResponse
    { _diarsrsInstanceId :: Maybe Text
    , _diarsrsInstanceType :: Maybe AttributeValue
    , _diarsrsKernelId :: Maybe AttributeValue
    , _diarsrsRamdiskId :: Maybe AttributeValue
    , _diarsrsUserData :: Maybe AttributeValue
    , _diarsrsDisableApiTermination :: Maybe AttributeBooleanValue
    , _diarsrsInstanceInitiatedShutdownBehavior :: Maybe AttributeValue
    , _diarsrsRootDeviceName :: Maybe AttributeValue
    , _diarsrsBlockDeviceMappings :: [InstanceBlockDeviceMapping]
    , _diarsrsProductCodes :: [ProductCode]
    , _diarsrsEbsOptimized :: Maybe AttributeBooleanValue
    , _diarsrsSriovNetSupport :: Maybe AttributeValue
    , _diarsrsSourceDestCheck :: Maybe AttributeBooleanValue
    } deriving (Show, Generic)

-- | The ID of the instance.
diarsrsInstanceId :: Lens' DescribeInstanceAttributeResponse (Maybe Text)
diarsrsInstanceId =
    lens _diarsrsInstanceId (\s a -> s { _diarsrsInstanceId = a })

-- | The instance type.
diarsrsInstanceType :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeValue)
diarsrsInstanceType =
    lens _diarsrsInstanceType (\s a -> s { _diarsrsInstanceType = a })

-- | The kernel ID.
diarsrsKernelId :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeValue)
diarsrsKernelId = lens _diarsrsKernelId (\s a -> s { _diarsrsKernelId = a })

-- | The RAM disk ID.
diarsrsRamdiskId :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeValue)
diarsrsRamdiskId =
    lens _diarsrsRamdiskId (\s a -> s { _diarsrsRamdiskId = a })

-- | The Base64-encoded MIME user data.
diarsrsUserData :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeValue)
diarsrsUserData = lens _diarsrsUserData (\s a -> s { _diarsrsUserData = a })

-- | If the value is true, you can't terminate the instance through the Amazon
-- EC2 console, CLI, or API; otherwise, you can.
diarsrsDisableApiTermination :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeBooleanValue)
diarsrsDisableApiTermination =
    lens _diarsrsDisableApiTermination
         (\s a -> s { _diarsrsDisableApiTermination = a })

-- | Indicates whether an instance stops or terminates when you initiate
-- shutdown from the instance (using the operating system command for system
-- shutdown).
diarsrsInstanceInitiatedShutdownBehavior :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeValue)
diarsrsInstanceInitiatedShutdownBehavior =
    lens _diarsrsInstanceInitiatedShutdownBehavior
         (\s a -> s { _diarsrsInstanceInitiatedShutdownBehavior = a })

-- | The name of the root device (for example, /dev/sda1).
diarsrsRootDeviceName :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeValue)
diarsrsRootDeviceName =
    lens _diarsrsRootDeviceName (\s a -> s { _diarsrsRootDeviceName = a })

-- | The block device mapping of the instance.
diarsrsBlockDeviceMappings :: Lens' DescribeInstanceAttributeResponse [InstanceBlockDeviceMapping]
diarsrsBlockDeviceMappings =
    lens _diarsrsBlockDeviceMappings
         (\s a -> s { _diarsrsBlockDeviceMappings = a })

-- | A list of product codes.
diarsrsProductCodes :: Lens' DescribeInstanceAttributeResponse [ProductCode]
diarsrsProductCodes =
    lens _diarsrsProductCodes (\s a -> s { _diarsrsProductCodes = a })

-- | Indicates whether the instance is optimized for EBS I/O.
diarsrsEbsOptimized :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeBooleanValue)
diarsrsEbsOptimized =
    lens _diarsrsEbsOptimized (\s a -> s { _diarsrsEbsOptimized = a })

-- | 
diarsrsSriovNetSupport :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeValue)
diarsrsSriovNetSupport =
    lens _diarsrsSriovNetSupport (\s a -> s { _diarsrsSriovNetSupport = a })

-- | Indicates whether source/destination checking is enabled. A value of true
-- means checking is enabled, and false means checking is disabled. This value
-- must be false for a NAT instance to perform NAT.
diarsrsSourceDestCheck :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeBooleanValue)
diarsrsSourceDestCheck =
    lens _diarsrsSourceDestCheck (\s a -> s { _diarsrsSourceDestCheck = a })

instance FromXML DescribeInstanceAttributeResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeInstanceAttribute where
    type Sv DescribeInstanceAttribute = EC2
    type Rs DescribeInstanceAttribute = DescribeInstanceAttributeResponse

    request = post "DescribeInstanceAttribute"
    response _ = xmlResponse
