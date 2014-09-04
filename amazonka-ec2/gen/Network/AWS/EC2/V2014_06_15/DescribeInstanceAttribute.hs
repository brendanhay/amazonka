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
    , describeInstanceAttribute
    -- ** Request lenses
    , diasAttribute
    , diasInstanceId

    -- * Response
    , DescribeInstanceAttributeResponse
    -- ** Response lenses
    , ibDisableApiTermination
    , ibEbsOptimized
    , ibSourceDestCheck
    , ibInstanceType
    , ibKernelId
    , ibRamdiskId
    , ibUserData
    , ibInstanceInitiatedShutdownBehavior
    , ibRootDeviceName
    , ibSriovNetSupport
    , ibBlockDeviceMappings
    , ibProductCodes
    , ibInstanceId
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeInstanceAttribute' request.
describeInstanceAttribute :: InstanceAttributeName -- ^ 'diasAttribute'
                          -> Text -- ^ 'diasInstanceId'
                          -> DescribeInstanceAttribute
describeInstanceAttribute p1 p2 = DescribeInstanceAttribute
    { _diasAttribute = p1
    , _diasInstanceId = p2
    }
{-# INLINE describeInstanceAttribute #-}

data DescribeInstanceAttribute = DescribeInstanceAttribute
    { _diasAttribute :: InstanceAttributeName
      -- ^ The instance attribute.
    , _diasInstanceId :: Text
      -- ^ The ID of the instance.
    } deriving (Show, Generic)

-- | The instance attribute.
diasAttribute :: Lens' DescribeInstanceAttribute InstanceAttributeName
diasAttribute f x =
    f (_diasAttribute x) <&> \y -> x { _diasAttribute = y }
{-# INLINE diasAttribute #-}

-- | The ID of the instance.
diasInstanceId :: Lens' DescribeInstanceAttribute Text
diasInstanceId f x =
    f (_diasInstanceId x) <&> \y -> x { _diasInstanceId = y }
{-# INLINE diasInstanceId #-}

instance ToQuery DescribeInstanceAttribute where
    toQuery = genericQuery def

data DescribeInstanceAttributeResponse = DescribeInstanceAttributeResponse
    { _ibDisableApiTermination :: Maybe AttributeBooleanValue
      -- ^ If the value is true, you can't terminate the instance through
      -- the Amazon EC2 console, CLI, or API; otherwise, you can.
    , _ibEbsOptimized :: Maybe AttributeBooleanValue
      -- ^ Indicates whether the instance is optimized for EBS I/O.
    , _ibSourceDestCheck :: Maybe AttributeBooleanValue
      -- ^ Indicates whether source/destination checking is enabled. A value
      -- of true means checking is enabled, and false means checking is
      -- disabled. This value must be false for a NAT instance to perform
      -- NAT.
    , _ibInstanceType :: Maybe AttributeValue
      -- ^ The instance type.
    , _ibKernelId :: Maybe AttributeValue
      -- ^ The kernel ID.
    , _ibRamdiskId :: Maybe AttributeValue
      -- ^ The RAM disk ID.
    , _ibUserData :: Maybe AttributeValue
      -- ^ The Base64-encoded MIME user data.
    , _ibInstanceInitiatedShutdownBehavior :: Maybe AttributeValue
      -- ^ Indicates whether an instance stops or terminates when you
      -- initiate shutdown from the instance (using the operating system
      -- command for system shutdown).
    , _ibRootDeviceName :: Maybe AttributeValue
      -- ^ The name of the root device (for example, /dev/sda1).
    , _ibSriovNetSupport :: Maybe AttributeValue
      -- ^ 
    , _ibBlockDeviceMappings :: [InstanceBlockDeviceMapping]
      -- ^ The block device mapping of the instance.
    , _ibProductCodes :: [ProductCode]
      -- ^ A list of product codes.
    , _ibInstanceId :: Maybe Text
      -- ^ The ID of the instance.
    } deriving (Show, Generic)

-- | If the value is true, you can't terminate the instance through the Amazon
-- EC2 console, CLI, or API; otherwise, you can.
ibDisableApiTermination :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeBooleanValue)
ibDisableApiTermination f x =
    f (_ibDisableApiTermination x) <&> \y -> x { _ibDisableApiTermination = y }
{-# INLINE ibDisableApiTermination #-}

-- | Indicates whether the instance is optimized for EBS I/O.
ibEbsOptimized :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeBooleanValue)
ibEbsOptimized f x =
    f (_ibEbsOptimized x) <&> \y -> x { _ibEbsOptimized = y }
{-# INLINE ibEbsOptimized #-}

-- | Indicates whether source/destination checking is enabled. A value of true
-- means checking is enabled, and false means checking is disabled. This value
-- must be false for a NAT instance to perform NAT.
ibSourceDestCheck :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeBooleanValue)
ibSourceDestCheck f x =
    f (_ibSourceDestCheck x) <&> \y -> x { _ibSourceDestCheck = y }
{-# INLINE ibSourceDestCheck #-}

-- | The instance type.
ibInstanceType :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeValue)
ibInstanceType f x =
    f (_ibInstanceType x) <&> \y -> x { _ibInstanceType = y }
{-# INLINE ibInstanceType #-}

-- | The kernel ID.
ibKernelId :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeValue)
ibKernelId f x =
    f (_ibKernelId x) <&> \y -> x { _ibKernelId = y }
{-# INLINE ibKernelId #-}

-- | The RAM disk ID.
ibRamdiskId :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeValue)
ibRamdiskId f x =
    f (_ibRamdiskId x) <&> \y -> x { _ibRamdiskId = y }
{-# INLINE ibRamdiskId #-}

-- | The Base64-encoded MIME user data.
ibUserData :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeValue)
ibUserData f x =
    f (_ibUserData x) <&> \y -> x { _ibUserData = y }
{-# INLINE ibUserData #-}

-- | Indicates whether an instance stops or terminates when you initiate
-- shutdown from the instance (using the operating system command for system
-- shutdown).
ibInstanceInitiatedShutdownBehavior :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeValue)
ibInstanceInitiatedShutdownBehavior f x =
    f (_ibInstanceInitiatedShutdownBehavior x) <&> \y -> x { _ibInstanceInitiatedShutdownBehavior = y }
{-# INLINE ibInstanceInitiatedShutdownBehavior #-}

-- | The name of the root device (for example, /dev/sda1).
ibRootDeviceName :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeValue)
ibRootDeviceName f x =
    f (_ibRootDeviceName x) <&> \y -> x { _ibRootDeviceName = y }
{-# INLINE ibRootDeviceName #-}

-- | 
ibSriovNetSupport :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeValue)
ibSriovNetSupport f x =
    f (_ibSriovNetSupport x) <&> \y -> x { _ibSriovNetSupport = y }
{-# INLINE ibSriovNetSupport #-}

-- | The block device mapping of the instance.
ibBlockDeviceMappings :: Lens' DescribeInstanceAttributeResponse [InstanceBlockDeviceMapping]
ibBlockDeviceMappings f x =
    f (_ibBlockDeviceMappings x) <&> \y -> x { _ibBlockDeviceMappings = y }
{-# INLINE ibBlockDeviceMappings #-}

-- | A list of product codes.
ibProductCodes :: Lens' DescribeInstanceAttributeResponse [ProductCode]
ibProductCodes f x =
    f (_ibProductCodes x) <&> \y -> x { _ibProductCodes = y }
{-# INLINE ibProductCodes #-}

-- | The ID of the instance.
ibInstanceId :: Lens' DescribeInstanceAttributeResponse (Maybe Text)
ibInstanceId f x =
    f (_ibInstanceId x) <&> \y -> x { _ibInstanceId = y }
{-# INLINE ibInstanceId #-}

instance FromXML DescribeInstanceAttributeResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeInstanceAttribute where
    type Sv DescribeInstanceAttribute = EC2
    type Rs DescribeInstanceAttribute = DescribeInstanceAttributeResponse

    request = post "DescribeInstanceAttribute"
    response _ = xmlResponse
