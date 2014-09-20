{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeInstanceAttribute
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
module Network.AWS.EC2.DescribeInstanceAttribute
    (
    -- * Request
      DescribeInstanceAttribute
    -- ** Request constructor
    , describeInstanceAttribute
    -- ** Request lenses
    , dia1InstanceId
    , dia1Attribute

    -- * Response
    , DescribeInstanceAttributeResponse
    -- ** Response constructor
    , describeInstanceAttributeResponse
    -- ** Response lenses
    , diarrInstanceId
    , diarrInstanceType
    , diarrKernelId
    , diarrRamdiskId
    , diarrUserData
    , diarrDisableApiTermination
    , diarrInstanceInitiatedShutdownBehavior
    , diarrRootDeviceName
    , diarrBlockDeviceMappings
    , diarrProductCodes
    , diarrEbsOptimized
    , diarrSriovNetSupport
    , diarrSourceDestCheck
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

data DescribeInstanceAttribute = DescribeInstanceAttribute
    { _dia1InstanceId :: Text
    , _dia1Attribute :: InstanceAttributeName
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeInstanceAttribute' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @InstanceId ::@ @Text@
--
-- * @Attribute ::@ @InstanceAttributeName@
--
describeInstanceAttribute :: Text -- ^ 'dia1InstanceId'
                          -> InstanceAttributeName -- ^ 'dia1Attribute'
                          -> DescribeInstanceAttribute
describeInstanceAttribute p1 p2 = DescribeInstanceAttribute
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

data DescribeInstanceAttributeResponse = DescribeInstanceAttributeResponse
    { _diarrInstanceId :: Maybe Text
    , _diarrInstanceType :: Maybe AttributeValue
    , _diarrKernelId :: Maybe AttributeValue
    , _diarrRamdiskId :: Maybe AttributeValue
    , _diarrUserData :: Maybe AttributeValue
    , _diarrDisableApiTermination :: Maybe AttributeBooleanValue
    , _diarrInstanceInitiatedShutdownBehavior :: Maybe AttributeValue
    , _diarrRootDeviceName :: Maybe AttributeValue
    , _diarrBlockDeviceMappings :: [InstanceBlockDeviceMapping]
    , _diarrProductCodes :: [ProductCode]
    , _diarrEbsOptimized :: Maybe AttributeBooleanValue
    , _diarrSriovNetSupport :: Maybe AttributeValue
    , _diarrSourceDestCheck :: Maybe AttributeBooleanValue
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeInstanceAttributeResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @InstanceId ::@ @Maybe Text@
--
-- * @InstanceType ::@ @Maybe AttributeValue@
--
-- * @KernelId ::@ @Maybe AttributeValue@
--
-- * @RamdiskId ::@ @Maybe AttributeValue@
--
-- * @UserData ::@ @Maybe AttributeValue@
--
-- * @DisableApiTermination ::@ @Maybe AttributeBooleanValue@
--
-- * @InstanceInitiatedShutdownBehavior ::@ @Maybe AttributeValue@
--
-- * @RootDeviceName ::@ @Maybe AttributeValue@
--
-- * @BlockDeviceMappings ::@ @[InstanceBlockDeviceMapping]@
--
-- * @ProductCodes ::@ @[ProductCode]@
--
-- * @EbsOptimized ::@ @Maybe AttributeBooleanValue@
--
-- * @SriovNetSupport ::@ @Maybe AttributeValue@
--
-- * @SourceDestCheck ::@ @Maybe AttributeBooleanValue@
--
describeInstanceAttributeResponse :: DescribeInstanceAttributeResponse
describeInstanceAttributeResponse = DescribeInstanceAttributeResponse
    { _diarrInstanceId = Nothing
    , _diarrInstanceType = Nothing
    , _diarrKernelId = Nothing
    , _diarrRamdiskId = Nothing
    , _diarrUserData = Nothing
    , _diarrDisableApiTermination = Nothing
    , _diarrInstanceInitiatedShutdownBehavior = Nothing
    , _diarrRootDeviceName = Nothing
    , _diarrBlockDeviceMappings = mempty
    , _diarrProductCodes = mempty
    , _diarrEbsOptimized = Nothing
    , _diarrSriovNetSupport = Nothing
    , _diarrSourceDestCheck = Nothing
    }

-- | The ID of the instance.
diarrInstanceId :: Lens' DescribeInstanceAttributeResponse (Maybe Text)
diarrInstanceId = lens _diarrInstanceId (\s a -> s { _diarrInstanceId = a })

-- | The instance type.
diarrInstanceType :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeValue)
diarrInstanceType =
    lens _diarrInstanceType (\s a -> s { _diarrInstanceType = a })

-- | The kernel ID.
diarrKernelId :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeValue)
diarrKernelId = lens _diarrKernelId (\s a -> s { _diarrKernelId = a })

-- | The RAM disk ID.
diarrRamdiskId :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeValue)
diarrRamdiskId = lens _diarrRamdiskId (\s a -> s { _diarrRamdiskId = a })

-- | The Base64-encoded MIME user data.
diarrUserData :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeValue)
diarrUserData = lens _diarrUserData (\s a -> s { _diarrUserData = a })

-- | If the value is true, you can't terminate the instance through the Amazon
-- EC2 console, CLI, or API; otherwise, you can.
diarrDisableApiTermination :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeBooleanValue)
diarrDisableApiTermination =
    lens _diarrDisableApiTermination
         (\s a -> s { _diarrDisableApiTermination = a })

-- | Indicates whether an instance stops or terminates when you initiate
-- shutdown from the instance (using the operating system command for system
-- shutdown).
diarrInstanceInitiatedShutdownBehavior :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeValue)
diarrInstanceInitiatedShutdownBehavior =
    lens _diarrInstanceInitiatedShutdownBehavior
         (\s a -> s { _diarrInstanceInitiatedShutdownBehavior = a })

-- | The name of the root device (for example, /dev/sda1).
diarrRootDeviceName :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeValue)
diarrRootDeviceName =
    lens _diarrRootDeviceName (\s a -> s { _diarrRootDeviceName = a })

-- | The block device mapping of the instance.
diarrBlockDeviceMappings :: Lens' DescribeInstanceAttributeResponse [InstanceBlockDeviceMapping]
diarrBlockDeviceMappings =
    lens _diarrBlockDeviceMappings
         (\s a -> s { _diarrBlockDeviceMappings = a })

-- | A list of product codes.
diarrProductCodes :: Lens' DescribeInstanceAttributeResponse [ProductCode]
diarrProductCodes =
    lens _diarrProductCodes (\s a -> s { _diarrProductCodes = a })

-- | Indicates whether the instance is optimized for EBS I/O.
diarrEbsOptimized :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeBooleanValue)
diarrEbsOptimized =
    lens _diarrEbsOptimized (\s a -> s { _diarrEbsOptimized = a })

-- | 
diarrSriovNetSupport :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeValue)
diarrSriovNetSupport =
    lens _diarrSriovNetSupport (\s a -> s { _diarrSriovNetSupport = a })

-- | Indicates whether source/destination checking is enabled. A value of true
-- means checking is enabled, and false means checking is disabled. This value
-- must be false for a NAT instance to perform NAT.
diarrSourceDestCheck :: Lens' DescribeInstanceAttributeResponse (Maybe AttributeBooleanValue)
diarrSourceDestCheck =
    lens _diarrSourceDestCheck (\s a -> s { _diarrSourceDestCheck = a })

instance FromXML DescribeInstanceAttributeResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeInstanceAttribute where
    type Sv DescribeInstanceAttribute = EC2
    type Rs DescribeInstanceAttribute = DescribeInstanceAttributeResponse

    request = post "DescribeInstanceAttribute"
    response _ = xmlResponse
