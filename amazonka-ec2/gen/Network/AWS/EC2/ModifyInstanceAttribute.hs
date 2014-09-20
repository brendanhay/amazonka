{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.ModifyInstanceAttribute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Modifies the specified attribute of the specified instance. You can specify
-- only one attribute at a time. To modify some attributes, the instance must
-- be stopped. For more information, see Modifying Attributes of a Stopped
-- Instance in the Amazon Elastic Compute Cloud User Guide. Example 1 This
-- example changes the instance type of the specified instance. The instance
-- must be in the stopped state.
-- https://ec2.amazonaws.com/?Action=ModifyInstanceAttribute
-- &amp;InstanceId=i-10a64379 &amp;InstanceType.Value=m1.small &amp;AUTHPARAMS
-- &lt;ModifyInstanceAttributeResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/ModifyInstanceAttributeResponse&gt;
-- Example 2 This example changes the InstanceInitiatedShutdownBehavior
-- attribute of the specified instance.
-- https://ec2.amazonaws.com/?Action=ModifyInstanceAttribute
-- &amp;InstanceId=i-10a64379
-- &amp;InstanceInitiatedShutdownBehavior.Value=terminate &amp;AUTHPARAMS
-- &lt;ModifyInstanceAttributeResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/ModifyInstanceAttributeResponse&gt;
-- Example 3 This example changes the DisableApiTermination attribute of the
-- specified instance.
-- https://ec2.amazonaws.com/?Action=ModifyInstanceAttribute
-- &amp;InstanceId=i-10a64379 &amp;DisableApiTermination.Value=true
-- &amp;AUTHPARAMS &lt;ModifyInstanceAttributeResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/ModifyInstanceAttributeResponse&gt;.
module Network.AWS.EC2.ModifyInstanceAttribute
    (
    -- * Request
      ModifyInstanceAttribute
    -- ** Request constructor
    , modifyInstanceAttribute
    -- ** Request lenses
    , mia1InstanceId
    , mia1Attribute
    , mia1Value
    , mia1BlockDeviceMappings
    , mia1SourceDestCheck
    , mia1DisableApiTermination
    , mia1InstanceType
    , mia1Kernel
    , mia1Ramdisk
    , mia1UserData
    , mia1InstanceInitiatedShutdownBehavior
    , mia1Groups
    , mia1EbsOptimized
    , mia1SriovNetSupport

    -- * Response
    , ModifyInstanceAttributeResponse
    -- ** Response constructor
    , modifyInstanceAttributeResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

data ModifyInstanceAttribute = ModifyInstanceAttribute
    { _mia1InstanceId :: Text
    , _mia1Attribute :: Maybe InstanceAttributeName
    , _mia1Value :: Maybe Text
    , _mia1BlockDeviceMappings :: [InstanceBlockDeviceMappingSpecification]
    , _mia1SourceDestCheck :: Maybe AttributeBooleanValue
    , _mia1DisableApiTermination :: Maybe AttributeBooleanValue
    , _mia1InstanceType :: Maybe AttributeValue
    , _mia1Kernel :: Maybe AttributeValue
    , _mia1Ramdisk :: Maybe AttributeValue
    , _mia1UserData :: Maybe AttributeValue
    , _mia1InstanceInitiatedShutdownBehavior :: Maybe AttributeValue
    , _mia1Groups :: [Text]
    , _mia1EbsOptimized :: Maybe AttributeBooleanValue
    , _mia1SriovNetSupport :: Maybe AttributeValue
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ModifyInstanceAttribute' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @InstanceId ::@ @Text@
--
-- * @Attribute ::@ @Maybe InstanceAttributeName@
--
-- * @Value ::@ @Maybe Text@
--
-- * @BlockDeviceMappings ::@ @[InstanceBlockDeviceMappingSpecification]@
--
-- * @SourceDestCheck ::@ @Maybe AttributeBooleanValue@
--
-- * @DisableApiTermination ::@ @Maybe AttributeBooleanValue@
--
-- * @InstanceType ::@ @Maybe AttributeValue@
--
-- * @Kernel ::@ @Maybe AttributeValue@
--
-- * @Ramdisk ::@ @Maybe AttributeValue@
--
-- * @UserData ::@ @Maybe AttributeValue@
--
-- * @InstanceInitiatedShutdownBehavior ::@ @Maybe AttributeValue@
--
-- * @Groups ::@ @[Text]@
--
-- * @EbsOptimized ::@ @Maybe AttributeBooleanValue@
--
-- * @SriovNetSupport ::@ @Maybe AttributeValue@
--
modifyInstanceAttribute :: Text -- ^ 'mia1InstanceId'
                        -> ModifyInstanceAttribute
modifyInstanceAttribute p1 = ModifyInstanceAttribute
    { _mia1InstanceId = p1
    , _mia1Attribute = Nothing
    , _mia1Value = Nothing
    , _mia1BlockDeviceMappings = mempty
    , _mia1SourceDestCheck = Nothing
    , _mia1DisableApiTermination = Nothing
    , _mia1InstanceType = Nothing
    , _mia1Kernel = Nothing
    , _mia1Ramdisk = Nothing
    , _mia1UserData = Nothing
    , _mia1InstanceInitiatedShutdownBehavior = Nothing
    , _mia1Groups = mempty
    , _mia1EbsOptimized = Nothing
    , _mia1SriovNetSupport = Nothing
    }

-- | The ID of the instance.
mia1InstanceId :: Lens' ModifyInstanceAttribute Text
mia1InstanceId = lens _mia1InstanceId (\s a -> s { _mia1InstanceId = a })

-- | The name of the attribute.
mia1Attribute :: Lens' ModifyInstanceAttribute (Maybe InstanceAttributeName)
mia1Attribute = lens _mia1Attribute (\s a -> s { _mia1Attribute = a })

-- | A new value for the attribute. Use only with the kernel, ramdisk, userData,
-- disableApiTermination, or intanceInitiateShutdownBehavior attribute.
mia1Value :: Lens' ModifyInstanceAttribute (Maybe Text)
mia1Value = lens _mia1Value (\s a -> s { _mia1Value = a })

-- | Modifies the DeleteOnTermination attribute for volumes that are currently
-- attached. The volume must be owned by the caller. If no value is specified
-- for DeleteOnTermination, the default is true and the volume is deleted when
-- the instance is terminated. To add instance store volumes to an Amazon
-- EBS-backed instance, you must add them when you launch the instance. For
-- more information, see Updating the Block Device Mapping when Launching an
-- Instance in the Amazon Elastic Compute Cloud User Guide.
mia1BlockDeviceMappings :: Lens' ModifyInstanceAttribute [InstanceBlockDeviceMappingSpecification]
mia1BlockDeviceMappings =
    lens _mia1BlockDeviceMappings
         (\s a -> s { _mia1BlockDeviceMappings = a })

-- | Specifies whether source/destination checking is enabled. A value of true
-- means that checking is enabled, and false means checking is disabled. This
-- value must be false for a NAT instance to perform NAT.
mia1SourceDestCheck :: Lens' ModifyInstanceAttribute (Maybe AttributeBooleanValue)
mia1SourceDestCheck =
    lens _mia1SourceDestCheck (\s a -> s { _mia1SourceDestCheck = a })

-- | Specifies whether to disable the ability to terminate the instance using
-- the Amazon EC2 console, CLI, and API.
mia1DisableApiTermination :: Lens' ModifyInstanceAttribute (Maybe AttributeBooleanValue)
mia1DisableApiTermination =
    lens _mia1DisableApiTermination
         (\s a -> s { _mia1DisableApiTermination = a })

-- | Changes the instance type to the specified value. For more information, see
-- Instance Types. If the instance type is not valid, the error returned is
-- InvalidInstanceAttributeValue.
mia1InstanceType :: Lens' ModifyInstanceAttribute (Maybe AttributeValue)
mia1InstanceType =
    lens _mia1InstanceType (\s a -> s { _mia1InstanceType = a })

-- | Changes the instance's kernel to the specified value.
mia1Kernel :: Lens' ModifyInstanceAttribute (Maybe AttributeValue)
mia1Kernel = lens _mia1Kernel (\s a -> s { _mia1Kernel = a })

-- | Changes the instance's RAM disk to the specified value.
mia1Ramdisk :: Lens' ModifyInstanceAttribute (Maybe AttributeValue)
mia1Ramdisk = lens _mia1Ramdisk (\s a -> s { _mia1Ramdisk = a })

-- | Changes the instance's user data to the specified value.
mia1UserData :: Lens' ModifyInstanceAttribute (Maybe AttributeValue)
mia1UserData = lens _mia1UserData (\s a -> s { _mia1UserData = a })

-- | Specifies whether an instance stops or terminates when you initiate
-- shutdown from the instance (using the operating system command for system
-- shutdown).
mia1InstanceInitiatedShutdownBehavior :: Lens' ModifyInstanceAttribute (Maybe AttributeValue)
mia1InstanceInitiatedShutdownBehavior =
    lens _mia1InstanceInitiatedShutdownBehavior
         (\s a -> s { _mia1InstanceInitiatedShutdownBehavior = a })

-- | [EC2-VPC] Changes the security groups of the instance. You must specify at
-- least one security group, even if it's just the default security group for
-- the VPC. You must specify the security group ID, not the security group
-- name. For example, if you want the instance to be in sg-1a1a1a1a and
-- sg-9b9b9b9b, specify GroupId.1=sg-1a1a1a1a and GroupId.2=sg-9b9b9b9b.
mia1Groups :: Lens' ModifyInstanceAttribute [Text]
mia1Groups = lens _mia1Groups (\s a -> s { _mia1Groups = a })

-- | Specifies whether the instance is optimized for EBS I/O. This optimization
-- provides dedicated throughput to Amazon EBS and an optimized configuration
-- stack to provide optimal EBS I/O performance. This optimization isn't
-- available with all instance types. Additional usage charges apply when
-- using an EBS Optimized instance.
mia1EbsOptimized :: Lens' ModifyInstanceAttribute (Maybe AttributeBooleanValue)
mia1EbsOptimized =
    lens _mia1EbsOptimized (\s a -> s { _mia1EbsOptimized = a })

-- | Set to simple to enable enhanced networking for the instance. There is no
-- way to disable enhanced networking at this time. This option is supported
-- only for HVM instances. Specifying this option with a PV instance can make
-- it unreachable.
mia1SriovNetSupport :: Lens' ModifyInstanceAttribute (Maybe AttributeValue)
mia1SriovNetSupport =
    lens _mia1SriovNetSupport (\s a -> s { _mia1SriovNetSupport = a })

instance ToQuery ModifyInstanceAttribute where
    toQuery = genericQuery def

data ModifyInstanceAttributeResponse = ModifyInstanceAttributeResponse
    deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ModifyInstanceAttributeResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
modifyInstanceAttributeResponse :: ModifyInstanceAttributeResponse
modifyInstanceAttributeResponse = ModifyInstanceAttributeResponse

instance AWSRequest ModifyInstanceAttribute where
    type Sv ModifyInstanceAttribute = EC2
    type Rs ModifyInstanceAttribute = ModifyInstanceAttributeResponse

    request = post "ModifyInstanceAttribute"
    response _ = nullaryResponse ModifyInstanceAttributeResponse
