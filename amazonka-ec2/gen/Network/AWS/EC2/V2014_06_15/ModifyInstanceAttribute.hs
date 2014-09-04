{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.ModifyInstanceAttribute
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
module Network.AWS.EC2.V2014_06_15.ModifyInstanceAttribute
    (
    -- * Request
      ModifyInstanceAttribute
    -- ** Request constructor
    , mkModifyInstanceAttributeRequest
    -- ** Request lenses
    , miasInstanceId
    , miasAttribute
    , miasValue
    , miasBlockDeviceMappings
    , miasSourceDestCheck
    , miasDisableApiTermination
    , miasInstanceType
    , miasKernel
    , miasRamdisk
    , miasUserData
    , miasInstanceInitiatedShutdownBehavior
    , miasGroups
    , miasEbsOptimized
    , miasSriovNetSupport

    -- * Response
    , ModifyInstanceAttributeResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ModifyInstanceAttribute' request.
mkModifyInstanceAttributeRequest :: Text -- ^ 'miasInstanceId'
                                 -> ModifyInstanceAttribute
mkModifyInstanceAttributeRequest p1 = ModifyInstanceAttribute
    { _miasInstanceId = p1
    , _miasAttribute = Nothing
    , _miasValue = Nothing
    , _miasBlockDeviceMappings = mempty
    , _miasSourceDestCheck = Nothing
    , _miasDisableApiTermination = Nothing
    , _miasInstanceType = Nothing
    , _miasKernel = Nothing
    , _miasRamdisk = Nothing
    , _miasUserData = Nothing
    , _miasInstanceInitiatedShutdownBehavior = Nothing
    , _miasGroups = mempty
    , _miasEbsOptimized = Nothing
    , _miasSriovNetSupport = Nothing
    }
{-# INLINE mkModifyInstanceAttributeRequest #-}

data ModifyInstanceAttribute = ModifyInstanceAttribute
    { _miasInstanceId :: Text
      -- ^ The ID of the instance.
    , _miasAttribute :: Maybe InstanceAttributeName
      -- ^ The name of the attribute.
    , _miasValue :: Maybe Text
      -- ^ A new value for the attribute. Use only with the kernel, ramdisk,
      -- userData, disableApiTermination, or
      -- intanceInitiateShutdownBehavior attribute.
    , _miasBlockDeviceMappings :: [InstanceBlockDeviceMappingSpecification]
      -- ^ Modifies the DeleteOnTermination attribute for volumes that are
      -- currently attached. The volume must be owned by the caller. If no
      -- value is specified for DeleteOnTermination, the default is true
      -- and the volume is deleted when the instance is terminated. To add
      -- instance store volumes to an Amazon EBS-backed instance, you must
      -- add them when you launch the instance. For more information, see
      -- Updating the Block Device Mapping when Launching an Instance in
      -- the Amazon Elastic Compute Cloud User Guide.
    , _miasSourceDestCheck :: Maybe AttributeBooleanValue
      -- ^ Specifies whether source/destination checking is enabled. A value
      -- of true means that checking is enabled, and false means checking
      -- is disabled. This value must be false for a NAT instance to
      -- perform NAT.
    , _miasDisableApiTermination :: Maybe AttributeBooleanValue
      -- ^ Specifies whether to disable the ability to terminate the
      -- instance using the Amazon EC2 console, CLI, and API.
    , _miasInstanceType :: Maybe AttributeValue
      -- ^ Changes the instance type to the specified value. For more
      -- information, see Instance Types. If the instance type is not
      -- valid, the error returned is InvalidInstanceAttributeValue.
    , _miasKernel :: Maybe AttributeValue
      -- ^ Changes the instance's kernel to the specified value.
    , _miasRamdisk :: Maybe AttributeValue
      -- ^ Changes the instance's RAM disk to the specified value.
    , _miasUserData :: Maybe AttributeValue
      -- ^ Changes the instance's user data to the specified value.
    , _miasInstanceInitiatedShutdownBehavior :: Maybe AttributeValue
      -- ^ Specifies whether an instance stops or terminates when you
      -- initiate shutdown from the instance (using the operating system
      -- command for system shutdown).
    , _miasGroups :: [Text]
      -- ^ [EC2-VPC] Changes the security groups of the instance. You must
      -- specify at least one security group, even if it's just the
      -- default security group for the VPC. You must specify the security
      -- group ID, not the security group name. For example, if you want
      -- the instance to be in sg-1a1a1a1a and sg-9b9b9b9b, specify
      -- GroupId.1=sg-1a1a1a1a and GroupId.2=sg-9b9b9b9b.
    , _miasEbsOptimized :: Maybe AttributeBooleanValue
      -- ^ Specifies whether the instance is optimized for EBS I/O. This
      -- optimization provides dedicated throughput to Amazon EBS and an
      -- optimized configuration stack to provide optimal EBS I/O
      -- performance. This optimization isn't available with all instance
      -- types. Additional usage charges apply when using an EBS Optimized
      -- instance.
    , _miasSriovNetSupport :: Maybe AttributeValue
      -- ^ Set to simple to enable enhanced networking for the instance.
      -- There is no way to disable enhanced networking at this time. This
      -- option is supported only for HVM instances. Specifying this
      -- option with a PV instance can make it unreachable.
    } deriving (Show, Generic)

-- | The ID of the instance.
miasInstanceId :: Lens' ModifyInstanceAttribute (Text)
miasInstanceId = lens _miasInstanceId (\s a -> s { _miasInstanceId = a })
{-# INLINE miasInstanceId #-}

-- | The name of the attribute.
miasAttribute :: Lens' ModifyInstanceAttribute (Maybe InstanceAttributeName)
miasAttribute = lens _miasAttribute (\s a -> s { _miasAttribute = a })
{-# INLINE miasAttribute #-}

-- | A new value for the attribute. Use only with the kernel, ramdisk, userData,
-- disableApiTermination, or intanceInitiateShutdownBehavior attribute.
miasValue :: Lens' ModifyInstanceAttribute (Maybe Text)
miasValue = lens _miasValue (\s a -> s { _miasValue = a })
{-# INLINE miasValue #-}

-- | Modifies the DeleteOnTermination attribute for volumes that are currently
-- attached. The volume must be owned by the caller. If no value is specified
-- for DeleteOnTermination, the default is true and the volume is deleted when
-- the instance is terminated. To add instance store volumes to an Amazon
-- EBS-backed instance, you must add them when you launch the instance. For
-- more information, see Updating the Block Device Mapping when Launching an
-- Instance in the Amazon Elastic Compute Cloud User Guide.
miasBlockDeviceMappings :: Lens' ModifyInstanceAttribute ([InstanceBlockDeviceMappingSpecification])
miasBlockDeviceMappings = lens _miasBlockDeviceMappings (\s a -> s { _miasBlockDeviceMappings = a })
{-# INLINE miasBlockDeviceMappings #-}

-- | Specifies whether source/destination checking is enabled. A value of true
-- means that checking is enabled, and false means checking is disabled. This
-- value must be false for a NAT instance to perform NAT.
miasSourceDestCheck :: Lens' ModifyInstanceAttribute (Maybe AttributeBooleanValue)
miasSourceDestCheck = lens _miasSourceDestCheck (\s a -> s { _miasSourceDestCheck = a })
{-# INLINE miasSourceDestCheck #-}

-- | Specifies whether to disable the ability to terminate the instance using
-- the Amazon EC2 console, CLI, and API.
miasDisableApiTermination :: Lens' ModifyInstanceAttribute (Maybe AttributeBooleanValue)
miasDisableApiTermination = lens _miasDisableApiTermination (\s a -> s { _miasDisableApiTermination = a })
{-# INLINE miasDisableApiTermination #-}

-- | Changes the instance type to the specified value. For more information, see
-- Instance Types. If the instance type is not valid, the error returned is
-- InvalidInstanceAttributeValue.
miasInstanceType :: Lens' ModifyInstanceAttribute (Maybe AttributeValue)
miasInstanceType = lens _miasInstanceType (\s a -> s { _miasInstanceType = a })
{-# INLINE miasInstanceType #-}

-- | Changes the instance's kernel to the specified value.
miasKernel :: Lens' ModifyInstanceAttribute (Maybe AttributeValue)
miasKernel = lens _miasKernel (\s a -> s { _miasKernel = a })
{-# INLINE miasKernel #-}

-- | Changes the instance's RAM disk to the specified value.
miasRamdisk :: Lens' ModifyInstanceAttribute (Maybe AttributeValue)
miasRamdisk = lens _miasRamdisk (\s a -> s { _miasRamdisk = a })
{-# INLINE miasRamdisk #-}

-- | Changes the instance's user data to the specified value.
miasUserData :: Lens' ModifyInstanceAttribute (Maybe AttributeValue)
miasUserData = lens _miasUserData (\s a -> s { _miasUserData = a })
{-# INLINE miasUserData #-}

-- | Specifies whether an instance stops or terminates when you initiate
-- shutdown from the instance (using the operating system command for system
-- shutdown).
miasInstanceInitiatedShutdownBehavior :: Lens' ModifyInstanceAttribute (Maybe AttributeValue)
miasInstanceInitiatedShutdownBehavior = lens _miasInstanceInitiatedShutdownBehavior (\s a -> s { _miasInstanceInitiatedShutdownBehavior = a })
{-# INLINE miasInstanceInitiatedShutdownBehavior #-}

-- | [EC2-VPC] Changes the security groups of the instance. You must specify at
-- least one security group, even if it's just the default security group for
-- the VPC. You must specify the security group ID, not the security group
-- name. For example, if you want the instance to be in sg-1a1a1a1a and
-- sg-9b9b9b9b, specify GroupId.1=sg-1a1a1a1a and GroupId.2=sg-9b9b9b9b.
miasGroups :: Lens' ModifyInstanceAttribute ([Text])
miasGroups = lens _miasGroups (\s a -> s { _miasGroups = a })
{-# INLINE miasGroups #-}

-- | Specifies whether the instance is optimized for EBS I/O. This optimization
-- provides dedicated throughput to Amazon EBS and an optimized configuration
-- stack to provide optimal EBS I/O performance. This optimization isn't
-- available with all instance types. Additional usage charges apply when
-- using an EBS Optimized instance.
miasEbsOptimized :: Lens' ModifyInstanceAttribute (Maybe AttributeBooleanValue)
miasEbsOptimized = lens _miasEbsOptimized (\s a -> s { _miasEbsOptimized = a })
{-# INLINE miasEbsOptimized #-}

-- | Set to simple to enable enhanced networking for the instance. There is no
-- way to disable enhanced networking at this time. This option is supported
-- only for HVM instances. Specifying this option with a PV instance can make
-- it unreachable.
miasSriovNetSupport :: Lens' ModifyInstanceAttribute (Maybe AttributeValue)
miasSriovNetSupport = lens _miasSriovNetSupport (\s a -> s { _miasSriovNetSupport = a })
{-# INLINE miasSriovNetSupport #-}

instance ToQuery ModifyInstanceAttribute where
    toQuery = genericQuery def

data ModifyInstanceAttributeResponse = ModifyInstanceAttributeResponse
    deriving (Eq, Show, Generic)

instance AWSRequest ModifyInstanceAttribute where
    type Sv ModifyInstanceAttribute = EC2
    type Rs ModifyInstanceAttribute = ModifyInstanceAttributeResponse

    request = post "ModifyInstanceAttribute"
    response _ = nullaryResponse ModifyInstanceAttributeResponse
