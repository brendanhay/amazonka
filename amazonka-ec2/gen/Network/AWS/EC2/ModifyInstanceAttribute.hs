{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
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
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Modifies the specified attribute of the specified instance. You can specify
-- only one attribute at a time.
--
-- To modify some attributes, the instance must be stopped. For more
-- information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_ChangingAttributesWhileInstanceStopped.html Modifying Attributes of a Stopped Instance> in the /AmazonElastic Compute Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ModifyInstanceAttribute.html>
module Network.AWS.EC2.ModifyInstanceAttribute
    (
    -- * Request
      ModifyInstanceAttribute
    -- ** Request constructor
    , modifyInstanceAttribute
    -- ** Request lenses
    , mia1Attribute
    , mia1BlockDeviceMappings
    , mia1DisableApiTermination
    , mia1DryRun
    , mia1EbsOptimized
    , mia1Groups
    , mia1InstanceId
    , mia1InstanceInitiatedShutdownBehavior
    , mia1InstanceType
    , mia1Kernel
    , mia1Ramdisk
    , mia1SourceDestCheck
    , mia1SriovNetSupport
    , mia1UserData
    , mia1Value

    -- * Response
    , ModifyInstanceAttributeResponse
    -- ** Response constructor
    , modifyInstanceAttributeResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data ModifyInstanceAttribute = ModifyInstanceAttribute
    { _mia1Attribute                         :: Maybe InstanceAttributeName
    , _mia1BlockDeviceMappings               :: List "item" InstanceBlockDeviceMappingSpecification
    , _mia1DisableApiTermination             :: Maybe AttributeBooleanValue
    , _mia1DryRun                            :: Maybe Bool
    , _mia1EbsOptimized                      :: Maybe AttributeBooleanValue
    , _mia1Groups                            :: List "groupId" Text
    , _mia1InstanceId                        :: Text
    , _mia1InstanceInitiatedShutdownBehavior :: Maybe AttributeValue
    , _mia1InstanceType                      :: Maybe AttributeValue
    , _mia1Kernel                            :: Maybe AttributeValue
    , _mia1Ramdisk                           :: Maybe AttributeValue
    , _mia1SourceDestCheck                   :: Maybe AttributeBooleanValue
    , _mia1SriovNetSupport                   :: Maybe AttributeValue
    , _mia1UserData                          :: Maybe BlobAttributeValue
    , _mia1Value                             :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'ModifyInstanceAttribute' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mia1Attribute' @::@ 'Maybe' 'InstanceAttributeName'
--
-- * 'mia1BlockDeviceMappings' @::@ ['InstanceBlockDeviceMappingSpecification']
--
-- * 'mia1DisableApiTermination' @::@ 'Maybe' 'AttributeBooleanValue'
--
-- * 'mia1DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'mia1EbsOptimized' @::@ 'Maybe' 'AttributeBooleanValue'
--
-- * 'mia1Groups' @::@ ['Text']
--
-- * 'mia1InstanceId' @::@ 'Text'
--
-- * 'mia1InstanceInitiatedShutdownBehavior' @::@ 'Maybe' 'AttributeValue'
--
-- * 'mia1InstanceType' @::@ 'Maybe' 'AttributeValue'
--
-- * 'mia1Kernel' @::@ 'Maybe' 'AttributeValue'
--
-- * 'mia1Ramdisk' @::@ 'Maybe' 'AttributeValue'
--
-- * 'mia1SourceDestCheck' @::@ 'Maybe' 'AttributeBooleanValue'
--
-- * 'mia1SriovNetSupport' @::@ 'Maybe' 'AttributeValue'
--
-- * 'mia1UserData' @::@ 'Maybe' 'BlobAttributeValue'
--
-- * 'mia1Value' @::@ 'Maybe' 'Text'
--
modifyInstanceAttribute :: Text -- ^ 'mia1InstanceId'
                        -> ModifyInstanceAttribute
modifyInstanceAttribute p1 = ModifyInstanceAttribute
    { _mia1InstanceId                        = p1
    , _mia1DryRun                            = Nothing
    , _mia1Attribute                         = Nothing
    , _mia1Value                             = Nothing
    , _mia1BlockDeviceMappings               = mempty
    , _mia1SourceDestCheck                   = Nothing
    , _mia1DisableApiTermination             = Nothing
    , _mia1InstanceType                      = Nothing
    , _mia1Kernel                            = Nothing
    , _mia1Ramdisk                           = Nothing
    , _mia1UserData                          = Nothing
    , _mia1InstanceInitiatedShutdownBehavior = Nothing
    , _mia1Groups                            = mempty
    , _mia1EbsOptimized                      = Nothing
    , _mia1SriovNetSupport                   = Nothing
    }

-- | The name of the attribute.
mia1Attribute :: Lens' ModifyInstanceAttribute (Maybe InstanceAttributeName)
mia1Attribute = lens _mia1Attribute (\s a -> s { _mia1Attribute = a })

-- | Modifies the 'DeleteOnTermination' attribute for volumes that are currently
-- attached. The volume must be owned by the caller. If no value is specified
-- for 'DeleteOnTermination', the default is 'true' and the volume is deleted when
-- the instance is terminated.
--
-- To add instance store volumes to an Amazon EBS-backed instance, you must add
-- them when you launch the instance. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/block-device-mapping-concepts.html#Using_OverridingAMIBDM Updating theBlock Device Mapping when Launching an Instance> in the /Amazon Elastic ComputeCloud User Guide/.
mia1BlockDeviceMappings :: Lens' ModifyInstanceAttribute [InstanceBlockDeviceMappingSpecification]
mia1BlockDeviceMappings =
    lens _mia1BlockDeviceMappings (\s a -> s { _mia1BlockDeviceMappings = a })
        . _List

-- | If the value is 'true', you can't terminate the instance using the Amazon EC2
-- console, CLI, or API; otherwise, you can.
mia1DisableApiTermination :: Lens' ModifyInstanceAttribute (Maybe AttributeBooleanValue)
mia1DisableApiTermination =
    lens _mia1DisableApiTermination
        (\s a -> s { _mia1DisableApiTermination = a })

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have the
-- required permissions, the error response is 'DryRunOperation'. Otherwise, it is 'UnauthorizedOperation'.
mia1DryRun :: Lens' ModifyInstanceAttribute (Maybe Bool)
mia1DryRun = lens _mia1DryRun (\s a -> s { _mia1DryRun = a })

-- | Specifies whether the instance is optimized for EBS I/O. This optimization
-- provides dedicated throughput to Amazon EBS and an optimized configuration
-- stack to provide optimal EBS I/O performance. This optimization isn't
-- available with all instance types. Additional usage charges apply when using
-- an EBS Optimized instance.
mia1EbsOptimized :: Lens' ModifyInstanceAttribute (Maybe AttributeBooleanValue)
mia1EbsOptimized = lens _mia1EbsOptimized (\s a -> s { _mia1EbsOptimized = a })

-- | [EC2-VPC] Changes the security groups of the instance. You must specify at
-- least one security group, even if it's just the default security group for
-- the VPC. You must specify the security group ID, not the security group name.
mia1Groups :: Lens' ModifyInstanceAttribute [Text]
mia1Groups = lens _mia1Groups (\s a -> s { _mia1Groups = a }) . _List

-- | The ID of the instance.
mia1InstanceId :: Lens' ModifyInstanceAttribute Text
mia1InstanceId = lens _mia1InstanceId (\s a -> s { _mia1InstanceId = a })

-- | Specifies whether an instance stops or terminates when you initiate shutdown
-- from the instance (using the operating system command for system shutdown).
mia1InstanceInitiatedShutdownBehavior :: Lens' ModifyInstanceAttribute (Maybe AttributeValue)
mia1InstanceInitiatedShutdownBehavior =
    lens _mia1InstanceInitiatedShutdownBehavior
        (\s a -> s { _mia1InstanceInitiatedShutdownBehavior = a })

-- | Changes the instance type to the specified value. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types>. If the instance type is not valid, the error returned is 'InvalidInstanceAttributeValue'.
mia1InstanceType :: Lens' ModifyInstanceAttribute (Maybe AttributeValue)
mia1InstanceType = lens _mia1InstanceType (\s a -> s { _mia1InstanceType = a })

-- | Changes the instance's kernel to the specified value. We recommend that you
-- use PV-GRUB instead of kernels and RAM disks. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedKernels.html PV-GRUB>.
mia1Kernel :: Lens' ModifyInstanceAttribute (Maybe AttributeValue)
mia1Kernel = lens _mia1Kernel (\s a -> s { _mia1Kernel = a })

-- | Changes the instance's RAM disk to the specified value. We recommend that you
-- use PV-GRUB instead of kernels and RAM disks. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedKernels.html PV-GRUB>.
mia1Ramdisk :: Lens' ModifyInstanceAttribute (Maybe AttributeValue)
mia1Ramdisk = lens _mia1Ramdisk (\s a -> s { _mia1Ramdisk = a })

-- | Specifies whether source/destination checking is enabled. A value of 'true'
-- means that checking is enabled, and 'false' means checking is disabled. This
-- value must be 'false' for a NAT instance to perform NAT.
mia1SourceDestCheck :: Lens' ModifyInstanceAttribute (Maybe AttributeBooleanValue)
mia1SourceDestCheck =
    lens _mia1SourceDestCheck (\s a -> s { _mia1SourceDestCheck = a })

-- | Set to 'simple' to enable enhanced networking for the instance.
--
-- There is no way to disable enhanced networking at this time.
--
-- This option is supported only for HVM instances. Specifying this option with
-- a PV instance can make it unreachable.
mia1SriovNetSupport :: Lens' ModifyInstanceAttribute (Maybe AttributeValue)
mia1SriovNetSupport =
    lens _mia1SriovNetSupport (\s a -> s { _mia1SriovNetSupport = a })

-- | Changes the instance's user data to the specified value.
mia1UserData :: Lens' ModifyInstanceAttribute (Maybe BlobAttributeValue)
mia1UserData = lens _mia1UserData (\s a -> s { _mia1UserData = a })

-- | A new value for the attribute. Use only with the 'kernel', 'ramdisk', 'userData', 'disableApiTermination', or 'intanceInitiateShutdownBehavior' attribute.
mia1Value :: Lens' ModifyInstanceAttribute (Maybe Text)
mia1Value = lens _mia1Value (\s a -> s { _mia1Value = a })

data ModifyInstanceAttributeResponse = ModifyInstanceAttributeResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'ModifyInstanceAttributeResponse' constructor.
modifyInstanceAttributeResponse :: ModifyInstanceAttributeResponse
modifyInstanceAttributeResponse = ModifyInstanceAttributeResponse

instance ToPath ModifyInstanceAttribute where
    toPath = const "/"

instance ToQuery ModifyInstanceAttribute where
    toQuery ModifyInstanceAttribute{..} = mconcat
        [ "Attribute"                         =? _mia1Attribute
        , "BlockDeviceMapping"                `toQueryList` _mia1BlockDeviceMappings
        , "DisableApiTermination"             =? _mia1DisableApiTermination
        , "DryRun"                            =? _mia1DryRun
        , "EbsOptimized"                      =? _mia1EbsOptimized
        , "GroupId"                           `toQueryList` _mia1Groups
        , "InstanceId"                        =? _mia1InstanceId
        , "InstanceInitiatedShutdownBehavior" =? _mia1InstanceInitiatedShutdownBehavior
        , "InstanceType"                      =? _mia1InstanceType
        , "Kernel"                            =? _mia1Kernel
        , "Ramdisk"                           =? _mia1Ramdisk
        , "SourceDestCheck"                   =? _mia1SourceDestCheck
        , "SriovNetSupport"                   =? _mia1SriovNetSupport
        , "UserData"                          =? _mia1UserData
        , "Value"                             =? _mia1Value
        ]

instance ToHeaders ModifyInstanceAttribute

instance AWSRequest ModifyInstanceAttribute where
    type Sv ModifyInstanceAttribute = EC2
    type Rs ModifyInstanceAttribute = ModifyInstanceAttributeResponse

    request  = post "ModifyInstanceAttribute"
    response = nullResponse ModifyInstanceAttributeResponse
