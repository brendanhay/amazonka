{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
-- specify only one attribute at a time. Valid attribute values are:
-- instanceType | kernel | ramdisk | userData | disableApiTermination |
-- instanceInitiatedShutdownBehavior | rootDeviceName | blockDeviceMapping |
-- productCodes | sourceDestCheck | groupSet | ebsOptimized | sriovNetSupport.
module Network.AWS.EC2.DescribeInstanceAttribute
    (
    -- * Request
      DescribeInstanceAttribute
    -- ** Request constructor
    , describeInstanceAttribute
    -- ** Request lenses
    , dia1Attribute
    , dia1DryRun
    , dia1InstanceId

    -- * Response
    , InstanceAttribute
    -- ** Response constructor
    , describeInstanceAttributeResponse
    -- ** Response lenses
    , iaBlockDeviceMappings
    , iaDisableApiTermination
    , iaEbsOptimized
    , iaGroups
    , iaInstanceId
    , iaInstanceInitiatedShutdownBehavior
    , iaInstanceType
    , iaKernelId
    , iaProductCodes
    , iaRamdiskId
    , iaRootDeviceName
    , iaSourceDestCheck
    , iaSriovNetSupport
    , iaUserData
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data DescribeInstanceAttribute = DescribeInstanceAttribute
    { _dia1Attribute  :: Text
    , _dia1DryRun     :: Maybe Bool
    , _dia1InstanceId :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeInstanceAttribute' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dia1Attribute' @::@ 'Text'
--
-- * 'dia1DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dia1InstanceId' @::@ 'Text'
--
describeInstanceAttribute :: Text -- ^ 'dia1InstanceId'
                          -> Text -- ^ 'dia1Attribute'
                          -> DescribeInstanceAttribute
describeInstanceAttribute p1 p2 = DescribeInstanceAttribute
    { _dia1InstanceId = p1
    , _dia1Attribute  = p2
    , _dia1DryRun     = Nothing
    }

-- | The instance attribute.
dia1Attribute :: Lens' DescribeInstanceAttribute Text
dia1Attribute = lens _dia1Attribute (\s a -> s { _dia1Attribute = a })

dia1DryRun :: Lens' DescribeInstanceAttribute (Maybe Bool)
dia1DryRun = lens _dia1DryRun (\s a -> s { _dia1DryRun = a })

-- | The ID of the instance.
dia1InstanceId :: Lens' DescribeInstanceAttribute Text
dia1InstanceId = lens _dia1InstanceId (\s a -> s { _dia1InstanceId = a })

instance ToQuery DescribeInstanceAttribute

instance ToPath DescribeInstanceAttribute where
    toPath = const "/"

data InstanceAttribute = InstanceAttribute
    { _iaBlockDeviceMappings               :: [InstanceBlockDeviceMapping]
    , _iaDisableApiTermination             :: Maybe AttributeBooleanValue
    , _iaEbsOptimized                      :: Maybe AttributeBooleanValue
    , _iaGroups                            :: [GroupIdentifier]
    , _iaInstanceId                        :: Maybe Text
    , _iaInstanceInitiatedShutdownBehavior :: Maybe AttributeValue
    , _iaInstanceType                      :: Maybe AttributeValue
    , _iaKernelId                          :: Maybe AttributeValue
    , _iaProductCodes                      :: [ProductCode]
    , _iaRamdiskId                         :: Maybe AttributeValue
    , _iaRootDeviceName                    :: Maybe AttributeValue
    , _iaSourceDestCheck                   :: Maybe AttributeBooleanValue
    , _iaSriovNetSupport                   :: Maybe AttributeValue
    , _iaUserData                          :: Maybe AttributeValue
    } deriving (Eq, Show, Generic)

-- | 'InstanceAttribute' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iaBlockDeviceMappings' @::@ ['InstanceBlockDeviceMapping']
--
-- * 'iaDisableApiTermination' @::@ 'Maybe' 'AttributeBooleanValue'
--
-- * 'iaEbsOptimized' @::@ 'Maybe' 'AttributeBooleanValue'
--
-- * 'iaGroups' @::@ ['GroupIdentifier']
--
-- * 'iaInstanceId' @::@ 'Maybe' 'Text'
--
-- * 'iaInstanceInitiatedShutdownBehavior' @::@ 'Maybe' 'AttributeValue'
--
-- * 'iaInstanceType' @::@ 'Maybe' 'AttributeValue'
--
-- * 'iaKernelId' @::@ 'Maybe' 'AttributeValue'
--
-- * 'iaProductCodes' @::@ ['ProductCode']
--
-- * 'iaRamdiskId' @::@ 'Maybe' 'AttributeValue'
--
-- * 'iaRootDeviceName' @::@ 'Maybe' 'AttributeValue'
--
-- * 'iaSourceDestCheck' @::@ 'Maybe' 'AttributeBooleanValue'
--
-- * 'iaSriovNetSupport' @::@ 'Maybe' 'AttributeValue'
--
-- * 'iaUserData' @::@ 'Maybe' 'AttributeValue'
--
describeInstanceAttributeResponse :: InstanceAttribute
describeInstanceAttributeResponse = InstanceAttribute
    { _iaInstanceId                        = Nothing
    , _iaInstanceType                      = Nothing
    , _iaKernelId                          = Nothing
    , _iaRamdiskId                         = Nothing
    , _iaUserData                          = Nothing
    , _iaDisableApiTermination             = Nothing
    , _iaInstanceInitiatedShutdownBehavior = Nothing
    , _iaRootDeviceName                    = Nothing
    , _iaBlockDeviceMappings               = mempty
    , _iaProductCodes                      = mempty
    , _iaEbsOptimized                      = Nothing
    , _iaSriovNetSupport                   = Nothing
    , _iaSourceDestCheck                   = Nothing
    , _iaGroups                            = mempty
    }

-- | The block device mapping of the instance.
iaBlockDeviceMappings :: Lens' InstanceAttribute [InstanceBlockDeviceMapping]
iaBlockDeviceMappings =
    lens _iaBlockDeviceMappings (\s a -> s { _iaBlockDeviceMappings = a })

-- | If the value is true, you can't terminate the instance through the Amazon
-- EC2 console, CLI, or API; otherwise, you can.
iaDisableApiTermination :: Lens' InstanceAttribute (Maybe AttributeBooleanValue)
iaDisableApiTermination =
    lens _iaDisableApiTermination (\s a -> s { _iaDisableApiTermination = a })

-- | Indicates whether the instance is optimized for EBS I/O.
iaEbsOptimized :: Lens' InstanceAttribute (Maybe AttributeBooleanValue)
iaEbsOptimized = lens _iaEbsOptimized (\s a -> s { _iaEbsOptimized = a })

-- | The security groups associated with the instance.
iaGroups :: Lens' InstanceAttribute [GroupIdentifier]
iaGroups = lens _iaGroups (\s a -> s { _iaGroups = a })

-- | The ID of the instance.
iaInstanceId :: Lens' InstanceAttribute (Maybe Text)
iaInstanceId = lens _iaInstanceId (\s a -> s { _iaInstanceId = a })

-- | Indicates whether an instance stops or terminates when you initiate
-- shutdown from the instance (using the operating system command for system
-- shutdown).
iaInstanceInitiatedShutdownBehavior :: Lens' InstanceAttribute (Maybe AttributeValue)
iaInstanceInitiatedShutdownBehavior =
    lens _iaInstanceInitiatedShutdownBehavior
        (\s a -> s { _iaInstanceInitiatedShutdownBehavior = a })

-- | The instance type.
iaInstanceType :: Lens' InstanceAttribute (Maybe AttributeValue)
iaInstanceType = lens _iaInstanceType (\s a -> s { _iaInstanceType = a })

-- | The kernel ID.
iaKernelId :: Lens' InstanceAttribute (Maybe AttributeValue)
iaKernelId = lens _iaKernelId (\s a -> s { _iaKernelId = a })

-- | A list of product codes.
iaProductCodes :: Lens' InstanceAttribute [ProductCode]
iaProductCodes = lens _iaProductCodes (\s a -> s { _iaProductCodes = a })

-- | The RAM disk ID.
iaRamdiskId :: Lens' InstanceAttribute (Maybe AttributeValue)
iaRamdiskId = lens _iaRamdiskId (\s a -> s { _iaRamdiskId = a })

-- | The name of the root device (for example, /dev/sda1).
iaRootDeviceName :: Lens' InstanceAttribute (Maybe AttributeValue)
iaRootDeviceName = lens _iaRootDeviceName (\s a -> s { _iaRootDeviceName = a })

-- | Indicates whether source/destination checking is enabled. A value of true
-- means checking is enabled, and false means checking is disabled. This
-- value must be false for a NAT instance to perform NAT.
iaSourceDestCheck :: Lens' InstanceAttribute (Maybe AttributeBooleanValue)
iaSourceDestCheck =
    lens _iaSourceDestCheck (\s a -> s { _iaSourceDestCheck = a })

iaSriovNetSupport :: Lens' InstanceAttribute (Maybe AttributeValue)
iaSriovNetSupport =
    lens _iaSriovNetSupport (\s a -> s { _iaSriovNetSupport = a })

-- | The Base64-encoded MIME user data.
iaUserData :: Lens' InstanceAttribute (Maybe AttributeValue)
iaUserData = lens _iaUserData (\s a -> s { _iaUserData = a })

instance FromXML InstanceAttribute where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "InstanceAttribute"

instance AWSRequest DescribeInstanceAttribute where
    type Sv DescribeInstanceAttribute = EC2
    type Rs DescribeInstanceAttribute = InstanceAttribute

    request  = post "DescribeInstanceAttribute"
    response = xmlResponse $ \h x -> InstanceAttribute
        <$> x %| "blockDeviceMapping"
        <*> x %| "disableApiTermination"
        <*> x %| "ebsOptimized"
        <*> x %| "groupSet"
        <*> x %| "instanceId"
        <*> x %| "instanceInitiatedShutdownBehavior"
        <*> x %| "instanceType"
        <*> x %| "kernel"
        <*> x %| "productCodes"
        <*> x %| "ramdisk"
        <*> x %| "rootDeviceName"
        <*> x %| "sourceDestCheck"
        <*> x %| "sriovNetSupport"
        <*> x %| "userData"
