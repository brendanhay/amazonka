{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DescribeImageAttribute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes the specified attribute of the specified AMI. You can specify
-- only one attribute at a time. Example 1 This example lists the launch
-- permissions for the specified AMI.
-- https://ec2.amazonaws.com/?Action=DescribeImageAttribute
-- &amp;ImageId=ami-61a54008 &amp;Attribute=launchPermission &amp;AUTHPARAMS
-- 59dbff89-35bd-4eac-99ed-be587EXAMPLE ami-61a54008 all 495219933132 Example
-- 2 This example lists the product codes for the specified AMI.
-- https://ec2.amazonaws.com/?Action=DescribeImageAttribute
-- &amp;ImageId=ami-2bb65342 &amp;Attribute=productCodes &amp;AUTHPARAMS
-- 59dbff89-35bd-4eac-99ed-be587EXAMPLE ami-2bb65342 a1b2c3d4e5f6g7h8i9j10k11
-- marketplace.
module Network.AWS.EC2.V2014_06_15.DescribeImageAttribute
    (
    -- * Request
      DescribeImageAttribute
    -- ** Request constructor
    , describeImageAttribute
    -- ** Request lenses
    , diarAttribute
    , diarImageId

    -- * Response
    , DescribeImageAttributeResponse
    -- ** Response lenses
    , iaKernelId
    , iaRamdiskId
    , iaDescription
    , iaSriovNetSupport
    , iaBlockDeviceMappings
    , iaLaunchPermissions
    , iaProductCodes
    , iaImageId
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeImageAttribute' request.
describeImageAttribute :: ImageAttributeName -- ^ 'diarAttribute'
                       -> Text -- ^ 'diarImageId'
                       -> DescribeImageAttribute
describeImageAttribute p1 p2 = DescribeImageAttribute
    { _diarAttribute = p1
    , _diarImageId = p2
    }
{-# INLINE describeImageAttribute #-}

data DescribeImageAttribute = DescribeImageAttribute
    { _diarAttribute :: ImageAttributeName
      -- ^ The AMI attribute.
    , _diarImageId :: Text
      -- ^ The ID of the AMI.
    } deriving (Show, Generic)

-- | The AMI attribute.
diarAttribute :: Lens' DescribeImageAttribute (ImageAttributeName)
diarAttribute f x =
    f (_diarAttribute x)
        <&> \y -> x { _diarAttribute = y }
{-# INLINE diarAttribute #-}

-- | The ID of the AMI.
diarImageId :: Lens' DescribeImageAttribute (Text)
diarImageId f x =
    f (_diarImageId x)
        <&> \y -> x { _diarImageId = y }
{-# INLINE diarImageId #-}

instance ToQuery DescribeImageAttribute where
    toQuery = genericQuery def

data DescribeImageAttributeResponse = DescribeImageAttributeResponse
    { _iaKernelId :: Maybe AttributeValue
      -- ^ The kernel ID.
    , _iaRamdiskId :: Maybe AttributeValue
      -- ^ The RAM disk ID.
    , _iaDescription :: Maybe AttributeValue
      -- ^ A description for the AMI.
    , _iaSriovNetSupport :: Maybe AttributeValue
      -- ^ 
    , _iaBlockDeviceMappings :: [BlockDeviceMapping]
      -- ^ One or more block device mapping entries.
    , _iaLaunchPermissions :: [LaunchPermission]
      -- ^ One or more launch permissions.
    , _iaProductCodes :: [ProductCode]
      -- ^ One or more product codes.
    , _iaImageId :: Maybe Text
      -- ^ The ID of the AMI.
    } deriving (Show, Generic)

-- | The kernel ID.
iaKernelId :: Lens' DescribeImageAttributeResponse (Maybe AttributeValue)
iaKernelId f x =
    f (_iaKernelId x)
        <&> \y -> x { _iaKernelId = y }
{-# INLINE iaKernelId #-}

-- | The RAM disk ID.
iaRamdiskId :: Lens' DescribeImageAttributeResponse (Maybe AttributeValue)
iaRamdiskId f x =
    f (_iaRamdiskId x)
        <&> \y -> x { _iaRamdiskId = y }
{-# INLINE iaRamdiskId #-}

-- | A description for the AMI.
iaDescription :: Lens' DescribeImageAttributeResponse (Maybe AttributeValue)
iaDescription f x =
    f (_iaDescription x)
        <&> \y -> x { _iaDescription = y }
{-# INLINE iaDescription #-}

-- | 
iaSriovNetSupport :: Lens' DescribeImageAttributeResponse (Maybe AttributeValue)
iaSriovNetSupport f x =
    f (_iaSriovNetSupport x)
        <&> \y -> x { _iaSriovNetSupport = y }
{-# INLINE iaSriovNetSupport #-}

-- | One or more block device mapping entries.
iaBlockDeviceMappings :: Lens' DescribeImageAttributeResponse ([BlockDeviceMapping])
iaBlockDeviceMappings f x =
    f (_iaBlockDeviceMappings x)
        <&> \y -> x { _iaBlockDeviceMappings = y }
{-# INLINE iaBlockDeviceMappings #-}

-- | One or more launch permissions.
iaLaunchPermissions :: Lens' DescribeImageAttributeResponse ([LaunchPermission])
iaLaunchPermissions f x =
    f (_iaLaunchPermissions x)
        <&> \y -> x { _iaLaunchPermissions = y }
{-# INLINE iaLaunchPermissions #-}

-- | One or more product codes.
iaProductCodes :: Lens' DescribeImageAttributeResponse ([ProductCode])
iaProductCodes f x =
    f (_iaProductCodes x)
        <&> \y -> x { _iaProductCodes = y }
{-# INLINE iaProductCodes #-}

-- | The ID of the AMI.
iaImageId :: Lens' DescribeImageAttributeResponse (Maybe Text)
iaImageId f x =
    f (_iaImageId x)
        <&> \y -> x { _iaImageId = y }
{-# INLINE iaImageId #-}

instance FromXML DescribeImageAttributeResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeImageAttribute where
    type Sv DescribeImageAttribute = EC2
    type Rs DescribeImageAttribute = DescribeImageAttributeResponse

    request = post "DescribeImageAttribute"
    response _ = xmlResponse
