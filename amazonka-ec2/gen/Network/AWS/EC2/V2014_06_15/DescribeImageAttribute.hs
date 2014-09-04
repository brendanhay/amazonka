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
    , mkDescribeImageAttributeRequest
    -- ** Request lenses
    , diarImageId
    , diarAttribute

    -- * Response
    , DescribeImageAttributeResponse
    -- ** Response lenses
    , iaImageId
    , iaLaunchPermissions
    , iaProductCodes
    , iaKernelId
    , iaRamdiskId
    , iaDescription
    , iaSriovNetSupport
    , iaBlockDeviceMappings
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeImageAttribute' request.
mkDescribeImageAttributeRequest :: Text -- ^ 'diarImageId'
                                -> ImageAttributeName -- ^ 'diarAttribute'
                                -> DescribeImageAttribute
mkDescribeImageAttributeRequest p1 p2 = DescribeImageAttribute
    { _diarImageId = p1
    , _diarAttribute = p2
    }
{-# INLINE mkDescribeImageAttributeRequest #-}

data DescribeImageAttribute = DescribeImageAttribute
    { _diarImageId :: Text
      -- ^ The ID of the AMI.
    , _diarAttribute :: ImageAttributeName
      -- ^ The AMI attribute.
    } deriving (Show, Generic)

-- | The ID of the AMI.
diarImageId :: Lens' DescribeImageAttribute (Text)
diarImageId = lens _diarImageId (\s a -> s { _diarImageId = a })
{-# INLINE diarImageId #-}

-- | The AMI attribute.
diarAttribute :: Lens' DescribeImageAttribute (ImageAttributeName)
diarAttribute = lens _diarAttribute (\s a -> s { _diarAttribute = a })
{-# INLINE diarAttribute #-}

instance ToQuery DescribeImageAttribute where
    toQuery = genericQuery def

data DescribeImageAttributeResponse = DescribeImageAttributeResponse
    { _iaImageId :: Maybe Text
      -- ^ The ID of the AMI.
    , _iaLaunchPermissions :: [LaunchPermission]
      -- ^ One or more launch permissions.
    , _iaProductCodes :: [ProductCode]
      -- ^ One or more product codes.
    , _iaKernelId :: Maybe AttributeValue
      -- ^ The kernel ID.
    , _iaRamdiskId :: Maybe AttributeValue
      -- ^ The RAM disk ID.
    , _iaDescription :: Maybe AttributeValue
      -- ^ A description for the AMI.
    , _iaSriovNetSupport :: Maybe AttributeValue
      -- ^ 
    , _iaBlockDeviceMappings :: [BlockDeviceMapping]
      -- ^ One or more block device mapping entries.
    } deriving (Show, Generic)

-- | The ID of the AMI.
iaImageId :: Lens' DescribeImageAttributeResponse (Maybe Text)
iaImageId = lens _iaImageId (\s a -> s { _iaImageId = a })
{-# INLINE iaImageId #-}

-- | One or more launch permissions.
iaLaunchPermissions :: Lens' DescribeImageAttributeResponse ([LaunchPermission])
iaLaunchPermissions = lens _iaLaunchPermissions (\s a -> s { _iaLaunchPermissions = a })
{-# INLINE iaLaunchPermissions #-}

-- | One or more product codes.
iaProductCodes :: Lens' DescribeImageAttributeResponse ([ProductCode])
iaProductCodes = lens _iaProductCodes (\s a -> s { _iaProductCodes = a })
{-# INLINE iaProductCodes #-}

-- | The kernel ID.
iaKernelId :: Lens' DescribeImageAttributeResponse (Maybe AttributeValue)
iaKernelId = lens _iaKernelId (\s a -> s { _iaKernelId = a })
{-# INLINE iaKernelId #-}

-- | The RAM disk ID.
iaRamdiskId :: Lens' DescribeImageAttributeResponse (Maybe AttributeValue)
iaRamdiskId = lens _iaRamdiskId (\s a -> s { _iaRamdiskId = a })
{-# INLINE iaRamdiskId #-}

-- | A description for the AMI.
iaDescription :: Lens' DescribeImageAttributeResponse (Maybe AttributeValue)
iaDescription = lens _iaDescription (\s a -> s { _iaDescription = a })
{-# INLINE iaDescription #-}

-- | 
iaSriovNetSupport :: Lens' DescribeImageAttributeResponse (Maybe AttributeValue)
iaSriovNetSupport = lens _iaSriovNetSupport (\s a -> s { _iaSriovNetSupport = a })
{-# INLINE iaSriovNetSupport #-}

-- | One or more block device mapping entries.
iaBlockDeviceMappings :: Lens' DescribeImageAttributeResponse ([BlockDeviceMapping])
iaBlockDeviceMappings = lens _iaBlockDeviceMappings (\s a -> s { _iaBlockDeviceMappings = a })
{-# INLINE iaBlockDeviceMappings #-}

instance FromXML DescribeImageAttributeResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeImageAttribute where
    type Sv DescribeImageAttribute = EC2
    type Rs DescribeImageAttribute = DescribeImageAttributeResponse

    request = post "DescribeImageAttribute"
    response _ = xmlResponse
