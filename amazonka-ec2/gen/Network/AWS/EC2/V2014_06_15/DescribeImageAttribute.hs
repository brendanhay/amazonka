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
    , mkDescribeImageAttribute
    -- ** Request lenses
    , diaImageId
    , diaAttribute

    -- * Response
    , DescribeImageAttributeResponse
    -- ** Response lenses
    , diarsImageId
    , diarsLaunchPermissions
    , diarsProductCodes
    , diarsKernelId
    , diarsRamdiskId
    , diarsDescription
    , diarsSriovNetSupport
    , diarsBlockDeviceMappings
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | 
data DescribeImageAttribute = DescribeImageAttribute
    { _diaImageId :: Text
    , _diaAttribute :: ImageAttributeName
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeImageAttribute' request.
mkDescribeImageAttribute :: Text -- ^ 'diaImageId'
                         -> ImageAttributeName -- ^ 'diaAttribute'
                         -> DescribeImageAttribute
mkDescribeImageAttribute p1 p2 = DescribeImageAttribute
    { _diaImageId = p1
    , _diaAttribute = p2
    }
{-# INLINE mkDescribeImageAttribute #-}

-- | The ID of the AMI.
diaImageId :: Lens' DescribeImageAttribute Text
diaImageId = lens _diaImageId (\s a -> s { _diaImageId = a })
{-# INLINE diaImageId #-}

-- | The AMI attribute.
diaAttribute :: Lens' DescribeImageAttribute ImageAttributeName
diaAttribute = lens _diaAttribute (\s a -> s { _diaAttribute = a })
{-# INLINE diaAttribute #-}

instance ToQuery DescribeImageAttribute where
    toQuery = genericQuery def

-- | Information about the image attribute.
data DescribeImageAttributeResponse = DescribeImageAttributeResponse
    { _diarsImageId :: Maybe Text
    , _diarsLaunchPermissions :: [LaunchPermission]
    , _diarsProductCodes :: [ProductCode]
    , _diarsKernelId :: Maybe AttributeValue
    , _diarsRamdiskId :: Maybe AttributeValue
    , _diarsDescription :: Maybe AttributeValue
    , _diarsSriovNetSupport :: Maybe AttributeValue
    , _diarsBlockDeviceMappings :: [BlockDeviceMapping]
    } deriving (Show, Generic)

-- | The ID of the AMI.
diarsImageId :: Lens' DescribeImageAttributeResponse (Maybe Text)
diarsImageId = lens _diarsImageId (\s a -> s { _diarsImageId = a })
{-# INLINE diarsImageId #-}

-- | One or more launch permissions.
diarsLaunchPermissions :: Lens' DescribeImageAttributeResponse [LaunchPermission]
diarsLaunchPermissions =
    lens _diarsLaunchPermissions (\s a -> s { _diarsLaunchPermissions = a })
{-# INLINE diarsLaunchPermissions #-}

-- | One or more product codes.
diarsProductCodes :: Lens' DescribeImageAttributeResponse [ProductCode]
diarsProductCodes =
    lens _diarsProductCodes (\s a -> s { _diarsProductCodes = a })
{-# INLINE diarsProductCodes #-}

-- | The kernel ID.
diarsKernelId :: Lens' DescribeImageAttributeResponse (Maybe AttributeValue)
diarsKernelId = lens _diarsKernelId (\s a -> s { _diarsKernelId = a })
{-# INLINE diarsKernelId #-}

-- | The RAM disk ID.
diarsRamdiskId :: Lens' DescribeImageAttributeResponse (Maybe AttributeValue)
diarsRamdiskId = lens _diarsRamdiskId (\s a -> s { _diarsRamdiskId = a })
{-# INLINE diarsRamdiskId #-}

-- | A description for the AMI.
diarsDescription :: Lens' DescribeImageAttributeResponse (Maybe AttributeValue)
diarsDescription =
    lens _diarsDescription (\s a -> s { _diarsDescription = a })
{-# INLINE diarsDescription #-}

-- | 
diarsSriovNetSupport :: Lens' DescribeImageAttributeResponse (Maybe AttributeValue)
diarsSriovNetSupport =
    lens _diarsSriovNetSupport (\s a -> s { _diarsSriovNetSupport = a })
{-# INLINE diarsSriovNetSupport #-}

-- | One or more block device mapping entries.
diarsBlockDeviceMappings :: Lens' DescribeImageAttributeResponse [BlockDeviceMapping]
diarsBlockDeviceMappings =
    lens _diarsBlockDeviceMappings
         (\s a -> s { _diarsBlockDeviceMappings = a })
{-# INLINE diarsBlockDeviceMappings #-}

instance FromXML DescribeImageAttributeResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeImageAttribute where
    type Sv DescribeImageAttribute = EC2
    type Rs DescribeImageAttribute = DescribeImageAttributeResponse

    request = post "DescribeImageAttribute"
    response _ = xmlResponse
