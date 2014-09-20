{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeImageAttribute
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
module Network.AWS.EC2.DescribeImageAttribute
    (
    -- * Request
      DescribeImageAttribute
    -- ** Request constructor
    , describeImageAttribute
    -- ** Request lenses
    , diaImageId
    , diaAttribute

    -- * Response
    , DescribeImageAttributeResponse
    -- ** Response constructor
    , describeImageAttributeResponse
    -- ** Response lenses
    , diarImageId
    , diarLaunchPermissions
    , diarProductCodes
    , diarKernelId
    , diarRamdiskId
    , diarDescription
    , diarSriovNetSupport
    , diarBlockDeviceMappings
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

data DescribeImageAttribute = DescribeImageAttribute
    { _diaImageId :: Text
    , _diaAttribute :: ImageAttributeName
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeImageAttribute' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ImageId ::@ @Text@
--
-- * @Attribute ::@ @ImageAttributeName@
--
describeImageAttribute :: Text -- ^ 'diaImageId'
                       -> ImageAttributeName -- ^ 'diaAttribute'
                       -> DescribeImageAttribute
describeImageAttribute p1 p2 = DescribeImageAttribute
    { _diaImageId = p1
    , _diaAttribute = p2
    }

-- | The ID of the AMI.
diaImageId :: Lens' DescribeImageAttribute Text
diaImageId = lens _diaImageId (\s a -> s { _diaImageId = a })

-- | The AMI attribute.
diaAttribute :: Lens' DescribeImageAttribute ImageAttributeName
diaAttribute = lens _diaAttribute (\s a -> s { _diaAttribute = a })

instance ToQuery DescribeImageAttribute where
    toQuery = genericQuery def

-- | Information about the image attribute.
data DescribeImageAttributeResponse = DescribeImageAttributeResponse
    { _diarImageId :: Maybe Text
    , _diarLaunchPermissions :: [LaunchPermission]
    , _diarProductCodes :: [ProductCode]
    , _diarKernelId :: Maybe AttributeValue
    , _diarRamdiskId :: Maybe AttributeValue
    , _diarDescription :: Maybe AttributeValue
    , _diarSriovNetSupport :: Maybe AttributeValue
    , _diarBlockDeviceMappings :: [BlockDeviceMapping]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeImageAttributeResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ImageId ::@ @Maybe Text@
--
-- * @LaunchPermissions ::@ @[LaunchPermission]@
--
-- * @ProductCodes ::@ @[ProductCode]@
--
-- * @KernelId ::@ @Maybe AttributeValue@
--
-- * @RamdiskId ::@ @Maybe AttributeValue@
--
-- * @Description ::@ @Maybe AttributeValue@
--
-- * @SriovNetSupport ::@ @Maybe AttributeValue@
--
-- * @BlockDeviceMappings ::@ @[BlockDeviceMapping]@
--
describeImageAttributeResponse :: DescribeImageAttributeResponse
describeImageAttributeResponse = DescribeImageAttributeResponse
    { _diarImageId = Nothing
    , _diarLaunchPermissions = mempty
    , _diarProductCodes = mempty
    , _diarKernelId = Nothing
    , _diarRamdiskId = Nothing
    , _diarDescription = Nothing
    , _diarSriovNetSupport = Nothing
    , _diarBlockDeviceMappings = mempty
    }

-- | The ID of the AMI.
diarImageId :: Lens' DescribeImageAttributeResponse (Maybe Text)
diarImageId = lens _diarImageId (\s a -> s { _diarImageId = a })

-- | One or more launch permissions.
diarLaunchPermissions :: Lens' DescribeImageAttributeResponse [LaunchPermission]
diarLaunchPermissions =
    lens _diarLaunchPermissions (\s a -> s { _diarLaunchPermissions = a })

-- | One or more product codes.
diarProductCodes :: Lens' DescribeImageAttributeResponse [ProductCode]
diarProductCodes =
    lens _diarProductCodes (\s a -> s { _diarProductCodes = a })

-- | The kernel ID.
diarKernelId :: Lens' DescribeImageAttributeResponse (Maybe AttributeValue)
diarKernelId = lens _diarKernelId (\s a -> s { _diarKernelId = a })

-- | The RAM disk ID.
diarRamdiskId :: Lens' DescribeImageAttributeResponse (Maybe AttributeValue)
diarRamdiskId = lens _diarRamdiskId (\s a -> s { _diarRamdiskId = a })

-- | A description for the AMI.
diarDescription :: Lens' DescribeImageAttributeResponse (Maybe AttributeValue)
diarDescription = lens _diarDescription (\s a -> s { _diarDescription = a })

-- | 
diarSriovNetSupport :: Lens' DescribeImageAttributeResponse (Maybe AttributeValue)
diarSriovNetSupport =
    lens _diarSriovNetSupport (\s a -> s { _diarSriovNetSupport = a })

-- | One or more block device mapping entries.
diarBlockDeviceMappings :: Lens' DescribeImageAttributeResponse [BlockDeviceMapping]
diarBlockDeviceMappings =
    lens _diarBlockDeviceMappings
         (\s a -> s { _diarBlockDeviceMappings = a })

instance FromXML DescribeImageAttributeResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeImageAttribute where
    type Sv DescribeImageAttribute = EC2
    type Rs DescribeImageAttribute = DescribeImageAttributeResponse

    request = post "DescribeImageAttribute"
    response _ = xmlResponse
