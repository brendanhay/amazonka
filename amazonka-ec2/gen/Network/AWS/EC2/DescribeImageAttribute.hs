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
    , diarItem
    , diarItem
    , diarKernel
    , diarRamdisk
    , diarDescription
    , diarSriovNetSupport
    , diarItem
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
    , _diarItem :: [LaunchPermission]
    , _diarItem :: [ProductCode]
    , _diarKernel :: Maybe AttributeValue
    , _diarRamdisk :: Maybe AttributeValue
    , _diarDescription :: Maybe AttributeValue
    , _diarSriovNetSupport :: Maybe AttributeValue
    , _diarItem :: [BlockDeviceMapping]
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
-- * @Item ::@ @[LaunchPermission]@
--
-- * @Item ::@ @[ProductCode]@
--
-- * @Kernel ::@ @Maybe AttributeValue@
--
-- * @Ramdisk ::@ @Maybe AttributeValue@
--
-- * @Description ::@ @Maybe AttributeValue@
--
-- * @SriovNetSupport ::@ @Maybe AttributeValue@
--
-- * @Item ::@ @[BlockDeviceMapping]@
--
describeImageAttributeResponse :: DescribeImageAttributeResponse
describeImageAttributeResponse = DescribeImageAttributeResponse
    { _diarImageId = Nothing
    , _diarItem = mempty
    , _diarItem = mempty
    , _diarKernel = Nothing
    , _diarRamdisk = Nothing
    , _diarDescription = Nothing
    , _diarSriovNetSupport = Nothing
    , _diarItem = mempty
    }

-- | The ID of the AMI.
diarImageId :: Lens' DescribeImageAttributeResponse (Maybe Text)
diarImageId = lens _diarImageId (\s a -> s { _diarImageId = a })

-- | One or more launch permissions.
diarItem :: Lens' DescribeImageAttributeResponse [LaunchPermission]
diarItem = lens _diarItem (\s a -> s { _diarItem = a })

-- | One or more product codes.
diarItem :: Lens' DescribeImageAttributeResponse [ProductCode]
diarItem = lens _diarItem (\s a -> s { _diarItem = a })

-- | The kernel ID.
diarKernel :: Lens' DescribeImageAttributeResponse (Maybe AttributeValue)
diarKernel = lens _diarKernel (\s a -> s { _diarKernel = a })

-- | The RAM disk ID.
diarRamdisk :: Lens' DescribeImageAttributeResponse (Maybe AttributeValue)
diarRamdisk = lens _diarRamdisk (\s a -> s { _diarRamdisk = a })

-- | A description for the AMI.
diarDescription :: Lens' DescribeImageAttributeResponse (Maybe AttributeValue)
diarDescription = lens _diarDescription (\s a -> s { _diarDescription = a })

-- | 
diarSriovNetSupport :: Lens' DescribeImageAttributeResponse (Maybe AttributeValue)
diarSriovNetSupport =
    lens _diarSriovNetSupport (\s a -> s { _diarSriovNetSupport = a })

-- | One or more block device mapping entries.
diarItem :: Lens' DescribeImageAttributeResponse [BlockDeviceMapping]
diarItem = lens _diarItem (\s a -> s { _diarItem = a })

instance FromXML DescribeImageAttributeResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeImageAttribute where
    type Sv DescribeImageAttribute = EC2
    type Rs DescribeImageAttribute = DescribeImageAttributeResponse

    request = post "DescribeImageAttribute"
    response _ = xmlResponse
