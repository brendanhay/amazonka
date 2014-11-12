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
-- only one attribute at a time.
module Network.AWS.EC2.DescribeImageAttribute
    (
    -- * Request
      DescribeImageAttribute
    -- ** Request constructor
    , describeImageAttribute
    -- ** Request lenses
    , diaAttribute
    , diaDryRun
    , diaImageId

    -- * Response
    , ImageAttribute
    -- ** Response constructor
    , describeImageAttributeResponse
    -- ** Response lenses
    , ia1BlockDeviceMappings
    , ia1Description
    , ia1ImageId
    , ia1KernelId
    , ia1LaunchPermissions
    , ia1ProductCodes
    , ia1RamdiskId
    , ia1SriovNetSupport
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data DescribeImageAttribute = DescribeImageAttribute
    { _diaAttribute :: Text
    , _diaDryRun    :: Maybe Bool
    , _diaImageId   :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeImageAttribute' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'diaAttribute' @::@ 'Text'
--
-- * 'diaDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'diaImageId' @::@ 'Text'
--
describeImageAttribute :: Text -- ^ 'diaImageId'
                       -> Text -- ^ 'diaAttribute'
                       -> DescribeImageAttribute
describeImageAttribute p1 p2 = DescribeImageAttribute
    { _diaImageId   = p1
    , _diaAttribute = p2
    , _diaDryRun    = Nothing
    }

-- | The AMI attribute.
diaAttribute :: Lens' DescribeImageAttribute Text
diaAttribute = lens _diaAttribute (\s a -> s { _diaAttribute = a })

diaDryRun :: Lens' DescribeImageAttribute (Maybe Bool)
diaDryRun = lens _diaDryRun (\s a -> s { _diaDryRun = a })

-- | The ID of the AMI.
diaImageId :: Lens' DescribeImageAttribute Text
diaImageId = lens _diaImageId (\s a -> s { _diaImageId = a })

instance ToQuery DescribeImageAttribute

instance ToPath DescribeImageAttribute where
    toPath = const "/"

data ImageAttribute = ImageAttribute
    { _ia1BlockDeviceMappings :: [BlockDeviceMapping]
    , _ia1Description         :: Maybe AttributeValue
    , _ia1ImageId             :: Maybe Text
    , _ia1KernelId            :: Maybe AttributeValue
    , _ia1LaunchPermissions   :: [LaunchPermission]
    , _ia1ProductCodes        :: [ProductCode]
    , _ia1RamdiskId           :: Maybe AttributeValue
    , _ia1SriovNetSupport     :: Maybe AttributeValue
    } deriving (Eq, Show, Generic)

-- | 'ImageAttribute' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ia1BlockDeviceMappings' @::@ ['BlockDeviceMapping']
--
-- * 'ia1Description' @::@ 'Maybe' 'AttributeValue'
--
-- * 'ia1ImageId' @::@ 'Maybe' 'Text'
--
-- * 'ia1KernelId' @::@ 'Maybe' 'AttributeValue'
--
-- * 'ia1LaunchPermissions' @::@ ['LaunchPermission']
--
-- * 'ia1ProductCodes' @::@ ['ProductCode']
--
-- * 'ia1RamdiskId' @::@ 'Maybe' 'AttributeValue'
--
-- * 'ia1SriovNetSupport' @::@ 'Maybe' 'AttributeValue'
--
describeImageAttributeResponse :: ImageAttribute
describeImageAttributeResponse = ImageAttribute
    { _ia1ImageId             = Nothing
    , _ia1LaunchPermissions   = mempty
    , _ia1ProductCodes        = mempty
    , _ia1KernelId            = Nothing
    , _ia1RamdiskId           = Nothing
    , _ia1Description         = Nothing
    , _ia1SriovNetSupport     = Nothing
    , _ia1BlockDeviceMappings = mempty
    }

-- | One or more block device mapping entries.
ia1BlockDeviceMappings :: Lens' ImageAttribute [BlockDeviceMapping]
ia1BlockDeviceMappings =
    lens _ia1BlockDeviceMappings (\s a -> s { _ia1BlockDeviceMappings = a })

-- | A description for the AMI.
ia1Description :: Lens' ImageAttribute (Maybe AttributeValue)
ia1Description = lens _ia1Description (\s a -> s { _ia1Description = a })

-- | The ID of the AMI.
ia1ImageId :: Lens' ImageAttribute (Maybe Text)
ia1ImageId = lens _ia1ImageId (\s a -> s { _ia1ImageId = a })

-- | The kernel ID.
ia1KernelId :: Lens' ImageAttribute (Maybe AttributeValue)
ia1KernelId = lens _ia1KernelId (\s a -> s { _ia1KernelId = a })

-- | One or more launch permissions.
ia1LaunchPermissions :: Lens' ImageAttribute [LaunchPermission]
ia1LaunchPermissions =
    lens _ia1LaunchPermissions (\s a -> s { _ia1LaunchPermissions = a })

-- | One or more product codes.
ia1ProductCodes :: Lens' ImageAttribute [ProductCode]
ia1ProductCodes = lens _ia1ProductCodes (\s a -> s { _ia1ProductCodes = a })

-- | The RAM disk ID.
ia1RamdiskId :: Lens' ImageAttribute (Maybe AttributeValue)
ia1RamdiskId = lens _ia1RamdiskId (\s a -> s { _ia1RamdiskId = a })

ia1SriovNetSupport :: Lens' ImageAttribute (Maybe AttributeValue)
ia1SriovNetSupport =
    lens _ia1SriovNetSupport (\s a -> s { _ia1SriovNetSupport = a })

instance FromXML ImageAttribute where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ImageAttribute"

instance AWSRequest DescribeImageAttribute where
    type Sv DescribeImageAttribute = EC2
    type Rs DescribeImageAttribute = ImageAttribute

    request  = post "DescribeImageAttribute"
    response = xmlResponse $ \h x -> ImageAttribute
        <$> x %| "blockDeviceMapping"
        <*> x %| "description"
        <*> x %| "imageId"
        <*> x %| "kernel"
        <*> x %| "launchPermission"
        <*> x %| "productCodes"
        <*> x %| "ramdisk"
        <*> x %| "sriovNetSupport"
