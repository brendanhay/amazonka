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

-- Module      : Network.AWS.EC2.DescribeImages
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

-- | Describes one or more of the images (AMIs, AKIs, and ARIs) available to you.
-- Images available to you include public images, private images that you own,
-- and private images owned by other AWS accounts but for which you have
-- explicit launch permissions.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeImages.html>
module Network.AWS.EC2.DescribeImages
    (
    -- * Request
      DescribeImages
    -- ** Request constructor
    , describeImages
    -- ** Request lenses
    , di2DryRun
    , di2ExecutableUsers
    , di2Filters
    , di2ImageIds
    , di2Owners

    -- * Response
    , DescribeImagesResponse
    -- ** Response constructor
    , describeImagesResponse
    -- ** Response lenses
    , dirImages
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DescribeImages = DescribeImages
    { _di2DryRun          :: Maybe Bool
    , _di2ExecutableUsers :: List "ExecutableBy" Text
    , _di2Filters         :: List "Filter" Filter
    , _di2ImageIds        :: List "ImageId" Text
    , _di2Owners          :: List "Owner" Text
    } deriving (Eq, Show)

-- | 'DescribeImages' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'di2DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'di2ExecutableUsers' @::@ ['Text']
--
-- * 'di2Filters' @::@ ['Filter']
--
-- * 'di2ImageIds' @::@ ['Text']
--
-- * 'di2Owners' @::@ ['Text']
--
describeImages :: DescribeImages
describeImages = DescribeImages
    { _di2DryRun          = Nothing
    , _di2ImageIds        = mempty
    , _di2Owners          = mempty
    , _di2ExecutableUsers = mempty
    , _di2Filters         = mempty
    }

di2DryRun :: Lens' DescribeImages (Maybe Bool)
di2DryRun = lens _di2DryRun (\s a -> s { _di2DryRun = a })

-- | Scopes the images by users with explicit launch permissions. Specify an AWS
-- account ID, 'self' (the sender of the request), or 'all' (public AMIs).
di2ExecutableUsers :: Lens' DescribeImages [Text]
di2ExecutableUsers =
    lens _di2ExecutableUsers (\s a -> s { _di2ExecutableUsers = a })
        . _List

-- | One or more filters.
--
-- 'architecture' - The image architecture ('i386' | 'x86_64').
--
-- 'block-device-mapping.delete-on-termination' - A Boolean value that
-- indicates whether the Amazon EBS volume is deleted on instance termination.
--
-- 'block-device-mapping.device-name' - The device name for the Amazon EBS
-- volume (for example, '/dev/sdh').
--
-- 'block-device-mapping.snapshot-id' - The ID of the snapshot used for the
-- Amazon EBS volume.
--
-- 'block-device-mapping.volume-size' - The volume size of the Amazon EBS
-- volume, in GiB.
--
-- 'block-device-mapping.volume-type' - The volume type of the Amazon EBS
-- volume ('gp2' | 'standard' | 'io1').
--
-- 'description' - The description of the image (provided during image
-- creation).
--
-- 'hypervisor' - The hypervisor type ('ovm' | 'xen').
--
-- 'image-id' - The ID of the image.
--
-- 'image-type' - The image type ('machine' | 'kernel' | 'ramdisk').
--
-- 'is-public' - A Boolean that indicates whether the image is public.
--
-- 'kernel-id' - The kernel ID.
--
-- 'manifest-location' - The location of the image manifest.
--
-- 'name' - The name of the AMI (provided during image creation).
--
-- 'owner-alias' - The AWS account alias (for example, 'amazon').
--
-- 'owner-id' - The AWS account ID of the image owner.
--
-- 'platform' - The platform. To only list Windows-based AMIs, use 'windows'.
--
-- 'product-code' - The product code.
--
-- 'product-code.type' - The type of the product code ('devpay' | 'marketplace').
--
-- 'ramdisk-id' - The RAM disk ID.
--
-- 'root-device-name' - The name of the root device volume (for example, '/dev/sda1').
--
-- 'root-device-type' - The type of the root device volume ('ebs' | 'instance-store'
-- ).
--
-- 'state' - The state of the image ('available' | 'pending' | 'failed').
--
-- 'state-reason-code' - The reason code for the state change.
--
-- 'state-reason-message' - The message for the state change.
--
-- 'tag':/key/=/value/ - The key/value combination of a tag assigned to the
-- resource.
--
-- 'tag-key' - The key of a tag assigned to the resource. This filter is
-- independent of the tag-value filter. For example, if you use both the filter
-- "tag-key=Purpose" and the filter "tag-value=X", you get any resources
-- assigned both the tag key Purpose (regardless of what the tag's value is),
-- and the tag value X (regardless of what the tag's key is). If you want to
-- list only resources where Purpose is X, see the 'tag':/key/=/value/ filter.
--
-- 'tag-value' - The value of a tag assigned to the resource. This filter is
-- independent of the 'tag-key' filter.
--
-- 'virtualization-type' - The virtualization type ('paravirtual' | 'hvm').
--
--
di2Filters :: Lens' DescribeImages [Filter]
di2Filters = lens _di2Filters (\s a -> s { _di2Filters = a }) . _List

-- | One or more image IDs.
--
-- Default: Describes all images available to you.
di2ImageIds :: Lens' DescribeImages [Text]
di2ImageIds = lens _di2ImageIds (\s a -> s { _di2ImageIds = a }) . _List

-- | Filters the images by the owner. Specify an AWS account ID, 'amazon' (owner is
-- Amazon), 'aws-marketplace' (owner is AWS Marketplace), 'self' (owner is the
-- sender of the request), or 'all' (all owners).
di2Owners :: Lens' DescribeImages [Text]
di2Owners = lens _di2Owners (\s a -> s { _di2Owners = a }) . _List

newtype DescribeImagesResponse = DescribeImagesResponse
    { _dirImages :: List "item" Image
    } deriving (Eq, Show, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeImagesResponse where
    type Item DescribeImagesResponse = Image

    fromList = DescribeImagesResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _dirImages

-- | 'DescribeImagesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dirImages' @::@ ['Image']
--
describeImagesResponse :: DescribeImagesResponse
describeImagesResponse = DescribeImagesResponse
    { _dirImages = mempty
    }

-- | Information about one or more images.
dirImages :: Lens' DescribeImagesResponse [Image]
dirImages = lens _dirImages (\s a -> s { _dirImages = a }) . _List

instance ToPath DescribeImages where
    toPath = const "/"

instance ToQuery DescribeImages where
    toQuery DescribeImages{..} = mconcat
        [ "dryRun"       =? _di2DryRun
        , "ExecutableBy" =? _di2ExecutableUsers
        , "Filter"       =? _di2Filters
        , "ImageId"      =? _di2ImageIds
        , "Owner"        =? _di2Owners
        ]

instance ToHeaders DescribeImages

instance AWSRequest DescribeImages where
    type Sv DescribeImages = EC2
    type Rs DescribeImages = DescribeImagesResponse

    request  = post "DescribeImages"
    response = xmlResponse

instance FromXML DescribeImagesResponse where
    parseXML x = DescribeImagesResponse
        <$> x .@  "imagesSet"
