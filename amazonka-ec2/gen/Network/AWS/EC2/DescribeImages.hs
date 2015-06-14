{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.EC2.DescribeImages
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Describes one or more of the images (AMIs, AKIs, and ARIs) available to
-- you. Images available to you include public images, private images that
-- you own, and private images owned by other AWS accounts but for which
-- you have explicit launch permissions.
--
-- Deregistered images are included in the returned results for an
-- unspecified interval after deregistration.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeImages.html>
module Network.AWS.EC2.DescribeImages
    (
    -- * Request
      DescribeImages
    -- ** Request constructor
    , describeImages
    -- ** Request lenses
    , describe1Owners
    , describe1ExecutableUsers
    , describe1Filters
    , describe1ImageIds
    , describe1DryRun

    -- * Response
    , DescribeImagesResponse
    -- ** Response constructor
    , describeImagesResponse
    -- ** Response lenses
    , dirImages
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.EC2.Types

-- | /See:/ 'describeImages' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'describe1Owners'
--
-- * 'describe1ExecutableUsers'
--
-- * 'describe1Filters'
--
-- * 'describe1ImageIds'
--
-- * 'describe1DryRun'
data DescribeImages = DescribeImages'{_describe1Owners :: Maybe [Text], _describe1ExecutableUsers :: Maybe [Text], _describe1Filters :: Maybe [Filter], _describe1ImageIds :: Maybe [Text], _describe1DryRun :: Maybe Bool} deriving (Eq, Read, Show)

-- | 'DescribeImages' smart constructor.
describeImages :: DescribeImages
describeImages = DescribeImages'{_describe1Owners = Nothing, _describe1ExecutableUsers = Nothing, _describe1Filters = Nothing, _describe1ImageIds = Nothing, _describe1DryRun = Nothing};

-- | Filters the images by the owner. Specify an AWS account ID, @amazon@
-- (owner is Amazon), @aws-marketplace@ (owner is AWS Marketplace), @self@
-- (owner is the sender of the request). Omitting this option returns all
-- images for which you have launch permissions, regardless of ownership.
describe1Owners :: Lens' DescribeImages (Maybe [Text])
describe1Owners = lens _describe1Owners (\ s a -> s{_describe1Owners = a});

-- | Scopes the images by users with explicit launch permissions. Specify an
-- AWS account ID, @self@ (the sender of the request), or @all@ (public
-- AMIs).
describe1ExecutableUsers :: Lens' DescribeImages (Maybe [Text])
describe1ExecutableUsers = lens _describe1ExecutableUsers (\ s a -> s{_describe1ExecutableUsers = a});

-- | One or more filters.
--
-- -   @architecture@ - The image architecture (@i386@ | @x86_64@).
--
-- -   @block-device-mapping.delete-on-termination@ - A Boolean value that
--     indicates whether the Amazon EBS volume is deleted on instance
--     termination.
--
-- -   @block-device-mapping.device-name@ - The device name for the EBS
--     volume (for example, @\/dev\/sdh@).
--
-- -   @block-device-mapping.snapshot-id@ - The ID of the snapshot used for
--     the EBS volume.
--
-- -   @block-device-mapping.volume-size@ - The volume size of the EBS
--     volume, in GiB.
--
-- -   @block-device-mapping.volume-type@ - The volume type of the EBS
--     volume (@gp2@ | @standard@ | @io1@).
--
-- -   @description@ - The description of the image (provided during image
--     creation).
--
-- -   @hypervisor@ - The hypervisor type (@ovm@ | @xen@).
--
-- -   @image-id@ - The ID of the image.
--
-- -   @image-type@ - The image type (@machine@ | @kernel@ | @ramdisk@).
--
-- -   @is-public@ - A Boolean that indicates whether the image is public.
--
-- -   @kernel-id@ - The kernel ID.
--
-- -   @manifest-location@ - The location of the image manifest.
--
-- -   @name@ - The name of the AMI (provided during image creation).
--
-- -   @owner-alias@ - The AWS account alias (for example, @amazon@).
--
-- -   @owner-id@ - The AWS account ID of the image owner.
--
-- -   @platform@ - The platform. To only list Windows-based AMIs, use
--     @windows@.
--
-- -   @product-code@ - The product code.
--
-- -   @product-code.type@ - The type of the product code (@devpay@ |
--     @marketplace@).
--
-- -   @ramdisk-id@ - The RAM disk ID.
--
-- -   @root-device-name@ - The name of the root device volume (for
--     example, @\/dev\/sda1@).
--
-- -   @root-device-type@ - The type of the root device volume (@ebs@ |
--     @instance-store@).
--
-- -   @state@ - The state of the image (@available@ | @pending@ |
--     @failed@).
--
-- -   @state-reason-code@ - The reason code for the state change.
--
-- -   @state-reason-message@ - The message for the state change.
--
-- -   @tag@:/key/=/value/ - The key\/value combination of a tag assigned
--     to the resource.
--
-- -   @tag-key@ - The key of a tag assigned to the resource. This filter
--     is independent of the tag-value filter. For example, if you use both
--     the filter \"tag-key=Purpose\" and the filter \"tag-value=X\", you
--     get any resources assigned both the tag key Purpose (regardless of
--     what the tag\'s value is), and the tag value X (regardless of what
--     the tag\'s key is). If you want to list only resources where Purpose
--     is X, see the @tag@:/key/=/value/ filter.
--
-- -   @tag-value@ - The value of a tag assigned to the resource. This
--     filter is independent of the @tag-key@ filter.
--
-- -   @virtualization-type@ - The virtualization type (@paravirtual@ |
--     @hvm@).
--
describe1Filters :: Lens' DescribeImages (Maybe [Filter])
describe1Filters = lens _describe1Filters (\ s a -> s{_describe1Filters = a});

-- | One or more image IDs.
--
-- Default: Describes all images available to you.
describe1ImageIds :: Lens' DescribeImages (Maybe [Text])
describe1ImageIds = lens _describe1ImageIds (\ s a -> s{_describe1ImageIds = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describe1DryRun :: Lens' DescribeImages (Maybe Bool)
describe1DryRun = lens _describe1DryRun (\ s a -> s{_describe1DryRun = a});

instance AWSRequest DescribeImages where
        type Sv DescribeImages = EC2
        type Rs DescribeImages = DescribeImagesResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DescribeImagesResponse' <$> parseXMLList "item" x)

instance ToHeaders DescribeImages where
        toHeaders = const mempty

instance ToPath DescribeImages where
        toPath = const "/"

instance ToQuery DescribeImages where
        toQuery DescribeImages'{..}
          = mconcat
              ["Action" =: ("DescribeImages" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "Owner" =: _describe1Owners,
               "ExecutableBy" =: _describe1ExecutableUsers,
               "Filter" =: _describe1Filters,
               "ImageId" =: _describe1ImageIds,
               "DryRun" =: _describe1DryRun]

-- | /See:/ 'describeImagesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dirImages'
newtype DescribeImagesResponse = DescribeImagesResponse'{_dirImages :: Maybe [Image]} deriving (Eq, Read, Show)

-- | 'DescribeImagesResponse' smart constructor.
describeImagesResponse :: DescribeImagesResponse
describeImagesResponse = DescribeImagesResponse'{_dirImages = Nothing};

-- | Information about one or more images.
dirImages :: Lens' DescribeImagesResponse (Maybe [Image])
dirImages = lens _dirImages (\ s a -> s{_dirImages = a});
