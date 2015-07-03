{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.DescribeImages
-- Copyright   : (c) 2013-2015 Brendan Hay
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
    , di2Owners
    , di2ExecutableUsers
    , di2Filters
    , di2ImageIds
    , di2DryRun

    -- * Response
    , DescribeImagesResponse
    -- ** Response constructor
    , describeImagesResponse
    -- ** Response lenses
    , descImages
    , descStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeImages' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'di2Owners'
--
-- * 'di2ExecutableUsers'
--
-- * 'di2Filters'
--
-- * 'di2ImageIds'
--
-- * 'di2DryRun'
data DescribeImages = DescribeImages'
    { _di2Owners          :: !(Maybe [Text])
    , _di2ExecutableUsers :: !(Maybe [Text])
    , _di2Filters         :: !(Maybe [Filter])
    , _di2ImageIds        :: !(Maybe [Text])
    , _di2DryRun          :: !(Maybe Bool)
    } deriving (Eq,Read,Show)

-- | 'DescribeImages' smart constructor.
describeImages :: DescribeImages
describeImages =
    DescribeImages'
    { _di2Owners = Nothing
    , _di2ExecutableUsers = Nothing
    , _di2Filters = Nothing
    , _di2ImageIds = Nothing
    , _di2DryRun = Nothing
    }

-- | Filters the images by the owner. Specify an AWS account ID, @amazon@
-- (owner is Amazon), @aws-marketplace@ (owner is AWS Marketplace), @self@
-- (owner is the sender of the request). Omitting this option returns all
-- images for which you have launch permissions, regardless of ownership.
di2Owners :: Lens' DescribeImages [Text]
di2Owners = lens _di2Owners (\ s a -> s{_di2Owners = a}) . _Default;

-- | Scopes the images by users with explicit launch permissions. Specify an
-- AWS account ID, @self@ (the sender of the request), or @all@ (public
-- AMIs).
di2ExecutableUsers :: Lens' DescribeImages [Text]
di2ExecutableUsers = lens _di2ExecutableUsers (\ s a -> s{_di2ExecutableUsers = a}) . _Default;

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
di2Filters :: Lens' DescribeImages [Filter]
di2Filters = lens _di2Filters (\ s a -> s{_di2Filters = a}) . _Default;

-- | One or more image IDs.
--
-- Default: Describes all images available to you.
di2ImageIds :: Lens' DescribeImages [Text]
di2ImageIds = lens _di2ImageIds (\ s a -> s{_di2ImageIds = a}) . _Default;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
di2DryRun :: Lens' DescribeImages (Maybe Bool)
di2DryRun = lens _di2DryRun (\ s a -> s{_di2DryRun = a});

instance AWSRequest DescribeImages where
        type Sv DescribeImages = EC2
        type Rs DescribeImages = DescribeImagesResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DescribeImagesResponse' <$>
                   (x .@? "imagesSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeImages where
        toHeaders = const mempty

instance ToPath DescribeImages where
        toPath = const "/"

instance ToQuery DescribeImages where
        toQuery DescribeImages'{..}
          = mconcat
              ["Action" =: ("DescribeImages" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               toQuery (toQueryList "Owner" <$> _di2Owners),
               toQuery
                 (toQueryList "ExecutableBy" <$> _di2ExecutableUsers),
               toQuery (toQueryList "Filter" <$> _di2Filters),
               toQuery (toQueryList "ImageId" <$> _di2ImageIds),
               "DryRun" =: _di2DryRun]

-- | /See:/ 'describeImagesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'descImages'
--
-- * 'descStatus'
data DescribeImagesResponse = DescribeImagesResponse'
    { _descImages :: !(Maybe [Image])
    , _descStatus :: !Int
    } deriving (Eq,Read,Show)

-- | 'DescribeImagesResponse' smart constructor.
describeImagesResponse :: Int -> DescribeImagesResponse
describeImagesResponse pStatus =
    DescribeImagesResponse'
    { _descImages = Nothing
    , _descStatus = pStatus
    }

-- | Information about one or more images.
descImages :: Lens' DescribeImagesResponse [Image]
descImages = lens _descImages (\ s a -> s{_descImages = a}) . _Default;

-- | FIXME: Undocumented member.
descStatus :: Lens' DescribeImagesResponse Int
descStatus = lens _descStatus (\ s a -> s{_descStatus = a});
