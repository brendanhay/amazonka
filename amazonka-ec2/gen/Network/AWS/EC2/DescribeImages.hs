{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeImages
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of the images (AMIs, AKIs, and ARIs) available to
-- you. Images available to you include public images, private images that
-- you own, and private images owned by other AWS accounts but for which
-- you have explicit launch permissions.
--
-- Deregistered images are included in the returned results for an
-- unspecified interval after deregistration.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeImages.html AWS API Reference> for DescribeImages.
module Network.AWS.EC2.DescribeImages
    (
    -- * Creating a Request
      describeImages
    , DescribeImages
    -- * Request Lenses
    , deseOwners
    , deseExecutableUsers
    , deseFilters
    , deseImageIds
    , deseDryRun

    -- * Destructuring the Response
    , describeImagesResponse
    , DescribeImagesResponse
    -- * Response Lenses
    , desrsImages
    , desrsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeImages' smart constructor.
data DescribeImages = DescribeImages'
    { _deseOwners          :: !(Maybe [Text])
    , _deseExecutableUsers :: !(Maybe [Text])
    , _deseFilters         :: !(Maybe [Filter])
    , _deseImageIds        :: !(Maybe [Text])
    , _deseDryRun          :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeImages' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deseOwners'
--
-- * 'deseExecutableUsers'
--
-- * 'deseFilters'
--
-- * 'deseImageIds'
--
-- * 'deseDryRun'
describeImages
    :: DescribeImages
describeImages =
    DescribeImages'
    { _deseOwners = Nothing
    , _deseExecutableUsers = Nothing
    , _deseFilters = Nothing
    , _deseImageIds = Nothing
    , _deseDryRun = Nothing
    }

-- | Filters the images by the owner. Specify an AWS account ID, 'amazon'
-- (owner is Amazon), 'aws-marketplace' (owner is AWS Marketplace), 'self'
-- (owner is the sender of the request). Omitting this option returns all
-- images for which you have launch permissions, regardless of ownership.
deseOwners :: Lens' DescribeImages [Text]
deseOwners = lens _deseOwners (\ s a -> s{_deseOwners = a}) . _Default . _Coerce;

-- | Scopes the images by users with explicit launch permissions. Specify an
-- AWS account ID, 'self' (the sender of the request), or 'all' (public
-- AMIs).
deseExecutableUsers :: Lens' DescribeImages [Text]
deseExecutableUsers = lens _deseExecutableUsers (\ s a -> s{_deseExecutableUsers = a}) . _Default . _Coerce;

-- | One or more filters.
--
-- -   'architecture' - The image architecture ('i386' | 'x86_64').
--
-- -   'block-device-mapping.delete-on-termination' - A Boolean value that
--     indicates whether the Amazon EBS volume is deleted on instance
--     termination.
--
-- -   'block-device-mapping.device-name' - The device name for the EBS
--     volume (for example, '\/dev\/sdh').
--
-- -   'block-device-mapping.snapshot-id' - The ID of the snapshot used for
--     the EBS volume.
--
-- -   'block-device-mapping.volume-size' - The volume size of the EBS
--     volume, in GiB.
--
-- -   'block-device-mapping.volume-type' - The volume type of the EBS
--     volume ('gp2' | 'standard' | 'io1').
--
-- -   'description' - The description of the image (provided during image
--     creation).
--
-- -   'hypervisor' - The hypervisor type ('ovm' | 'xen').
--
-- -   'image-id' - The ID of the image.
--
-- -   'image-type' - The image type ('machine' | 'kernel' | 'ramdisk').
--
-- -   'is-public' - A Boolean that indicates whether the image is public.
--
-- -   'kernel-id' - The kernel ID.
--
-- -   'manifest-location' - The location of the image manifest.
--
-- -   'name' - The name of the AMI (provided during image creation).
--
-- -   'owner-alias' - The AWS account alias (for example, 'amazon').
--
-- -   'owner-id' - The AWS account ID of the image owner.
--
-- -   'platform' - The platform. To only list Windows-based AMIs, use
--     'windows'.
--
-- -   'product-code' - The product code.
--
-- -   'product-code.type' - The type of the product code ('devpay' |
--     'marketplace').
--
-- -   'ramdisk-id' - The RAM disk ID.
--
-- -   'root-device-name' - The name of the root device volume (for
--     example, '\/dev\/sda1').
--
-- -   'root-device-type' - The type of the root device volume ('ebs' |
--     'instance-store').
--
-- -   'state' - The state of the image ('available' | 'pending' |
--     'failed').
--
-- -   'state-reason-code' - The reason code for the state change.
--
-- -   'state-reason-message' - The message for the state change.
--
-- -   'tag':/key/=/value/ - The key\/value combination of a tag assigned
--     to the resource.
--
-- -   'tag-key' - The key of a tag assigned to the resource. This filter
--     is independent of the tag-value filter. For example, if you use both
--     the filter \"tag-key=Purpose\" and the filter \"tag-value=X\", you
--     get any resources assigned both the tag key Purpose (regardless of
--     what the tag\'s value is), and the tag value X (regardless of what
--     the tag\'s key is). If you want to list only resources where Purpose
--     is X, see the 'tag':/key/=/value/ filter.
--
-- -   'tag-value' - The value of a tag assigned to the resource. This
--     filter is independent of the 'tag-key' filter.
--
-- -   'virtualization-type' - The virtualization type ('paravirtual' |
--     'hvm').
--
deseFilters :: Lens' DescribeImages [Filter]
deseFilters = lens _deseFilters (\ s a -> s{_deseFilters = a}) . _Default . _Coerce;

-- | One or more image IDs.
--
-- Default: Describes all images available to you.
deseImageIds :: Lens' DescribeImages [Text]
deseImageIds = lens _deseImageIds (\ s a -> s{_deseImageIds = a}) . _Default . _Coerce;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is 'DryRunOperation'.
-- Otherwise, it is 'UnauthorizedOperation'.
deseDryRun :: Lens' DescribeImages (Maybe Bool)
deseDryRun = lens _deseDryRun (\ s a -> s{_deseDryRun = a});

instance AWSRequest DescribeImages where
        type Rs DescribeImages = DescribeImagesResponse
        request = postQuery eC2
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
               toQuery (toQueryList "Owner" <$> _deseOwners),
               toQuery
                 (toQueryList "ExecutableBy" <$>
                    _deseExecutableUsers),
               toQuery (toQueryList "Filter" <$> _deseFilters),
               toQuery (toQueryList "ImageId" <$> _deseImageIds),
               "DryRun" =: _deseDryRun]

-- | /See:/ 'describeImagesResponse' smart constructor.
data DescribeImagesResponse = DescribeImagesResponse'
    { _desrsImages :: !(Maybe [Image])
    , _desrsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeImagesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desrsImages'
--
-- * 'desrsStatus'
describeImagesResponse
    :: Int -- ^ 'desrsStatus'
    -> DescribeImagesResponse
describeImagesResponse pStatus_ =
    DescribeImagesResponse'
    { _desrsImages = Nothing
    , _desrsStatus = pStatus_
    }

-- | Information about one or more images.
desrsImages :: Lens' DescribeImagesResponse [Image]
desrsImages = lens _desrsImages (\ s a -> s{_desrsImages = a}) . _Default . _Coerce;

-- | The response status code.
desrsStatus :: Lens' DescribeImagesResponse Int
desrsStatus = lens _desrsStatus (\ s a -> s{_desrsStatus = a});
