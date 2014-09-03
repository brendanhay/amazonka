{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DescribeImages
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes one or more of the images (AMIs, AKIs, and ARIs) available to
-- you. Images available to you include public images, private images that you
-- own, and private images owned by other AWS accounts but for which you have
-- explicit launch permissions. Deregistered images are included in the
-- returned results for an unspecified interval after deregistration. Example
-- 1 This example describes the specified AMI.
-- https://ec2.amazonaws.com/?Action=DescribeImages
-- &amp;ImageId.1=ami-be3adfd7 &amp;AUTHPARAMS
-- 59dbff89-35bd-4eac-99ed-be587EXAMPLE ami-1a2b3c4d amazon/getting-started
-- available 123456789012 true i386 machine aki-1a2b3c4d ari-1a2b3c4d amazon
-- getting-started Image Description ebs /dev/sda /dev/sda1 snap-1a2b3c4d 15
-- false standard paravirtual xen Example 2 This example filters the response
-- to include only public Windows images with an x86_64 architecture.
-- https://ec2.amazonaws.com/?Action=DescribeImages
-- &amp;Filter.1.Name=is-public &amp;Filter.1.Value.1=true
-- &amp;Filter.2.Name=architecture &amp;Filter.2.Value.1=x86_64
-- &amp;Filter.3.Name=platform &amp;Filter.3.Value.1=windows &amp;AUTHPARAMS
-- 59dbff89-35bd-4eac-99ed-be587EXAMPLE ami-1a2b3c4d
-- ec2-public-windows-images/Server2003r2-x86_64-Win-v1.07.manifest.xml
-- available 123456789012 true x86_64 machine windows amazon instance-store
-- hvm xen ... Example 3 This example returns the results to display images
-- where the owner is aws-marketplace.
-- https://ec2.amazonaws.com/?Action=DescribeImages
-- &amp;Owner.0=aws-marketplace &amp;AUTHPARAMS
-- 4a4a27a2-2e7c-475d-b35b-ca822EXAMPLE ami-1a2b3c4d
-- aws-marketplace/example-marketplace-amzn-ami.1 available 123456789012 true
-- a1b2c3d4e5f6g7h8i9j10k11 marketplace i386 machine aki-1a2b3c4d
-- aws-marketplace example-marketplace-amzn-ami.1 Amazon Linux AMI i386 EBS
-- ebs /dev/sda1 /dev/sda1 snap-1a2b3c4d 8 true paravirtual xen ...
module Network.AWS.EC2.V2014_06_15.DescribeImages
    (
    -- * Request
      DescribeImages
    -- ** Request constructor
    , describeImages
    -- ** Request lenses
    , disExecutableUsers
    , disFilters
    , disImageIds
    , disOwners

    -- * Response
    , DescribeImagesResponse
    -- ** Response lenses
    , ditImages
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeImages' request.
describeImages :: DescribeImages
describeImages = DescribeImages
    { _disExecutableUsers = mempty
    , _disFilters = mempty
    , _disImageIds = mempty
    , _disOwners = mempty
    }

data DescribeImages = DescribeImages
    { _disExecutableUsers :: [Text]
      -- ^ Scopes the images by users with explicit launch permissions.
      -- Specify an AWS account ID, self (the sender of the request), or
      -- all (public AMIs).
    , _disFilters :: [Filter]
      -- ^ One or more filters. architecture - The image architecture (i386
      -- | x86_64). block-device-mapping.delete-on-termination - A Boolean
      -- value that indicates whether the Amazon EBS volume is deleted on
      -- instance termination. block-device-mapping.device-name - The
      -- device name for the Amazon EBS volume (for example, /dev/sdh).
      -- block-device-mapping.snapshot-id - The ID of the snapshot used
      -- for the Amazon EBS volume. block-device-mapping.volume-size - The
      -- volume size of the Amazon EBS volume, in GiB.
      -- block-device-mapping.volume-type - The volume type of the Amazon
      -- EBS volume (gp2 | standard | io1). description - The description
      -- of the image (provided during image creation). hypervisor - The
      -- hypervisor type (ovm | xen). image-id - The ID of the image.
      -- image-type - The image type (machine | kernel | ramdisk).
      -- is-public - A Boolean that indicates whether the image is public.
      -- kernel-id - The kernel ID. manifest-location - The location of
      -- the image manifest. name - The name of the AMI (provided during
      -- image creation). owner-alias - The AWS account alias (for
      -- example, amazon). owner-id - The AWS account ID of the image
      -- owner. platform - The platform. To only list Windows-based AMIs,
      -- use windows. product-code - The product code. product-code.type -
      -- The type of the product code (devpay | marketplace). ramdisk-id -
      -- The RAM disk ID. root-device-name - The name of the root device
      -- volume (for example, /dev/sda1). root-device-type - The type of
      -- the root device volume (ebs | instance-store). state - The state
      -- of the image (available | pending | failed). state-reason-code -
      -- The reason code for the state change. state-reason-message - The
      -- message for the state change. tag:key=value - The key/value
      -- combination of a tag assigned to the resource. tag-key - The key
      -- of a tag assigned to the resource. This filter is independent of
      -- the tag-value filter. For example, if you use both the filter
      -- "tag-key=Purpose" and the filter "tag-value=X", you get any
      -- resources assigned both the tag key Purpose (regardless of what
      -- the tag's value is), and the tag value X (regardless of what the
      -- tag's key is). If you want to list only resources where Purpose
      -- is X, see the tag:key=value filter. tag-value - The value of a
      -- tag assigned to the resource. This filter is independent of the
      -- tag-key filter. virtualization-type - The virtualization type
      -- (paravirtual | hvm).
    , _disImageIds :: [Text]
      -- ^ One or more image IDs. Default: Describes all images available to
      -- you.
    , _disOwners :: [Text]
      -- ^ Filters the images by the owner. Specify an AWS account ID,
      -- amazon (owner is Amazon), aws-marketplace (owner is AWS
      -- Marketplace), self (owner is the sender of the request), or all
      -- (all owners).
    } deriving (Show, Generic)

-- | Scopes the images by users with explicit launch permissions. Specify an AWS
-- account ID, self (the sender of the request), or all (public AMIs).
disExecutableUsers
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> DescribeImages
    -> f DescribeImages
disExecutableUsers f x =
    (\y -> x { _disExecutableUsers = y })
       <$> f (_disExecutableUsers x)
{-# INLINE disExecutableUsers #-}

-- | One or more filters. architecture - The image architecture (i386 | x86_64).
-- block-device-mapping.delete-on-termination - A Boolean value that indicates
-- whether the Amazon EBS volume is deleted on instance termination.
-- block-device-mapping.device-name - The device name for the Amazon EBS
-- volume (for example, /dev/sdh). block-device-mapping.snapshot-id - The ID
-- of the snapshot used for the Amazon EBS volume.
-- block-device-mapping.volume-size - The volume size of the Amazon EBS
-- volume, in GiB. block-device-mapping.volume-type - The volume type of the
-- Amazon EBS volume (gp2 | standard | io1). description - The description of
-- the image (provided during image creation). hypervisor - The hypervisor
-- type (ovm | xen). image-id - The ID of the image. image-type - The image
-- type (machine | kernel | ramdisk). is-public - A Boolean that indicates
-- whether the image is public. kernel-id - The kernel ID. manifest-location -
-- The location of the image manifest. name - The name of the AMI (provided
-- during image creation). owner-alias - The AWS account alias (for example,
-- amazon). owner-id - The AWS account ID of the image owner. platform - The
-- platform. To only list Windows-based AMIs, use windows. product-code - The
-- product code. product-code.type - The type of the product code (devpay |
-- marketplace). ramdisk-id - The RAM disk ID. root-device-name - The name of
-- the root device volume (for example, /dev/sda1). root-device-type - The
-- type of the root device volume (ebs | instance-store). state - The state of
-- the image (available | pending | failed). state-reason-code - The reason
-- code for the state change. state-reason-message - The message for the state
-- change. tag:key=value - The key/value combination of a tag assigned to the
-- resource. tag-key - The key of a tag assigned to the resource. This filter
-- is independent of the tag-value filter. For example, if you use both the
-- filter "tag-key=Purpose" and the filter "tag-value=X", you get any
-- resources assigned both the tag key Purpose (regardless of what the tag's
-- value is), and the tag value X (regardless of what the tag's key is). If
-- you want to list only resources where Purpose is X, see the tag:key=value
-- filter. tag-value - The value of a tag assigned to the resource. This
-- filter is independent of the tag-key filter. virtualization-type - The
-- virtualization type (paravirtual | hvm).
disFilters
    :: Functor f
    => ([Filter]
    -> f ([Filter]))
    -> DescribeImages
    -> f DescribeImages
disFilters f x =
    (\y -> x { _disFilters = y })
       <$> f (_disFilters x)
{-# INLINE disFilters #-}

-- | One or more image IDs. Default: Describes all images available to you.
disImageIds
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> DescribeImages
    -> f DescribeImages
disImageIds f x =
    (\y -> x { _disImageIds = y })
       <$> f (_disImageIds x)
{-# INLINE disImageIds #-}

-- | Filters the images by the owner. Specify an AWS account ID, amazon (owner
-- is Amazon), aws-marketplace (owner is AWS Marketplace), self (owner is the
-- sender of the request), or all (all owners).
disOwners
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> DescribeImages
    -> f DescribeImages
disOwners f x =
    (\y -> x { _disOwners = y })
       <$> f (_disOwners x)
{-# INLINE disOwners #-}

instance ToQuery DescribeImages where
    toQuery = genericQuery def

data DescribeImagesResponse = DescribeImagesResponse
    { _ditImages :: [Image]
      -- ^ Information about one or more images.
    } deriving (Show, Generic)

-- | Information about one or more images.
ditImages
    :: Functor f
    => ([Image]
    -> f ([Image]))
    -> DescribeImagesResponse
    -> f DescribeImagesResponse
ditImages f x =
    (\y -> x { _ditImages = y })
       <$> f (_ditImages x)
{-# INLINE ditImages #-}

instance FromXML DescribeImagesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeImages where
    type Sv DescribeImages = EC2
    type Rs DescribeImages = DescribeImagesResponse

    request = post "DescribeImages"
    response _ = xmlResponse
