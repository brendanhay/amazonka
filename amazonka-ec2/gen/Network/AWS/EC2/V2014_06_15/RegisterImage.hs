{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.RegisterImage
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Registers an AMI. When you're creating an AMI, this is the final step you
-- must complete before you can launch an instance from the AMI. For more
-- information about creating AMIs, see Creating Your Own AMIs in the Amazon
-- Elastic Compute Cloud User Guide. For Amazon EBS-backed instances,
-- CreateImage creates and registers the AMI in a single request, so you don't
-- have to register the AMI yourself. You can also use RegisterImage to create
-- an Amazon EBS-backed AMI from a snapshot of a root device volume. For more
-- information, see Launching an Instance from a Snapshot in the Amazon
-- Elastic Compute Cloud User Guide. If needed, you can deregister an AMI at
-- any time. Any modifications you make to an AMI backed by an instance store
-- volume invalidates its registration. If you make changes to an image,
-- deregister the previous image and register the new image. You can't
-- register an image where a secondary (non-root) snapshot has AWS Marketplace
-- product codes. Example 1 This example registers the AMI specified in the
-- my-new-image.manifest.xml manifest file, located in the bucket called
-- myawsbucket. https://ec2.amazonaws.com/?Action=RegisterImage
-- &amp;ImageLocation=myawsbucket/my-new-image.manifest.xml &amp;AUTHPARAMS
-- &lt;RegisterImageResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-02-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;imageId&gt;ami-1a2b3c4d&lt;/imageId&gt; &lt;/RegisterImageResponse&gt;
-- Example 2 This example registers an Amazon EBS snapshot to create an AMI
-- backed by Amazon EBS. https://ec2.amazonaws.com/?Action=RegisterImage
-- &amp;RootDeviceName=/dev/sda1
-- &amp;BlockDeviceMapping.1.DeviceName=/dev/sda1
-- &amp;BlockDeviceMapping.1.Ebs.SnapshotId=snap-1a2b3c4d &amp;Name=MyImage
-- &amp;AUTHPARAMS &lt;RegisterImageResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-02-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;imageId&gt;ami-1a2b3c4d&lt;/imageId&gt; &lt;/RegisterImageResponse&gt;
-- Example 3 This example registers the AMI with an Amazon EBS snapshot as the
-- root device, a separate snapshot as a secondary device, and an empty 100
-- GiB Amazon EBS volume as a storage device.
-- https://ec2.amazonaws.com/?Action=RegisterImage
-- &amp;RootDeviceName=/dev/sda1
-- &amp;BlockDeviceMapping.1.DeviceName=/dev/sda1
-- &amp;BlockDeviceMapping.1.Ebs.SnapshotId=snap-1a2b3c4d
-- &amp;BlockDeviceMapping.2.DeviceName=/dev/sdb
-- &amp;BlockDeviceMapping.2.Ebs.SnapshotId=snap-2a2b3c4d
-- &amp;BlockDeviceMapping.3.DeviceName=/dev/sdc
-- &amp;BlockDeviceMapping.3.Ebs.VolumeSize=100 &amp;Name=MyImage
-- &amp;AUTHPARAMS &lt;RegisterImageResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-02-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;imageId&gt;ami-1a2b3c4d&lt;/imageId&gt; &lt;/RegisterImageResponse&gt;.
module Network.AWS.EC2.V2014_06_15.RegisterImage
    (
    -- * Request
      RegisterImage
    -- ** Request constructor
    , mkRegisterImage
    -- ** Request lenses
    , ri2ImageLocation
    , ri2Name
    , ri2Description
    , ri2Architecture
    , ri2KernelId
    , ri2RamdiskId
    , ri2RootDeviceName
    , ri2BlockDeviceMappings
    , ri2VirtualizationType
    , ri2SriovNetSupport

    -- * Response
    , RegisterImageResponse
    -- ** Response lenses
    , rirImageId
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | 
data RegisterImage = RegisterImage
    { _ri2ImageLocation :: Maybe Text
    , _ri2Name :: Text
    , _ri2Description :: Maybe Text
    , _ri2Architecture :: Maybe ArchitectureValues
    , _ri2KernelId :: Maybe Text
    , _ri2RamdiskId :: Maybe Text
    , _ri2RootDeviceName :: Maybe Text
    , _ri2BlockDeviceMappings :: [BlockDeviceMapping]
    , _ri2VirtualizationType :: Maybe VirtualizationType
    , _ri2SriovNetSupport :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RegisterImage' request.
mkRegisterImage :: Text -- ^ 'ri2Name'
                -> RegisterImage
mkRegisterImage p2 = RegisterImage
    { _ri2ImageLocation = Nothing
    , _ri2Name = p2
    , _ri2Description = Nothing
    , _ri2Architecture = Nothing
    , _ri2KernelId = Nothing
    , _ri2RamdiskId = Nothing
    , _ri2RootDeviceName = Nothing
    , _ri2BlockDeviceMappings = mempty
    , _ri2VirtualizationType = Nothing
    , _ri2SriovNetSupport = Nothing
    }

-- | The full path to your AMI manifest in Amazon S3 storage.
ri2ImageLocation :: Lens' RegisterImage (Maybe Text)
ri2ImageLocation =
    lens _ri2ImageLocation (\s a -> s { _ri2ImageLocation = a })

-- | A name for your AMI. Constraints: 3-128 alphanumeric characters,
-- parenthesis (()), commas (,), slashes (/), dashes (-), or underscores (_).
ri2Name :: Lens' RegisterImage Text
ri2Name = lens _ri2Name (\s a -> s { _ri2Name = a })

-- | A description for your AMI.
ri2Description :: Lens' RegisterImage (Maybe Text)
ri2Description = lens _ri2Description (\s a -> s { _ri2Description = a })

-- | The architecture of the AMI. Default: For Amazon EBS-backed AMIs, i386. For
-- instance store-backed AMIs, the architecture specified in the manifest
-- file.
ri2Architecture :: Lens' RegisterImage (Maybe ArchitectureValues)
ri2Architecture = lens _ri2Architecture (\s a -> s { _ri2Architecture = a })

-- | The ID of the kernel.
ri2KernelId :: Lens' RegisterImage (Maybe Text)
ri2KernelId = lens _ri2KernelId (\s a -> s { _ri2KernelId = a })

-- | The ID of the RAM disk.
ri2RamdiskId :: Lens' RegisterImage (Maybe Text)
ri2RamdiskId = lens _ri2RamdiskId (\s a -> s { _ri2RamdiskId = a })

-- | The name of the root device (for example, /dev/sda1, or xvda).
ri2RootDeviceName :: Lens' RegisterImage (Maybe Text)
ri2RootDeviceName =
    lens _ri2RootDeviceName (\s a -> s { _ri2RootDeviceName = a })

-- | One or more block device mapping entries.
ri2BlockDeviceMappings :: Lens' RegisterImage [BlockDeviceMapping]
ri2BlockDeviceMappings =
    lens _ri2BlockDeviceMappings (\s a -> s { _ri2BlockDeviceMappings = a })

-- | The type of virtualization.
ri2VirtualizationType :: Lens' RegisterImage (Maybe VirtualizationType)
ri2VirtualizationType =
    lens _ri2VirtualizationType (\s a -> s { _ri2VirtualizationType = a })

-- | Set to simple to enable enhanced networking for the AMI and any instances
-- that you launch from the AMI. There is no way to disable enhanced
-- networking at this time. This option is supported only for HVM AMIs.
-- Specifying this option with a PV AMI can make instances launched from the
-- AMI unreachable.
ri2SriovNetSupport :: Lens' RegisterImage (Maybe Text)
ri2SriovNetSupport =
    lens _ri2SriovNetSupport (\s a -> s { _ri2SriovNetSupport = a })

instance ToQuery RegisterImage where
    toQuery = genericQuery def

-- | 
newtype RegisterImageResponse = RegisterImageResponse
    { _rirImageId :: Maybe Text
    } deriving (Show, Generic)

-- | The ID of the newly registered AMI.
rirImageId :: Lens' RegisterImageResponse (Maybe Text)
rirImageId = lens _rirImageId (\s a -> s { _rirImageId = a })

instance FromXML RegisterImageResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest RegisterImage where
    type Sv RegisterImage = EC2
    type Rs RegisterImage = RegisterImageResponse

    request = post "RegisterImage"
    response _ = xmlResponse
