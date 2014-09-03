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
    , registerImage
    -- ** Request lenses
    , rivName
    , rivArchitecture
    , rivBlockDeviceMappings
    , rivImageLocation
    , rivDescription
    , rivKernelId
    , rivRamdiskId
    , rivRootDeviceName
    , rivVirtualizationType
    , rivSriovNetSupport

    -- * Response
    , RegisterImageResponse
    -- ** Response lenses
    , riwImageId
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'RegisterImage' request.
registerImage :: Text -- ^ 'rivName'
              -> RegisterImage
registerImage p1 = RegisterImage
    { _rivName = p1
    , _rivArchitecture = Nothing
    , _rivBlockDeviceMappings = mempty
    , _rivImageLocation = Nothing
    , _rivDescription = Nothing
    , _rivKernelId = Nothing
    , _rivRamdiskId = Nothing
    , _rivRootDeviceName = Nothing
    , _rivVirtualizationType = Nothing
    , _rivSriovNetSupport = Nothing
    }

data RegisterImage = RegisterImage
    { _rivName :: Text
      -- ^ A name for your AMI. Constraints: 3-128 alphanumeric characters,
      -- parenthesis (()), commas (,), slashes (/), dashes (-), or
      -- underscores (_).
    , _rivArchitecture :: Maybe ArchitectureValues
      -- ^ The architecture of the AMI. Default: For Amazon EBS-backed AMIs,
      -- i386. For instance store-backed AMIs, the architecture specified
      -- in the manifest file.
    , _rivBlockDeviceMappings :: [BlockDeviceMapping]
      -- ^ One or more block device mapping entries.
    , _rivImageLocation :: Maybe Text
      -- ^ The full path to your AMI manifest in Amazon S3 storage.
    , _rivDescription :: Maybe Text
      -- ^ A description for your AMI.
    , _rivKernelId :: Maybe Text
      -- ^ The ID of the kernel.
    , _rivRamdiskId :: Maybe Text
      -- ^ The ID of the RAM disk.
    , _rivRootDeviceName :: Maybe Text
      -- ^ The name of the root device (for example, /dev/sda1, or xvda).
    , _rivVirtualizationType :: Maybe VirtualizationType
      -- ^ The type of virtualization.
    , _rivSriovNetSupport :: Maybe Text
      -- ^ Set to simple to enable enhanced networking for the AMI and any
      -- instances that you launch from the AMI. There is no way to
      -- disable enhanced networking at this time. This option is
      -- supported only for HVM AMIs. Specifying this option with a PV AMI
      -- can make instances launched from the AMI unreachable.
    } deriving (Show, Generic)

-- | A name for your AMI. Constraints: 3-128 alphanumeric characters,
-- parenthesis (()), commas (,), slashes (/), dashes (-), or underscores (_).
rivName
    :: Functor f
    => (Text
    -> f (Text))
    -> RegisterImage
    -> f RegisterImage
rivName f x =
    (\y -> x { _rivName = y })
       <$> f (_rivName x)
{-# INLINE rivName #-}

-- | The architecture of the AMI. Default: For Amazon EBS-backed AMIs, i386. For
-- instance store-backed AMIs, the architecture specified in the manifest
-- file.
rivArchitecture
    :: Functor f
    => (Maybe ArchitectureValues
    -> f (Maybe ArchitectureValues))
    -> RegisterImage
    -> f RegisterImage
rivArchitecture f x =
    (\y -> x { _rivArchitecture = y })
       <$> f (_rivArchitecture x)
{-# INLINE rivArchitecture #-}

-- | One or more block device mapping entries.
rivBlockDeviceMappings
    :: Functor f
    => ([BlockDeviceMapping]
    -> f ([BlockDeviceMapping]))
    -> RegisterImage
    -> f RegisterImage
rivBlockDeviceMappings f x =
    (\y -> x { _rivBlockDeviceMappings = y })
       <$> f (_rivBlockDeviceMappings x)
{-# INLINE rivBlockDeviceMappings #-}

-- | The full path to your AMI manifest in Amazon S3 storage.
rivImageLocation
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> RegisterImage
    -> f RegisterImage
rivImageLocation f x =
    (\y -> x { _rivImageLocation = y })
       <$> f (_rivImageLocation x)
{-# INLINE rivImageLocation #-}

-- | A description for your AMI.
rivDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> RegisterImage
    -> f RegisterImage
rivDescription f x =
    (\y -> x { _rivDescription = y })
       <$> f (_rivDescription x)
{-# INLINE rivDescription #-}

-- | The ID of the kernel.
rivKernelId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> RegisterImage
    -> f RegisterImage
rivKernelId f x =
    (\y -> x { _rivKernelId = y })
       <$> f (_rivKernelId x)
{-# INLINE rivKernelId #-}

-- | The ID of the RAM disk.
rivRamdiskId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> RegisterImage
    -> f RegisterImage
rivRamdiskId f x =
    (\y -> x { _rivRamdiskId = y })
       <$> f (_rivRamdiskId x)
{-# INLINE rivRamdiskId #-}

-- | The name of the root device (for example, /dev/sda1, or xvda).
rivRootDeviceName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> RegisterImage
    -> f RegisterImage
rivRootDeviceName f x =
    (\y -> x { _rivRootDeviceName = y })
       <$> f (_rivRootDeviceName x)
{-# INLINE rivRootDeviceName #-}

-- | The type of virtualization.
rivVirtualizationType
    :: Functor f
    => (Maybe VirtualizationType
    -> f (Maybe VirtualizationType))
    -> RegisterImage
    -> f RegisterImage
rivVirtualizationType f x =
    (\y -> x { _rivVirtualizationType = y })
       <$> f (_rivVirtualizationType x)
{-# INLINE rivVirtualizationType #-}

-- | Set to simple to enable enhanced networking for the AMI and any instances
-- that you launch from the AMI. There is no way to disable enhanced
-- networking at this time. This option is supported only for HVM AMIs.
-- Specifying this option with a PV AMI can make instances launched from the
-- AMI unreachable.
rivSriovNetSupport
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> RegisterImage
    -> f RegisterImage
rivSriovNetSupport f x =
    (\y -> x { _rivSriovNetSupport = y })
       <$> f (_rivSriovNetSupport x)
{-# INLINE rivSriovNetSupport #-}

instance ToQuery RegisterImage where
    toQuery = genericQuery def

data RegisterImageResponse = RegisterImageResponse
    { _riwImageId :: Maybe Text
      -- ^ The ID of the newly registered AMI.
    } deriving (Show, Generic)

-- | The ID of the newly registered AMI.
riwImageId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> RegisterImageResponse
    -> f RegisterImageResponse
riwImageId f x =
    (\y -> x { _riwImageId = y })
       <$> f (_riwImageId x)
{-# INLINE riwImageId #-}

instance FromXML RegisterImageResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest RegisterImage where
    type Sv RegisterImage = EC2
    type Rs RegisterImage = RegisterImageResponse

    request = post "RegisterImage"
    response _ = xmlResponse
