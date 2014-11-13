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

-- Module      : Network.AWS.EC2.RegisterImage
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
-- Elastic Compute Cloud User Guide. You can also use RegisterImage to create
-- an Amazon EBS-backed AMI from a snapshot of a root device volume. For more
-- information, see Launching an Instance from a Snapshot in the Amazon
-- Elastic Compute Cloud User Guide. If needed, you can deregister an AMI at
-- any time. Any modifications you make to an AMI backed by an instance store
-- volume invalidates its registration. If you make changes to an image,
-- deregister the previous image and register the new image.
module Network.AWS.EC2.RegisterImage
    (
    -- * Request
      RegisterImage
    -- ** Request constructor
    , registerImage
    -- ** Request lenses
    , ri1Architecture
    , ri1BlockDeviceMappings
    , ri1Description
    , ri1DryRun
    , ri1ImageLocation
    , ri1KernelId
    , ri1Name
    , ri1RamdiskId
    , ri1RootDeviceName
    , ri1SriovNetSupport
    , ri1VirtualizationType

    -- * Response
    , RegisterImageResponse
    -- ** Response constructor
    , registerImageResponse
    -- ** Response lenses
    , rirImageId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data RegisterImage = RegisterImage
    { _ri1Architecture        :: Maybe Text
    , _ri1BlockDeviceMappings :: [BlockDeviceMapping]
    , _ri1Description         :: Maybe Text
    , _ri1DryRun              :: Maybe Bool
    , _ri1ImageLocation       :: Maybe Text
    , _ri1KernelId            :: Maybe Text
    , _ri1Name                :: Text
    , _ri1RamdiskId           :: Maybe Text
    , _ri1RootDeviceName      :: Maybe Text
    , _ri1SriovNetSupport     :: Maybe Text
    , _ri1VirtualizationType  :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'RegisterImage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ri1Architecture' @::@ 'Maybe' 'Text'
--
-- * 'ri1BlockDeviceMappings' @::@ ['BlockDeviceMapping']
--
-- * 'ri1Description' @::@ 'Maybe' 'Text'
--
-- * 'ri1DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'ri1ImageLocation' @::@ 'Maybe' 'Text'
--
-- * 'ri1KernelId' @::@ 'Maybe' 'Text'
--
-- * 'ri1Name' @::@ 'Text'
--
-- * 'ri1RamdiskId' @::@ 'Maybe' 'Text'
--
-- * 'ri1RootDeviceName' @::@ 'Maybe' 'Text'
--
-- * 'ri1SriovNetSupport' @::@ 'Maybe' 'Text'
--
-- * 'ri1VirtualizationType' @::@ 'Maybe' 'Text'
--
registerImage :: Text -- ^ 'ri1Name'
              -> RegisterImage
registerImage p1 = RegisterImage
    { _ri1Name                = p1
    , _ri1DryRun              = Nothing
    , _ri1ImageLocation       = Nothing
    , _ri1Description         = Nothing
    , _ri1Architecture        = Nothing
    , _ri1KernelId            = Nothing
    , _ri1RamdiskId           = Nothing
    , _ri1RootDeviceName      = Nothing
    , _ri1BlockDeviceMappings = mempty
    , _ri1VirtualizationType  = Nothing
    , _ri1SriovNetSupport     = Nothing
    }

-- | The architecture of the AMI. Default: For Amazon EBS-backed AMIs, i386.
-- For instance store-backed AMIs, the architecture specified in the
-- manifest file.
ri1Architecture :: Lens' RegisterImage (Maybe Text)
ri1Architecture = lens _ri1Architecture (\s a -> s { _ri1Architecture = a })

-- | One or more block device mapping entries.
ri1BlockDeviceMappings :: Lens' RegisterImage [BlockDeviceMapping]
ri1BlockDeviceMappings =
    lens _ri1BlockDeviceMappings (\s a -> s { _ri1BlockDeviceMappings = a })

-- | A description for your AMI.
ri1Description :: Lens' RegisterImage (Maybe Text)
ri1Description = lens _ri1Description (\s a -> s { _ri1Description = a })

ri1DryRun :: Lens' RegisterImage (Maybe Bool)
ri1DryRun = lens _ri1DryRun (\s a -> s { _ri1DryRun = a })

-- | The full path to your AMI manifest in Amazon S3 storage.
ri1ImageLocation :: Lens' RegisterImage (Maybe Text)
ri1ImageLocation = lens _ri1ImageLocation (\s a -> s { _ri1ImageLocation = a })

-- | The ID of the kernel.
ri1KernelId :: Lens' RegisterImage (Maybe Text)
ri1KernelId = lens _ri1KernelId (\s a -> s { _ri1KernelId = a })

-- | A name for your AMI. Constraints: 3-128 alphanumeric characters,
-- parentheses (()), square brackets ([]), spaces ( ), periods (.), slashes
-- (/), dashes (-), single quotes ('), at-signs (@), or underscores(_).
ri1Name :: Lens' RegisterImage Text
ri1Name = lens _ri1Name (\s a -> s { _ri1Name = a })

-- | The ID of the RAM disk.
ri1RamdiskId :: Lens' RegisterImage (Maybe Text)
ri1RamdiskId = lens _ri1RamdiskId (\s a -> s { _ri1RamdiskId = a })

-- | The name of the root device (for example, /dev/sda1, or xvda).
ri1RootDeviceName :: Lens' RegisterImage (Maybe Text)
ri1RootDeviceName =
    lens _ri1RootDeviceName (\s a -> s { _ri1RootDeviceName = a })

-- | Set to simple to enable enhanced networking for the AMI and any instances
-- that you launch from the AMI. There is no way to disable enhanced
-- networking at this time. This option is supported only for HVM AMIs.
-- Specifying this option with a PV AMI can make instances launched from the
-- AMI unreachable.
ri1SriovNetSupport :: Lens' RegisterImage (Maybe Text)
ri1SriovNetSupport =
    lens _ri1SriovNetSupport (\s a -> s { _ri1SriovNetSupport = a })

-- | The type of virtualization. Default: paravirtual.
ri1VirtualizationType :: Lens' RegisterImage (Maybe Text)
ri1VirtualizationType =
    lens _ri1VirtualizationType (\s a -> s { _ri1VirtualizationType = a })

instance ToQuery RegisterImage

instance ToPath RegisterImage where
    toPath = const "/"

newtype RegisterImageResponse = RegisterImageResponse
    { _rirImageId :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'RegisterImageResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rirImageId' @::@ 'Maybe' 'Text'
--
registerImageResponse :: RegisterImageResponse
registerImageResponse = RegisterImageResponse
    { _rirImageId = Nothing
    }

-- | The ID of the newly registered AMI.
rirImageId :: Lens' RegisterImageResponse (Maybe Text)
rirImageId = lens _rirImageId (\s a -> s { _rirImageId = a })

instance AWSRequest RegisterImage where
    type Sv RegisterImage = EC2
    type Rs RegisterImage = RegisterImageResponse

    request  = post "RegisterImage"
    response = xmlResponse $ \h x -> RegisterImageResponse
        <$> x %| "imageId"
