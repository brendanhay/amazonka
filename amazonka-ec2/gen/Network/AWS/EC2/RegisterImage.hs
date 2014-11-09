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
    , riArchitecture
    , riBlockDeviceMappings
    , riDescription
    , riDryRun
    , riImageLocation
    , riKernelId
    , riName
    , riRamdiskId
    , riRootDeviceName
    , riSriovNetSupport
    , riVirtualizationType

    -- * Response
    , RegisterImageResult
    -- ** Response constructor
    , registerImageResult
    -- ** Response lenses
    , rirImageId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data RegisterImage = RegisterImage
    { _riArchitecture        :: Maybe Text
    , _riBlockDeviceMappings :: [BlockDeviceMapping]
    , _riDescription         :: Maybe Text
    , _riDryRun              :: Maybe Bool
    , _riImageLocation       :: Maybe Text
    , _riKernelId            :: Maybe Text
    , _riName                :: Text
    , _riRamdiskId           :: Maybe Text
    , _riRootDeviceName      :: Maybe Text
    , _riSriovNetSupport     :: Maybe Text
    , _riVirtualizationType  :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'RegisterImage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'riArchitecture' @::@ 'Maybe' 'Text'
--
-- * 'riBlockDeviceMappings' @::@ ['BlockDeviceMapping']
--
-- * 'riDescription' @::@ 'Maybe' 'Text'
--
-- * 'riDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'riImageLocation' @::@ 'Maybe' 'Text'
--
-- * 'riKernelId' @::@ 'Maybe' 'Text'
--
-- * 'riName' @::@ 'Text'
--
-- * 'riRamdiskId' @::@ 'Maybe' 'Text'
--
-- * 'riRootDeviceName' @::@ 'Maybe' 'Text'
--
-- * 'riSriovNetSupport' @::@ 'Maybe' 'Text'
--
-- * 'riVirtualizationType' @::@ 'Maybe' 'Text'
--
registerImage :: Text -- ^ 'riName'
              -> RegisterImage
registerImage p1 = RegisterImage
    { _riName                = p1
    , _riDryRun              = Nothing
    , _riImageLocation       = Nothing
    , _riDescription         = Nothing
    , _riArchitecture        = Nothing
    , _riKernelId            = Nothing
    , _riRamdiskId           = Nothing
    , _riRootDeviceName      = Nothing
    , _riBlockDeviceMappings = mempty
    , _riVirtualizationType  = Nothing
    , _riSriovNetSupport     = Nothing
    }

-- | The architecture of the AMI. Default: For Amazon EBS-backed AMIs, i386.
-- For instance store-backed AMIs, the architecture specified in the
-- manifest file.
riArchitecture :: Lens' RegisterImage (Maybe Text)
riArchitecture = lens _riArchitecture (\s a -> s { _riArchitecture = a })

-- | One or more block device mapping entries.
riBlockDeviceMappings :: Lens' RegisterImage [BlockDeviceMapping]
riBlockDeviceMappings =
    lens _riBlockDeviceMappings (\s a -> s { _riBlockDeviceMappings = a })

-- | A description for your AMI.
riDescription :: Lens' RegisterImage (Maybe Text)
riDescription = lens _riDescription (\s a -> s { _riDescription = a })

riDryRun :: Lens' RegisterImage (Maybe Bool)
riDryRun = lens _riDryRun (\s a -> s { _riDryRun = a })

-- | The full path to your AMI manifest in Amazon S3 storage.
riImageLocation :: Lens' RegisterImage (Maybe Text)
riImageLocation = lens _riImageLocation (\s a -> s { _riImageLocation = a })

-- | The ID of the kernel.
riKernelId :: Lens' RegisterImage (Maybe Text)
riKernelId = lens _riKernelId (\s a -> s { _riKernelId = a })

-- | A name for your AMI. Constraints: 3-128 alphanumeric characters,
-- parentheses (()), square brackets ([]), spaces ( ), periods (.), slashes
-- (/), dashes (-), single quotes ('), at-signs (@), or underscores(_).
riName :: Lens' RegisterImage Text
riName = lens _riName (\s a -> s { _riName = a })

-- | The ID of the RAM disk.
riRamdiskId :: Lens' RegisterImage (Maybe Text)
riRamdiskId = lens _riRamdiskId (\s a -> s { _riRamdiskId = a })

-- | The name of the root device (for example, /dev/sda1, or xvda).
riRootDeviceName :: Lens' RegisterImage (Maybe Text)
riRootDeviceName = lens _riRootDeviceName (\s a -> s { _riRootDeviceName = a })

-- | Set to simple to enable enhanced networking for the AMI and any instances
-- that you launch from the AMI. There is no way to disable enhanced
-- networking at this time. This option is supported only for HVM AMIs.
-- Specifying this option with a PV AMI can make instances launched from the
-- AMI unreachable.
riSriovNetSupport :: Lens' RegisterImage (Maybe Text)
riSriovNetSupport =
    lens _riSriovNetSupport (\s a -> s { _riSriovNetSupport = a })

-- | The type of virtualization. Default: paravirtual.
riVirtualizationType :: Lens' RegisterImage (Maybe Text)
riVirtualizationType =
    lens _riVirtualizationType (\s a -> s { _riVirtualizationType = a })

instance ToPath RegisterImage where
    toPath = const "/"

instance ToQuery RegisterImage

newtype RegisterImageResult = RegisterImageResult
    { _rirImageId :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'RegisterImageResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rirImageId' @::@ 'Maybe' 'Text'
--
registerImageResult :: RegisterImageResult
registerImageResult = RegisterImageResult
    { _rirImageId = Nothing
    }

-- | The ID of the newly registered AMI.
rirImageId :: Lens' RegisterImageResult (Maybe Text)
rirImageId = lens _rirImageId (\s a -> s { _rirImageId = a })

instance AWSRequest RegisterImage where
    type Sv RegisterImage = EC2
    type Rs RegisterImage = RegisterImageResult

    request  = post "RegisterImage"
    response = const . xmlResponse $ \h x -> RegisterImageResult
        <$> x %| "imageId"
