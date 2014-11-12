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

-- Module      : Network.AWS.EC2.CreateImage
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates an Amazon EBS-backed AMI from an Amazon EBS-backed instance that is
-- either running or stopped. If you customized your instance with instance
-- store volumes or EBS volumes in addition to the root device volume, the new
-- AMI contains block device mapping information for those volumes. When you
-- launch an instance from this new AMI, the instance automatically launches
-- with those additional volumes. For more information, see Creating Amazon
-- EBS-Backed Linux AMIs in the Amazon Elastic Compute Cloud User Guide.
module Network.AWS.EC2.CreateImage
    (
    -- * Request
      CreateImage
    -- ** Request constructor
    , createImage
    -- ** Request lenses
    , ci1BlockDeviceMappings
    , ci1Description
    , ci1DryRun
    , ci1InstanceId
    , ci1Name
    , ci1NoReboot

    -- * Response
    , CreateImageResult
    -- ** Response constructor
    , createImageResponse
    -- ** Response lenses
    , cirImageId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data CreateImage = CreateImage
    { _ci1BlockDeviceMappings :: [BlockDeviceMapping]
    , _ci1Description         :: Maybe Text
    , _ci1DryRun              :: Maybe Bool
    , _ci1InstanceId          :: Text
    , _ci1Name                :: Text
    , _ci1NoReboot            :: Maybe Bool
    } deriving (Eq, Show, Generic)

-- | 'CreateImage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ci1BlockDeviceMappings' @::@ ['BlockDeviceMapping']
--
-- * 'ci1Description' @::@ 'Maybe' 'Text'
--
-- * 'ci1DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'ci1InstanceId' @::@ 'Text'
--
-- * 'ci1Name' @::@ 'Text'
--
-- * 'ci1NoReboot' @::@ 'Maybe' 'Bool'
--
createImage :: Text -- ^ 'ci1InstanceId'
            -> Text -- ^ 'ci1Name'
            -> CreateImage
createImage p1 p2 = CreateImage
    { _ci1InstanceId          = p1
    , _ci1Name                = p2
    , _ci1DryRun              = Nothing
    , _ci1Description         = Nothing
    , _ci1NoReboot            = Nothing
    , _ci1BlockDeviceMappings = mempty
    }

-- | Information about one or more block device mappings.
ci1BlockDeviceMappings :: Lens' CreateImage [BlockDeviceMapping]
ci1BlockDeviceMappings =
    lens _ci1BlockDeviceMappings (\s a -> s { _ci1BlockDeviceMappings = a })

-- | A description for the new image.
ci1Description :: Lens' CreateImage (Maybe Text)
ci1Description = lens _ci1Description (\s a -> s { _ci1Description = a })

ci1DryRun :: Lens' CreateImage (Maybe Bool)
ci1DryRun = lens _ci1DryRun (\s a -> s { _ci1DryRun = a })

-- | The ID of the instance.
ci1InstanceId :: Lens' CreateImage Text
ci1InstanceId = lens _ci1InstanceId (\s a -> s { _ci1InstanceId = a })

-- | A name for the new image. Constraints: 3-128 alphanumeric characters,
-- parentheses (()), square brackets ([]), spaces ( ), periods (.), slashes
-- (/), dashes (-), single quotes ('), at-signs (@), or underscores(_).
ci1Name :: Lens' CreateImage Text
ci1Name = lens _ci1Name (\s a -> s { _ci1Name = a })

-- | By default, this parameter is set to false, which means Amazon EC2
-- attempts to shut down the instance cleanly before image creation and then
-- reboots the instance. When the parameter is set to true, Amazon EC2
-- doesn't shut down the instance before creating the image. When this
-- option is used, file system integrity on the created image can't be
-- guaranteed.
ci1NoReboot :: Lens' CreateImage (Maybe Bool)
ci1NoReboot = lens _ci1NoReboot (\s a -> s { _ci1NoReboot = a })

instance ToQuery CreateImage

instance ToPath CreateImage where
    toPath = const "/"

newtype CreateImageResult = CreateImageResult
    { _cirImageId :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'CreateImageResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cirImageId' @::@ 'Maybe' 'Text'
--
createImageResponse :: CreateImageResult
createImageResponse = CreateImageResult
    { _cirImageId = Nothing
    }

-- | The ID of the new AMI.
cirImageId :: Lens' CreateImageResult (Maybe Text)
cirImageId = lens _cirImageId (\s a -> s { _cirImageId = a })

instance FromXML CreateImageResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CreateImageResult"

instance AWSRequest CreateImage where
    type Sv CreateImage = EC2
    type Rs CreateImage = CreateImageResult

    request  = post "CreateImage"
    response = xmlResponse $ \h x -> CreateImageResult
        <$> x %| "imageId"
