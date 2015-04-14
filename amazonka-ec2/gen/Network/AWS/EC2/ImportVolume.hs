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

-- Module      : Network.AWS.EC2.ImportVolume
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

-- | Creates an import volume task using metadata from the specified disk image.
-- After importing the image, you then upload it using the 'ec2-import-volume'
-- command in the Amazon EC2 command-line interface (CLI) tools. For more
-- information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UploadingYourInstancesandVolumes.html Using the Command Line Tools to Import Your Virtual Machineto Amazon EC2> in the /Amazon Elastic Compute Cloud User Guide for Linux/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ImportVolume.html>
module Network.AWS.EC2.ImportVolume
    (
    -- * Request
      ImportVolume
    -- ** Request constructor
    , importVolume
    -- ** Request lenses
    , ivAvailabilityZone
    , ivDescription
    , ivDryRun
    , ivImage
    , ivVolume

    -- * Response
    , ImportVolumeResponse
    -- ** Response constructor
    , importVolumeResponse
    -- ** Response lenses
    , ivrConversionTask
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data ImportVolume = ImportVolume
    { _ivAvailabilityZone :: Text
    , _ivDescription      :: Maybe Text
    , _ivDryRun           :: Maybe Bool
    , _ivImage            :: DiskImageDetail
    , _ivVolume           :: VolumeDetail
    } deriving (Eq, Read, Show)

-- | 'ImportVolume' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ivAvailabilityZone' @::@ 'Text'
--
-- * 'ivDescription' @::@ 'Maybe' 'Text'
--
-- * 'ivDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'ivImage' @::@ 'DiskImageDetail'
--
-- * 'ivVolume' @::@ 'VolumeDetail'
--
importVolume :: Text -- ^ 'ivAvailabilityZone'
             -> DiskImageDetail -- ^ 'ivImage'
             -> VolumeDetail -- ^ 'ivVolume'
             -> ImportVolume
importVolume p1 p2 p3 = ImportVolume
    { _ivAvailabilityZone = p1
    , _ivImage            = p2
    , _ivVolume           = p3
    , _ivDryRun           = Nothing
    , _ivDescription      = Nothing
    }

-- | The Availability Zone for the resulting Amazon EBS volume.
ivAvailabilityZone :: Lens' ImportVolume Text
ivAvailabilityZone =
    lens _ivAvailabilityZone (\s a -> s { _ivAvailabilityZone = a })

-- | An optional description for the volume being imported.
ivDescription :: Lens' ImportVolume (Maybe Text)
ivDescription = lens _ivDescription (\s a -> s { _ivDescription = a })

ivDryRun :: Lens' ImportVolume (Maybe Bool)
ivDryRun = lens _ivDryRun (\s a -> s { _ivDryRun = a })

ivImage :: Lens' ImportVolume DiskImageDetail
ivImage = lens _ivImage (\s a -> s { _ivImage = a })

ivVolume :: Lens' ImportVolume VolumeDetail
ivVolume = lens _ivVolume (\s a -> s { _ivVolume = a })

newtype ImportVolumeResponse = ImportVolumeResponse
    { _ivrConversionTask :: Maybe ConversionTask
    } deriving (Eq, Read, Show)

-- | 'ImportVolumeResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ivrConversionTask' @::@ 'Maybe' 'ConversionTask'
--
importVolumeResponse :: ImportVolumeResponse
importVolumeResponse = ImportVolumeResponse
    { _ivrConversionTask = Nothing
    }

ivrConversionTask :: Lens' ImportVolumeResponse (Maybe ConversionTask)
ivrConversionTask =
    lens _ivrConversionTask (\s a -> s { _ivrConversionTask = a })

instance ToPath ImportVolume where
    toPath = const "/"

instance ToQuery ImportVolume where
    toQuery ImportVolume{..} = mconcat
        [ "AvailabilityZone" =? _ivAvailabilityZone
        , "Description"      =? _ivDescription
        , "DryRun"           =? _ivDryRun
        , "Image"            =? _ivImage
        , "Volume"           =? _ivVolume
        ]

instance ToHeaders ImportVolume

instance AWSRequest ImportVolume where
    type Sv ImportVolume = EC2
    type Rs ImportVolume = ImportVolumeResponse

    request  = post "ImportVolume"
    response = xmlResponse

instance FromXML ImportVolumeResponse where
    parseXML x = ImportVolumeResponse
        <$> x .@? "conversionTask"
