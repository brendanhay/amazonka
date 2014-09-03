{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.ImportVolume
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates an import volume task using metadata from the specified disk image.
-- After importing the image, you then upload it using the ec2-import-volume
-- command in the Amazon EC2 command-line interface (CLI) tools. For more
-- information, see Using the Command Line Tools to Import Your Virtual
-- Machine to Amazon EC2 in the Amazon Elastic Compute Cloud User Guide.
-- Example This example creates an import volume task that migrates a Windows
-- Server 2008 SP2 (32-bit) volume into the AWS us-east-1 region.
-- https://ec2.amazonaws.com/?Action=ImportVolume
-- &amp;AvailabilityZone=us-east-1c &amp;Image.Format=VMDK
-- &amp;Image.Bytes=128696320
-- &amp;Image.ImportManifestUrl=https://s3.amazonaws.com/myawsbucket/​a3a5e1b6-590d-43cc-97c1-15c7325d3f41/​Win_2008_Server_Data_Center_SP2_32-bit.​vmdkmanifest.xml?AWSAccessKeyId=​AKIAIOSFODNN7EXAMPLE&amp;​Expires=1294855591&amp;​Signature=5snej01TlTtL0uR7KExtEXAMPLE%3D
-- &amp;VolumeSize=8 &amp;AUTHPARAMS&gt; import-i-fh95npoc 2010-12-22T12:01Z 0
-- us-east-1c VDMK 128696320
-- https://s3.amazonaws.com/myawsbucket/​a3a5e1b6-590d-43cc-97c1-15c7325d3f41/​Win_2008_Server_Data_Center_SP2_32-bit.​vmdkmanifest.xml?AWSAccessKeyId=​AKIAIOSFODNN7EXAMPLE&amp;​Expires=1294855591&amp;​Signature=5snej01TlTtL0uR7KExtEXAMPLE%3D
-- ccb1b0536a4a70e86016b85229b5c6b10b14a4eb 8 vol-34d8a2ff active.
module Network.AWS.EC2.V2014_06_15.ImportVolume where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ImportVolume' request.
importVolume :: DiskImageDetail -- ^ '_ivrImage'
             -> Text -- ^ '_ivrAvailabilityZone'
             -> VolumeDetail -- ^ '_ivrVolume'
             -> ImportVolume
importVolume p1 p2 p3 = ImportVolume
    { _ivrImage = p1
    , _ivrAvailabilityZone = p2
    , _ivrVolume = p3
    , _ivrDescription = Nothing
    }

data ImportVolume = ImportVolume
    { _ivrImage :: DiskImageDetail
      -- ^ 
    , _ivrAvailabilityZone :: Text
      -- ^ The Availability Zone for the resulting Amazon EBS volume.
    , _ivrVolume :: VolumeDetail
      -- ^ 
    , _ivrDescription :: Maybe Text
      -- ^ An optional description for the volume being imported.
    } deriving (Show, Generic)

makeLenses ''ImportVolume

instance ToQuery ImportVolume where
    toQuery = genericQuery def

data ImportVolumeResponse = ImportVolumeResponse
    { _ivsConversionTask :: Maybe ConversionTask
      -- ^ 
    } deriving (Show, Generic)

makeLenses ''ImportVolumeResponse

instance FromXML ImportVolumeResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ImportVolume where
    type Sv ImportVolume = EC2
    type Rs ImportVolume = ImportVolumeResponse

    request = post "ImportVolume"
    response _ = xmlResponse
