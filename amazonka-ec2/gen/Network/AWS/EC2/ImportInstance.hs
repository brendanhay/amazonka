{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.ImportInstance
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates an import instance task using metadata from the specified disk
-- image. After importing the image, you then upload it using the
-- ec2-import-volume command in the EC2 command line tools. For more
-- information, see Using the Command Line Tools to Import Your Virtual
-- Machine to Amazon EC2 in the Amazon Elastic Compute Cloud User Guide.
-- Example This example creates an import instance task that migrates a
-- Windows Server 2008 SP2 (32-bit) VM into the AWS us-east-1 region.
-- https://ec2.amazonaws.com/?Action=ImportInstance
-- &amp;LaunchSpecification.Architecture=x86_64
-- &amp;LaunchSpecification.InstanceType=m1.xlarge
-- &amp;DiskImage.1.Image.Format=VMDK &amp;DiskImage.1.Image.Bytes=1179593728
-- &amp;DiskImage.1.Image.ImportManifestUrl=https://s3.amazonaws.com/myawsbucket/​a3a5e1b6-590d-43cc-97c1-15c7325d3f41/​Win_2008_Server_Data_Center_SP2_32-bit.​vmdkmanifest.xml?AWSAccessKeyId=​AKIAIOSFODNN7EXAMPLE&amp;​Expires=1294855591&amp;​Signature=5snej01TlTtL0uR7KExtEXAMPLE%3D
-- &amp;DiskImage.1.Volume.Size=12 &amp;Platform=Windows &amp;AUTHPARAMS
-- import-i-ffvko9js 2010-12-22T12:01Z 0 us-east-1a VMDK 1179593728
-- https://s3.amazonaws.com/myawsbucket/​a3a5e1b6-590d-43cc-97c1-15c7325d3f41/​Win_2008_Server_Data_Center_SP2_32-bit.​vmdkmanifest.xml?AWSAccessKeyId=​AKIAIOSFODNN7EXAMPLE&amp;​Expires=1294855591&amp;​Signature=5snej01TlTtL0uR7KExtEXAMPLE%3D
-- 12 vol-1a2b3c4d active i-12655a7f.
module Network.AWS.EC2.ImportInstance
    (
    -- * Request
      ImportInstance
    -- ** Request constructor
    , mkImportInstance
    -- ** Request lenses
    , iiDescription
    , iiLaunchSpecification
    , iiDiskImages
    , iiPlatform

    -- * Response
    , ImportInstanceResponse
    -- ** Response constructor
    , mkImportInstanceResponse
    -- ** Response lenses
    , iirConversionTask
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

data ImportInstance = ImportInstance
    { _iiDescription :: !(Maybe Text)
    , _iiLaunchSpecification :: Maybe ImportInstanceLaunchSpecification
    , _iiDiskImages :: [DiskImage]
    , _iiPlatform :: PlatformValues
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ImportInstance' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Description ::@ @Maybe Text@
--
-- * @LaunchSpecification ::@ @Maybe ImportInstanceLaunchSpecification@
--
-- * @DiskImages ::@ @[DiskImage]@
--
-- * @Platform ::@ @PlatformValues@
--
mkImportInstance :: PlatformValues -- ^ 'iiPlatform'
                 -> ImportInstance
mkImportInstance p4 = ImportInstance
    { _iiDescription = Nothing
    , _iiLaunchSpecification = Nothing
    , _iiDiskImages = mempty
    , _iiPlatform = p4
    }

-- | A description for the instance being imported.
iiDescription :: Lens' ImportInstance (Maybe Text)
iiDescription = lens _iiDescription (\s a -> s { _iiDescription = a })

-- | 
iiLaunchSpecification :: Lens' ImportInstance (Maybe ImportInstanceLaunchSpecification)
iiLaunchSpecification =
    lens _iiLaunchSpecification (\s a -> s { _iiLaunchSpecification = a })

-- | 
iiDiskImages :: Lens' ImportInstance [DiskImage]
iiDiskImages = lens _iiDiskImages (\s a -> s { _iiDiskImages = a })

-- | The instance operating system.
iiPlatform :: Lens' ImportInstance PlatformValues
iiPlatform = lens _iiPlatform (\s a -> s { _iiPlatform = a })

instance ToQuery ImportInstance where
    toQuery = genericQuery def

newtype ImportInstanceResponse = ImportInstanceResponse
    { _iirConversionTask :: Maybe ConversionTask
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ImportInstanceResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ConversionTask ::@ @Maybe ConversionTask@
--
mkImportInstanceResponse :: ImportInstanceResponse
mkImportInstanceResponse = ImportInstanceResponse
    { _iirConversionTask = Nothing
    }

-- | 
iirConversionTask :: Lens' ImportInstanceResponse (Maybe ConversionTask)
iirConversionTask =
    lens _iirConversionTask (\s a -> s { _iirConversionTask = a })

instance FromXML ImportInstanceResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ImportInstance where
    type Sv ImportInstance = EC2
    type Rs ImportInstance = ImportInstanceResponse

    request = post "ImportInstance"
    response _ = xmlResponse
