{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.ImportInstance
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
module Network.AWS.EC2.V2014_06_15.ImportInstance
    (
    -- * Request
      ImportInstance
    -- ** Request constructor
    , importInstance
    -- ** Request lenses
    , iiiiiiiiiuPlatform
    , iiiiiiiiiuDiskImages
    , iiiiiiiiiuLaunchSpecification
    , iiiiiiiiiuDescription

    -- * Response
    , ImportInstanceResponse
    -- ** Response lenses
    , iiiiiiiiivConversionTask
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ImportInstance' request.
importInstance :: PlatformValues -- ^ 'iiiiiiiiiuPlatform'
               -> ImportInstance
importInstance p1 = ImportInstance
    { _iiiiiiiiiuPlatform = p1
    , _iiiiiiiiiuDiskImages = mempty
    , _iiiiiiiiiuLaunchSpecification = Nothing
    , _iiiiiiiiiuDescription = Nothing
    }
{-# INLINE importInstance #-}

data ImportInstance = ImportInstance
    { _iiiiiiiiiuPlatform :: PlatformValues
      -- ^ The instance operating system.
    , _iiiiiiiiiuDiskImages :: [DiskImage]
      -- ^ 
    , _iiiiiiiiiuLaunchSpecification :: Maybe ImportInstanceLaunchSpecification
      -- ^ 
    , _iiiiiiiiiuDescription :: Maybe Text
      -- ^ A description for the instance being imported.
    } deriving (Show, Generic)

-- | The instance operating system.
iiiiiiiiiuPlatform :: Lens' ImportInstance (PlatformValues)
iiiiiiiiiuPlatform f x =
    f (_iiiiiiiiiuPlatform x)
        <&> \y -> x { _iiiiiiiiiuPlatform = y }
{-# INLINE iiiiiiiiiuPlatform #-}

-- | 
iiiiiiiiiuDiskImages :: Lens' ImportInstance ([DiskImage])
iiiiiiiiiuDiskImages f x =
    f (_iiiiiiiiiuDiskImages x)
        <&> \y -> x { _iiiiiiiiiuDiskImages = y }
{-# INLINE iiiiiiiiiuDiskImages #-}

-- | 
iiiiiiiiiuLaunchSpecification :: Lens' ImportInstance (Maybe ImportInstanceLaunchSpecification)
iiiiiiiiiuLaunchSpecification f x =
    f (_iiiiiiiiiuLaunchSpecification x)
        <&> \y -> x { _iiiiiiiiiuLaunchSpecification = y }
{-# INLINE iiiiiiiiiuLaunchSpecification #-}

-- | A description for the instance being imported.
iiiiiiiiiuDescription :: Lens' ImportInstance (Maybe Text)
iiiiiiiiiuDescription f x =
    f (_iiiiiiiiiuDescription x)
        <&> \y -> x { _iiiiiiiiiuDescription = y }
{-# INLINE iiiiiiiiiuDescription #-}

instance ToQuery ImportInstance where
    toQuery = genericQuery def

data ImportInstanceResponse = ImportInstanceResponse
    { _iiiiiiiiivConversionTask :: Maybe ConversionTask
      -- ^ 
    } deriving (Show, Generic)

-- | 
iiiiiiiiivConversionTask :: Lens' ImportInstanceResponse (Maybe ConversionTask)
iiiiiiiiivConversionTask f x =
    f (_iiiiiiiiivConversionTask x)
        <&> \y -> x { _iiiiiiiiivConversionTask = y }
{-# INLINE iiiiiiiiivConversionTask #-}

instance FromXML ImportInstanceResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ImportInstance where
    type Sv ImportInstance = EC2
    type Rs ImportInstance = ImportInstanceResponse

    request = post "ImportInstance"
    response _ = xmlResponse
