{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.CreateImage
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
-- Example This example request creates an AMI from the specified instance.
-- https://ec2.amazonaws.com/?Action=CreateImage
-- &amp;Description=Standard+Web+Server+v1.0 &amp;InstanceId=i-10a64379
-- &amp;Name=standard-web-server-v1.0 &amp;AUTHPARAMS &lt;CreateImageResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-02-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;imageId&gt;ami-4fa54026&lt;/imageId&gt; &lt;/CreateImageResponse&gt;.
module Network.AWS.EC2.V2014_06_15.CreateImage
    (
    -- * Request
      CreateImage
    -- ** Request constructor
    , mkCreateImage
    -- ** Request lenses
    , ci1InstanceId
    , ci1Name
    , ci1Description
    , ci1NoReboot
    , ci1BlockDeviceMappings

    -- * Response
    , CreateImageResponse
    -- ** Response constructor
    , mkCreateImageResponse
    -- ** Response lenses
    , cirrImageId
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

data CreateImage = CreateImage
    { _ci1InstanceId :: Text
    , _ci1Name :: Text
    , _ci1Description :: Maybe Text
    , _ci1NoReboot :: Maybe Bool
    , _ci1BlockDeviceMappings :: [BlockDeviceMapping]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateImage' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @InstanceId ::@ @Text@
--
-- * @Name ::@ @Text@
--
-- * @Description ::@ @Maybe Text@
--
-- * @NoReboot ::@ @Maybe Bool@
--
-- * @BlockDeviceMappings ::@ @[BlockDeviceMapping]@
--
mkCreateImage :: Text -- ^ 'ci1InstanceId'
              -> Text -- ^ 'ci1Name'
              -> CreateImage
mkCreateImage p1 p2 = CreateImage
    { _ci1InstanceId = p1
    , _ci1Name = p2
    , _ci1Description = Nothing
    , _ci1NoReboot = Nothing
    , _ci1BlockDeviceMappings = mempty
    }

-- | The ID of the instance.
ci1InstanceId :: Lens' CreateImage Text
ci1InstanceId = lens _ci1InstanceId (\s a -> s { _ci1InstanceId = a })

-- | A name for the new image. Constraints: 3-128 alphanumeric characters,
-- parenthesis (()), periods (.), slashes (/), dashes (-), or underscores(_).
ci1Name :: Lens' CreateImage Text
ci1Name = lens _ci1Name (\s a -> s { _ci1Name = a })

-- | A description for the new image.
ci1Description :: Lens' CreateImage (Maybe Text)
ci1Description = lens _ci1Description (\s a -> s { _ci1Description = a })

-- | By default, this parameter is set to false, which means Amazon EC2 attempts
-- to shut down the instance cleanly before image creation and then reboots
-- the instance. When the parameter is set to true, Amazon EC2 doesn't shut
-- down the instance before creating the image. When this option is used, file
-- system integrity on the created image can't be guaranteed.
ci1NoReboot :: Lens' CreateImage (Maybe Bool)
ci1NoReboot = lens _ci1NoReboot (\s a -> s { _ci1NoReboot = a })

-- | Information about one or more block device mappings.
ci1BlockDeviceMappings :: Lens' CreateImage [BlockDeviceMapping]
ci1BlockDeviceMappings =
    lens _ci1BlockDeviceMappings (\s a -> s { _ci1BlockDeviceMappings = a })

instance ToQuery CreateImage where
    toQuery = genericQuery def

newtype CreateImageResponse = CreateImageResponse
    { _cirrImageId :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateImageResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ImageId ::@ @Maybe Text@
--
mkCreateImageResponse :: CreateImageResponse
mkCreateImageResponse = CreateImageResponse
    { _cirrImageId = Nothing
    }

-- | The ID of the new AMI.
cirrImageId :: Lens' CreateImageResponse (Maybe Text)
cirrImageId = lens _cirrImageId (\s a -> s { _cirrImageId = a })

instance FromXML CreateImageResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateImage where
    type Sv CreateImage = EC2
    type Rs CreateImage = CreateImageResponse

    request = post "CreateImage"
    response _ = xmlResponse
