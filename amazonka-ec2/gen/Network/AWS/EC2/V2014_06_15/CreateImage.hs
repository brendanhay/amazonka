{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
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
module Network.AWS.EC2.V2014_06_15.CreateImage where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CreateImage' request.
createImage :: Text -- ^ '_citInstanceId'
            -> Text -- ^ '_citName'
            -> CreateImage
createImage p1 p2 = CreateImage
    { _citInstanceId = p1
    , _citName = p2
    , _citBlockDeviceMappings = mempty
    , _citNoReboot = Nothing
    , _citDryRun = Nothing
    , _citDescription = Nothing
    }

data CreateImage = CreateImage
    { _citInstanceId :: Text
      -- ^ The ID of the instance.
    , _citName :: Text
      -- ^ A name for the new image. Constraints: 3-128 alphanumeric
      -- characters, parenthesis (()), periods (.), slashes (/), dashes
      -- (-), or underscores(_).
    , _citBlockDeviceMappings :: [BlockDeviceMapping]
      -- ^ Information about one or more block device mappings.
    , _citNoReboot :: Maybe Bool
      -- ^ By default, this parameter is set to false, which means Amazon
      -- EC2 attempts to shut down the instance cleanly before image
      -- creation and then reboots the instance. When the parameter is set
      -- to true, Amazon EC2 doesn't shut down the instance before
      -- creating the image. When this option is used, file system
      -- integrity on the created image can't be guaranteed.
    , _citDryRun :: Maybe Bool
      -- ^ 
    , _citDescription :: Maybe Text
      -- ^ A description for the new image.
    } deriving (Show, Generic)

makeLenses ''CreateImage

instance ToQuery CreateImage where
    toQuery = genericQuery def

data CreateImageResponse = CreateImageResponse
    { _ciuImageId :: Maybe Text
      -- ^ The ID of the new AMI.
    } deriving (Show, Generic)

makeLenses ''CreateImageResponse

instance FromXML CreateImageResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateImage where
    type Sv CreateImage = EC2
    type Rs CreateImage = CreateImageResponse

    request = post "CreateImage"
    response _ = xmlResponse
