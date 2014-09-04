{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.CopyImage
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Initiates the copy of an AMI from the specified source region to the region
-- in which the request was made. You specify the destination region by using
-- its endpoint when making the request. AMIs that use encrypted Amazon EBS
-- snapshots cannot be copied with this method. For more information, see
-- Copying AMIs in the Amazon Elastic Compute Cloud User Guide. Example This
-- example request copies the AMI in us-west-2 with the ID ami-1a2b3c4d,
-- naming the new AMI My-Standard-AMI.
-- https://ec2.amazonaws.com/?Action=CopyImage &amp;SourceRegion=us-west-2
-- &amp;SourceImageId=ami-1a2b3c4d &amp;Name=My-Standard-AMI
-- &amp;Description=This%20is%20the%20new%20version%20of%20My-Standard-AMI
-- &amp;ClientToken=550e8400-e29b-41d4-a716-446655440000 &amp;AUTHPARAMS
-- &lt;CopyImageResponse xmlns="http://ec2.amazonaws.com/doc/2014-02-01/"&gt;
-- &lt;requestId&gt;60bc441d-fa2c-494d-b155-5d6a3EXAMPLE&lt;/requestId&gt;
-- &lt;imageId&gt;ami-4d3c2b1a&lt;/imageId&gt; &lt;/CopyImageResponse&gt;.
module Network.AWS.EC2.V2014_06_15.CopyImage
    (
    -- * Request
      CopyImage
    -- ** Request constructor
    , copyImage
    -- ** Request lenses
    , cirSourceRegion
    , cirSourceImageId
    , cirName
    , cirDescription
    , cirClientToken

    -- * Response
    , CopyImageResponse
    -- ** Response lenses
    , cisImageId
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CopyImage' request.
copyImage :: Text -- ^ 'cirSourceRegion'
          -> Text -- ^ 'cirSourceImageId'
          -> CopyImage
copyImage p1 p2 = CopyImage
    { _cirSourceRegion = p1
    , _cirSourceImageId = p2
    , _cirName = Nothing
    , _cirDescription = Nothing
    , _cirClientToken = Nothing
    }
{-# INLINE copyImage #-}

data CopyImage = CopyImage
    { _cirSourceRegion :: Text
      -- ^ The name of the region that contains the AMI to copy.
    , _cirSourceImageId :: Text
      -- ^ The ID of the AMI to copy.
    , _cirName :: Maybe Text
      -- ^ The name of the new AMI in the destination region.
    , _cirDescription :: Maybe Text
      -- ^ A description for the new AMI in the destination region.
    , _cirClientToken :: Maybe Text
      -- ^ Unique, case-sensitive identifier you provide to ensure
      -- idempotency of the request. For more information, see How to
      -- Ensure Idempotency in the Amazon Elastic Compute Cloud User
      -- Guide.
    } deriving (Show, Generic)

-- | The name of the region that contains the AMI to copy.
cirSourceRegion :: Lens' CopyImage (Text)
cirSourceRegion f x =
    f (_cirSourceRegion x)
        <&> \y -> x { _cirSourceRegion = y }
{-# INLINE cirSourceRegion #-}

-- | The ID of the AMI to copy.
cirSourceImageId :: Lens' CopyImage (Text)
cirSourceImageId f x =
    f (_cirSourceImageId x)
        <&> \y -> x { _cirSourceImageId = y }
{-# INLINE cirSourceImageId #-}

-- | The name of the new AMI in the destination region.
cirName :: Lens' CopyImage (Maybe Text)
cirName f x =
    f (_cirName x)
        <&> \y -> x { _cirName = y }
{-# INLINE cirName #-}

-- | A description for the new AMI in the destination region.
cirDescription :: Lens' CopyImage (Maybe Text)
cirDescription f x =
    f (_cirDescription x)
        <&> \y -> x { _cirDescription = y }
{-# INLINE cirDescription #-}

-- | Unique, case-sensitive identifier you provide to ensure idempotency of the
-- request. For more information, see How to Ensure Idempotency in the Amazon
-- Elastic Compute Cloud User Guide.
cirClientToken :: Lens' CopyImage (Maybe Text)
cirClientToken f x =
    f (_cirClientToken x)
        <&> \y -> x { _cirClientToken = y }
{-# INLINE cirClientToken #-}

instance ToQuery CopyImage where
    toQuery = genericQuery def

data CopyImageResponse = CopyImageResponse
    { _cisImageId :: Maybe Text
      -- ^ The ID of the new AMI.
    } deriving (Show, Generic)

-- | The ID of the new AMI.
cisImageId :: Lens' CopyImageResponse (Maybe Text)
cisImageId f x =
    f (_cisImageId x)
        <&> \y -> x { _cisImageId = y }
{-# INLINE cisImageId #-}

instance FromXML CopyImageResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CopyImage where
    type Sv CopyImage = EC2
    type Rs CopyImage = CopyImageResponse

    request = post "CopyImage"
    response _ = xmlResponse
