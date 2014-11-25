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

-- Module      : Network.AWS.EC2.CopyImage
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
-- snapshots cannot be copied with this method.
--
-- For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/CopyingAMIs.html Copying AMIs> in the /Amazon Elastic Compute CloudUser Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CopyImage.html>
module Network.AWS.EC2.CopyImage
    (
    -- * Request
      CopyImage
    -- ** Request constructor
    , copyImage
    -- ** Request lenses
    , ciClientToken
    , ciDescription
    , ciDryRun
    , ciName
    , ciSourceImageId
    , ciSourceRegion

    -- * Response
    , CopyImageResponse
    -- ** Response constructor
    , copyImageResponse
    -- ** Response lenses
    , cir1ImageId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data CopyImage = CopyImage
    { _ciClientToken   :: Maybe Text
    , _ciDescription   :: Maybe Text
    , _ciDryRun        :: Maybe Bool
    , _ciName          :: Text
    , _ciSourceImageId :: Text
    , _ciSourceRegion  :: Text
    } deriving (Eq, Ord, Show)

-- | 'CopyImage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ciClientToken' @::@ 'Maybe' 'Text'
--
-- * 'ciDescription' @::@ 'Maybe' 'Text'
--
-- * 'ciDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'ciName' @::@ 'Text'
--
-- * 'ciSourceImageId' @::@ 'Text'
--
-- * 'ciSourceRegion' @::@ 'Text'
--
copyImage :: Text -- ^ 'ciSourceRegion'
          -> Text -- ^ 'ciSourceImageId'
          -> Text -- ^ 'ciName'
          -> CopyImage
copyImage p1 p2 p3 = CopyImage
    { _ciSourceRegion  = p1
    , _ciSourceImageId = p2
    , _ciName          = p3
    , _ciDryRun        = Nothing
    , _ciDescription   = Nothing
    , _ciClientToken   = Nothing
    }

-- | Unique, case-sensitive identifier you provide to ensure idempotency of the
-- request. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency> in the /AmazonElastic Compute Cloud User Guide/.
--
ciClientToken :: Lens' CopyImage (Maybe Text)
ciClientToken = lens _ciClientToken (\s a -> s { _ciClientToken = a })

-- | A description for the new AMI in the destination region.
--
ciDescription :: Lens' CopyImage (Maybe Text)
ciDescription = lens _ciDescription (\s a -> s { _ciDescription = a })

ciDryRun :: Lens' CopyImage (Maybe Bool)
ciDryRun = lens _ciDryRun (\s a -> s { _ciDryRun = a })

-- | The name of the new AMI in the destination region.
--
ciName :: Lens' CopyImage Text
ciName = lens _ciName (\s a -> s { _ciName = a })

-- | The ID of the AMI to copy.
--
ciSourceImageId :: Lens' CopyImage Text
ciSourceImageId = lens _ciSourceImageId (\s a -> s { _ciSourceImageId = a })

-- | The name of the region that contains the AMI to copy.
--
ciSourceRegion :: Lens' CopyImage Text
ciSourceRegion = lens _ciSourceRegion (\s a -> s { _ciSourceRegion = a })

newtype CopyImageResponse = CopyImageResponse
    { _cir1ImageId :: Maybe Text
    } deriving (Eq, Ord, Show, Monoid)

-- | 'CopyImageResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cir1ImageId' @::@ 'Maybe' 'Text'
--
copyImageResponse :: CopyImageResponse
copyImageResponse = CopyImageResponse
    { _cir1ImageId = Nothing
    }

-- | The ID of the new AMI.
--
cir1ImageId :: Lens' CopyImageResponse (Maybe Text)
cir1ImageId = lens _cir1ImageId (\s a -> s { _cir1ImageId = a })

instance ToPath CopyImage where
    toPath = const "/"

instance ToQuery CopyImage where
    toQuery CopyImage{..} = mconcat
        [ "ClientToken"   =? _ciClientToken
        , "Description"   =? _ciDescription
        , "dryRun"        =? _ciDryRun
        , "Name"          =? _ciName
        , "SourceImageId" =? _ciSourceImageId
        , "SourceRegion"  =? _ciSourceRegion
        ]

instance ToHeaders CopyImage

instance AWSRequest CopyImage where
    type Sv CopyImage = EC2
    type Rs CopyImage = CopyImageResponse

    request  = post "CopyImage"
    response = xmlResponse

instance FromXML CopyImageResponse where
    parseXML x = CopyImageResponse
        <$> x .@? "imageId"
