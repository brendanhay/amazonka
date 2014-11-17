{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeVolumeAttribute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes the specified attribute of the specified volume. You can specify
-- only one attribute at a time. For more information about Amazon EBS
-- volumes, see Amazon EBS Volumes in the Amazon Elastic Compute Cloud User
-- Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeVolumeAttribute.html>
module Network.AWS.EC2.DescribeVolumeAttribute
    (
    -- * Request
      DescribeVolumeAttribute
    -- ** Request constructor
    , describeVolumeAttribute
    -- ** Request lenses
    , dvaAttribute
    , dvaDryRun
    , dvaVolumeId

    -- * Response
    , DescribeVolumeAttributeResponse
    -- ** Response constructor
    , describeVolumeAttributeResponse
    -- ** Response lenses
    , dvarAutoEnableIO
    , dvarProductCodes
    , dvarVolumeId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DescribeVolumeAttribute = DescribeVolumeAttribute
    { _dvaAttribute :: Maybe Text
    , _dvaDryRun    :: Maybe Bool
    , _dvaVolumeId  :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeVolumeAttribute' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvaAttribute' @::@ 'Maybe' 'Text'
--
-- * 'dvaDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dvaVolumeId' @::@ 'Text'
--
describeVolumeAttribute :: Text -- ^ 'dvaVolumeId'
                        -> DescribeVolumeAttribute
describeVolumeAttribute p1 = DescribeVolumeAttribute
    { _dvaVolumeId  = p1
    , _dvaDryRun    = Nothing
    , _dvaAttribute = Nothing
    }

-- | The instance attribute.
dvaAttribute :: Lens' DescribeVolumeAttribute (Maybe Text)
dvaAttribute = lens _dvaAttribute (\s a -> s { _dvaAttribute = a })

dvaDryRun :: Lens' DescribeVolumeAttribute (Maybe Bool)
dvaDryRun = lens _dvaDryRun (\s a -> s { _dvaDryRun = a })

-- | The ID of the volume.
dvaVolumeId :: Lens' DescribeVolumeAttribute Text
dvaVolumeId = lens _dvaVolumeId (\s a -> s { _dvaVolumeId = a })

data DescribeVolumeAttributeResponse = DescribeVolumeAttributeResponse
    { _dvarAutoEnableIO :: Maybe AttributeBooleanValue
    , _dvarProductCodes :: [ProductCode]
    , _dvarVolumeId     :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'DescribeVolumeAttributeResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvarAutoEnableIO' @::@ 'Maybe' 'AttributeBooleanValue'
--
-- * 'dvarProductCodes' @::@ ['ProductCode']
--
-- * 'dvarVolumeId' @::@ 'Maybe' 'Text'
--
describeVolumeAttributeResponse :: DescribeVolumeAttributeResponse
describeVolumeAttributeResponse = DescribeVolumeAttributeResponse
    { _dvarVolumeId     = Nothing
    , _dvarAutoEnableIO = Nothing
    , _dvarProductCodes = mempty
    }

-- | The state of autoEnableIO attribute.
dvarAutoEnableIO :: Lens' DescribeVolumeAttributeResponse (Maybe AttributeBooleanValue)
dvarAutoEnableIO = lens _dvarAutoEnableIO (\s a -> s { _dvarAutoEnableIO = a })

-- | A list of product codes.
dvarProductCodes :: Lens' DescribeVolumeAttributeResponse [ProductCode]
dvarProductCodes = lens _dvarProductCodes (\s a -> s { _dvarProductCodes = a })

-- | The ID of the volume.
dvarVolumeId :: Lens' DescribeVolumeAttributeResponse (Maybe Text)
dvarVolumeId = lens _dvarVolumeId (\s a -> s { _dvarVolumeId = a })

instance ToPath DescribeVolumeAttribute where
    toPath = const "/"

instance ToQuery DescribeVolumeAttribute

instance ToHeaders DescribeVolumeAttribute

instance AWSRequest DescribeVolumeAttribute where
    type Sv DescribeVolumeAttribute = EC2
    type Rs DescribeVolumeAttribute = DescribeVolumeAttributeResponse

    request  = post "DescribeVolumeAttribute"
    response = xmlResponse

instance FromXML DescribeVolumeAttributeResponse where
    parseXML c = DescribeVolumeAttributeResponse
        <$> c .: "autoEnableIO"
        <*> c .: "productCodes"
        <*> c .: "volumeId"
