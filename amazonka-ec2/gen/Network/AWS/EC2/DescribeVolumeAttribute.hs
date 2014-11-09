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
module Network.AWS.EC2.DescribeVolumeAttribute
    (
    -- * Request
      DescribeVolumeAttribute
    -- ** Request constructor
    , describeVolumeAttribute
    -- ** Request lenses
    , dva1Attribute
    , dva1DryRun
    , dva1VolumeId

    -- * Response
    , DescribeVolumeAttributeResult
    -- ** Response constructor
    , describeVolumeAttributeResult
    -- ** Response lenses
    , dvarAutoEnableIO
    , dvarProductCodes
    , dvarVolumeId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data DescribeVolumeAttribute = DescribeVolumeAttribute
    { _dva1Attribute :: Maybe Text
    , _dva1DryRun    :: Maybe Bool
    , _dva1VolumeId  :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeVolumeAttribute' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dva1Attribute' @::@ 'Maybe' 'Text'
--
-- * 'dva1DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dva1VolumeId' @::@ 'Text'
--
describeVolumeAttribute :: Text -- ^ 'dva1VolumeId'
                        -> DescribeVolumeAttribute
describeVolumeAttribute p1 = DescribeVolumeAttribute
    { _dva1VolumeId  = p1
    , _dva1DryRun    = Nothing
    , _dva1Attribute = Nothing
    }

-- | The instance attribute.
dva1Attribute :: Lens' DescribeVolumeAttribute (Maybe Text)
dva1Attribute = lens _dva1Attribute (\s a -> s { _dva1Attribute = a })

dva1DryRun :: Lens' DescribeVolumeAttribute (Maybe Bool)
dva1DryRun = lens _dva1DryRun (\s a -> s { _dva1DryRun = a })

-- | The ID of the volume.
dva1VolumeId :: Lens' DescribeVolumeAttribute Text
dva1VolumeId = lens _dva1VolumeId (\s a -> s { _dva1VolumeId = a })

instance ToPath DescribeVolumeAttribute where
    toPath = const "/"

instance ToQuery DescribeVolumeAttribute

data DescribeVolumeAttributeResult = DescribeVolumeAttributeResult
    { _dvarAutoEnableIO :: Maybe AttributeBooleanValue
    , _dvarProductCodes :: [ProductCode]
    , _dvarVolumeId     :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'DescribeVolumeAttributeResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvarAutoEnableIO' @::@ 'Maybe' 'AttributeBooleanValue'
--
-- * 'dvarProductCodes' @::@ ['ProductCode']
--
-- * 'dvarVolumeId' @::@ 'Maybe' 'Text'
--
describeVolumeAttributeResult :: DescribeVolumeAttributeResult
describeVolumeAttributeResult = DescribeVolumeAttributeResult
    { _dvarVolumeId     = Nothing
    , _dvarAutoEnableIO = Nothing
    , _dvarProductCodes = mempty
    }

-- | The state of autoEnableIO attribute.
dvarAutoEnableIO :: Lens' DescribeVolumeAttributeResult (Maybe AttributeBooleanValue)
dvarAutoEnableIO = lens _dvarAutoEnableIO (\s a -> s { _dvarAutoEnableIO = a })

-- | A list of product codes.
dvarProductCodes :: Lens' DescribeVolumeAttributeResult [ProductCode]
dvarProductCodes = lens _dvarProductCodes (\s a -> s { _dvarProductCodes = a })

-- | The ID of the volume.
dvarVolumeId :: Lens' DescribeVolumeAttributeResult (Maybe Text)
dvarVolumeId = lens _dvarVolumeId (\s a -> s { _dvarVolumeId = a })

instance AWSRequest DescribeVolumeAttribute where
    type Sv DescribeVolumeAttribute = EC2
    type Rs DescribeVolumeAttribute = DescribeVolumeAttributeResult

    request  = post "DescribeVolumeAttribute"
    response = const . xmlResponse $ \h x -> DescribeVolumeAttributeResult
record
