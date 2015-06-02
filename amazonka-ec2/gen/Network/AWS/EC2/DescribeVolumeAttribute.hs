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

-- Module      : Network.AWS.EC2.DescribeVolumeAttribute
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

-- | Describes the specified attribute of the specified volume. You can specify
-- only one attribute at a time.
--
-- For more information about EBS volumes, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumes.html Amazon EBS Volumes> in the /AmazonElastic Compute Cloud User Guide/.
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
    { _dvaAttribute :: Maybe VolumeAttributeName
    , _dvaDryRun    :: Maybe Bool
    , _dvaVolumeId  :: Text
    } deriving (Eq, Read, Show)

-- | 'DescribeVolumeAttribute' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvaAttribute' @::@ 'Maybe' 'VolumeAttributeName'
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
dvaAttribute :: Lens' DescribeVolumeAttribute (Maybe VolumeAttributeName)
dvaAttribute = lens _dvaAttribute (\s a -> s { _dvaAttribute = a })

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have the
-- required permissions, the error response is 'DryRunOperation'. Otherwise, it is 'UnauthorizedOperation'.
dvaDryRun :: Lens' DescribeVolumeAttribute (Maybe Bool)
dvaDryRun = lens _dvaDryRun (\s a -> s { _dvaDryRun = a })

-- | The ID of the volume.
dvaVolumeId :: Lens' DescribeVolumeAttribute Text
dvaVolumeId = lens _dvaVolumeId (\s a -> s { _dvaVolumeId = a })

data DescribeVolumeAttributeResponse = DescribeVolumeAttributeResponse
    { _dvarAutoEnableIO :: Maybe AttributeBooleanValue
    , _dvarProductCodes :: List "item" ProductCode
    , _dvarVolumeId     :: Maybe Text
    } deriving (Eq, Read, Show)

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

-- | The state of 'autoEnableIO' attribute.
dvarAutoEnableIO :: Lens' DescribeVolumeAttributeResponse (Maybe AttributeBooleanValue)
dvarAutoEnableIO = lens _dvarAutoEnableIO (\s a -> s { _dvarAutoEnableIO = a })

-- | A list of product codes.
dvarProductCodes :: Lens' DescribeVolumeAttributeResponse [ProductCode]
dvarProductCodes = lens _dvarProductCodes (\s a -> s { _dvarProductCodes = a }) . _List

-- | The ID of the volume.
dvarVolumeId :: Lens' DescribeVolumeAttributeResponse (Maybe Text)
dvarVolumeId = lens _dvarVolumeId (\s a -> s { _dvarVolumeId = a })

instance ToPath DescribeVolumeAttribute where
    toPath = const "/"

instance ToQuery DescribeVolumeAttribute where
    toQuery DescribeVolumeAttribute{..} = mconcat
        [ "Attribute" =? _dvaAttribute
        , "DryRun"    =? _dvaDryRun
        , "VolumeId"  =? _dvaVolumeId
        ]

instance ToHeaders DescribeVolumeAttribute

instance AWSRequest DescribeVolumeAttribute where
    type Sv DescribeVolumeAttribute = EC2
    type Rs DescribeVolumeAttribute = DescribeVolumeAttributeResponse

    request  = post "DescribeVolumeAttribute"
    response = xmlResponse

instance FromXML DescribeVolumeAttributeResponse where
    parseXML x = DescribeVolumeAttributeResponse
        <$> x .@? "autoEnableIO"
        <*> x .@? "productCodes" .!@ mempty
        <*> x .@? "volumeId"
