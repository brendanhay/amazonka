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

-- Module      : Network.AWS.AutoScaling.DescribeAutoScalingInstances
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a description of each Auto Scaling instance in the InstanceIds
-- list. If a list is not provided, the service returns the full details of
-- all instances up to a maximum of 50. By default, the service returns a list
-- of 20 items. This action supports pagination by returning a token if there
-- are more pages to retrieve. To get the next page, call this action again
-- with the returned token as the NextToken parameter.
module Network.AWS.AutoScaling.DescribeAutoScalingInstances
    (
    -- * Request
      DescribeAutoScalingInstancesType
    -- ** Request constructor
    , describeAutoScalingInstancesType
    -- ** Request lenses
    , dasitInstanceIds
    , dasitMaxRecords
    , dasitNextToken

    -- * Response
    , AutoScalingInstancesType
    -- ** Response constructor
    , autoScalingInstancesType
    -- ** Response lenses
    , asitAutoScalingInstances
    , asitNextToken
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types

data DescribeAutoScalingInstancesType = DescribeAutoScalingInstancesType
    { _dasitInstanceIds :: [Text]
    , _dasitMaxRecords  :: Maybe Int
    , _dasitNextToken   :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeAutoScalingInstancesType' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dasitInstanceIds' @::@ ['Text']
--
-- * 'dasitMaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'dasitNextToken' @::@ 'Maybe' 'Text'
--
describeAutoScalingInstancesType :: DescribeAutoScalingInstancesType
describeAutoScalingInstancesType = DescribeAutoScalingInstancesType
    { _dasitInstanceIds = mempty
    , _dasitMaxRecords  = Nothing
    , _dasitNextToken   = Nothing
    }

-- | The list of Auto Scaling instances to describe. If this list is omitted,
-- all auto scaling instances are described. The list of requested instances
-- cannot contain more than 50 items. If unknown instances are requested,
-- they are ignored with no error.
dasitInstanceIds :: Lens' DescribeAutoScalingInstancesType [Text]
dasitInstanceIds = lens _dasitInstanceIds (\s a -> s { _dasitInstanceIds = a })

-- | The maximum number of Auto Scaling instances to be described with each
-- call.
dasitMaxRecords :: Lens' DescribeAutoScalingInstancesType (Maybe Int)
dasitMaxRecords = lens _dasitMaxRecords (\s a -> s { _dasitMaxRecords = a })

-- | The token returned by a previous call to indicate that there is more data
-- available.
dasitNextToken :: Lens' DescribeAutoScalingInstancesType (Maybe Text)
dasitNextToken = lens _dasitNextToken (\s a -> s { _dasitNextToken = a })

instance ToQuery DescribeAutoScalingInstancesType

instance ToPath DescribeAutoScalingInstancesType where
    toPath = const "/"

data AutoScalingInstancesType = AutoScalingInstancesType
    { _asitAutoScalingInstances :: [AutoScalingInstanceDetails]
    , _asitNextToken            :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'AutoScalingInstancesType' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'asitAutoScalingInstances' @::@ ['AutoScalingInstanceDetails']
--
-- * 'asitNextToken' @::@ 'Maybe' 'Text'
--
autoScalingInstancesType :: AutoScalingInstancesType
autoScalingInstancesType = AutoScalingInstancesType
    { _asitAutoScalingInstances = mempty
    , _asitNextToken            = Nothing
    }

-- | A list of Auto Scaling instances.
asitAutoScalingInstances :: Lens' AutoScalingInstancesType [AutoScalingInstanceDetails]
asitAutoScalingInstances =
    lens _asitAutoScalingInstances
        (\s a -> s { _asitAutoScalingInstances = a })

-- | A string that marks the start of the next batch of returned results.
asitNextToken :: Lens' AutoScalingInstancesType (Maybe Text)
asitNextToken = lens _asitNextToken (\s a -> s { _asitNextToken = a })

instance FromXML AutoScalingInstancesType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AutoScalingInstancesType"

instance AWSRequest DescribeAutoScalingInstancesType where
    type Sv DescribeAutoScalingInstancesType = AutoScaling
    type Rs DescribeAutoScalingInstancesType = AutoScalingInstancesType

    request  = post "DescribeAutoScalingInstances"
    response = xmlResponse $ \h x -> AutoScalingInstancesType
        <$> x %| "AutoScalingInstances"
        <*> x %| "NextToken"
