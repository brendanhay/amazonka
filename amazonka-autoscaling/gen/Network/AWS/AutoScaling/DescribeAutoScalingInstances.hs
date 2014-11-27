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

-- Module      : Network.AWS.AutoScaling.DescribeAutoScalingInstances
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

-- | Describes one or more Auto Scaling instances. If a list is not provided, the
-- call describes all instances.
--
-- You can describe up to a maximum of 50 instances with a single call. By
-- default, a call returns up to 20 instances. If there are more items to
-- return, the call returns a token. To get the next set of items, repeat the
-- call with the returned token in the 'NextToken' parameter.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeAutoScalingInstances.html>
module Network.AWS.AutoScaling.DescribeAutoScalingInstances
    (
    -- * Request
      DescribeAutoScalingInstances
    -- ** Request constructor
    , describeAutoScalingInstances
    -- ** Request lenses
    , dasiInstanceIds
    , dasiMaxRecords
    , dasiNextToken

    -- * Response
    , DescribeAutoScalingInstancesResponse
    -- ** Response constructor
    , describeAutoScalingInstancesResponse
    -- ** Response lenses
    , dasirAutoScalingInstances
    , dasirNextToken
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types
import qualified GHC.Exts

data DescribeAutoScalingInstances = DescribeAutoScalingInstances
    { _dasiInstanceIds :: List "InstanceIds" Text
    , _dasiMaxRecords  :: Maybe Int
    , _dasiNextToken   :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'DescribeAutoScalingInstances' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dasiInstanceIds' @::@ ['Text']
--
-- * 'dasiMaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'dasiNextToken' @::@ 'Maybe' 'Text'
--
describeAutoScalingInstances :: DescribeAutoScalingInstances
describeAutoScalingInstances = DescribeAutoScalingInstances
    { _dasiInstanceIds = mempty
    , _dasiMaxRecords  = Nothing
    , _dasiNextToken   = Nothing
    }

-- | One or more Auto Scaling instances to describe, up to 50 instances. If you
-- omit this parameter, all Auto Scaling instances are described. If you specify
-- an ID that does not exist, it is ignored with no error.
dasiInstanceIds :: Lens' DescribeAutoScalingInstances [Text]
dasiInstanceIds = lens _dasiInstanceIds (\s a -> s { _dasiInstanceIds = a }) . _List

-- | The maximum number of items to return with this call.
dasiMaxRecords :: Lens' DescribeAutoScalingInstances (Maybe Int)
dasiMaxRecords = lens _dasiMaxRecords (\s a -> s { _dasiMaxRecords = a })

-- | The token for the next set of items to return. (You received this token from
-- a previous call.)
dasiNextToken :: Lens' DescribeAutoScalingInstances (Maybe Text)
dasiNextToken = lens _dasiNextToken (\s a -> s { _dasiNextToken = a })

data DescribeAutoScalingInstancesResponse = DescribeAutoScalingInstancesResponse
    { _dasirAutoScalingInstances :: List "AutoScalingInstances" AutoScalingInstanceDetails
    , _dasirNextToken            :: Maybe Text
    } deriving (Eq, Show)

-- | 'DescribeAutoScalingInstancesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dasirAutoScalingInstances' @::@ ['AutoScalingInstanceDetails']
--
-- * 'dasirNextToken' @::@ 'Maybe' 'Text'
--
describeAutoScalingInstancesResponse :: DescribeAutoScalingInstancesResponse
describeAutoScalingInstancesResponse = DescribeAutoScalingInstancesResponse
    { _dasirAutoScalingInstances = mempty
    , _dasirNextToken            = Nothing
    }

-- | The instances.
dasirAutoScalingInstances :: Lens' DescribeAutoScalingInstancesResponse [AutoScalingInstanceDetails]
dasirAutoScalingInstances =
    lens _dasirAutoScalingInstances
        (\s a -> s { _dasirAutoScalingInstances = a })
            . _List

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
dasirNextToken :: Lens' DescribeAutoScalingInstancesResponse (Maybe Text)
dasirNextToken = lens _dasirNextToken (\s a -> s { _dasirNextToken = a })

instance ToPath DescribeAutoScalingInstances where
    toPath = const "/"

instance ToQuery DescribeAutoScalingInstances where
    toQuery DescribeAutoScalingInstances{..} = mconcat
        [ "InstanceIds" =? _dasiInstanceIds
        , "MaxRecords"  =? _dasiMaxRecords
        , "NextToken"   =? _dasiNextToken
        ]

instance ToHeaders DescribeAutoScalingInstances

instance AWSRequest DescribeAutoScalingInstances where
    type Sv DescribeAutoScalingInstances = AutoScaling
    type Rs DescribeAutoScalingInstances = DescribeAutoScalingInstancesResponse

    request  = post "DescribeAutoScalingInstances"
    response = xmlResponse

instance FromXML DescribeAutoScalingInstancesResponse where
    parseXML = withElement "DescribeAutoScalingInstancesResult" $ \x -> DescribeAutoScalingInstancesResponse
        <$> x .@  "AutoScalingInstances"
        <*> x .@? "NextToken"

instance AWSPager DescribeAutoScalingInstances where
    page rq rs
        | stop (rq ^. dasiNextToken) = Nothing
        | otherwise = (\x -> rq & dasiNextToken ?~ x)
            <$> (rs ^. dasirNextToken)
