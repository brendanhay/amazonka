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

-- Module      : Network.AWS.ECS.DescribeServices
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

-- | Describes the specified services running in your cluster.
--
-- <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_DescribeServices.html>
module Network.AWS.ECS.DescribeServices
    (
    -- * Request
      DescribeServices
    -- ** Request constructor
    , describeServices
    -- ** Request lenses
    , ds1Cluster
    , ds1Services

    -- * Response
    , DescribeServicesResponse
    -- ** Response constructor
    , describeServicesResponse
    -- ** Response lenses
    , dsrFailures
    , dsrServices
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.ECS.Types
import qualified GHC.Exts

data DescribeServices = DescribeServices
    { _ds1Cluster  :: Maybe Text
    , _ds1Services :: List "services" Text
    } deriving (Eq, Ord, Read, Show)

-- | 'DescribeServices' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ds1Cluster' @::@ 'Maybe' 'Text'
--
-- * 'ds1Services' @::@ ['Text']
--
describeServices :: DescribeServices
describeServices = DescribeServices
    { _ds1Cluster  = Nothing
    , _ds1Services = mempty
    }

-- | The name of the cluster that hosts the service you want to describe.
ds1Cluster :: Lens' DescribeServices (Maybe Text)
ds1Cluster = lens _ds1Cluster (\s a -> s { _ds1Cluster = a })

-- | A list of services you want to describe.
ds1Services :: Lens' DescribeServices [Text]
ds1Services = lens _ds1Services (\s a -> s { _ds1Services = a }) . _List

data DescribeServicesResponse = DescribeServicesResponse
    { _dsrFailures :: List "failures" Failure
    , _dsrServices :: List "services" ContainerService
    } deriving (Eq, Read, Show)

-- | 'DescribeServicesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsrFailures' @::@ ['Failure']
--
-- * 'dsrServices' @::@ ['ContainerService']
--
describeServicesResponse :: DescribeServicesResponse
describeServicesResponse = DescribeServicesResponse
    { _dsrServices = mempty
    , _dsrFailures = mempty
    }

-- | Any failures associated with the call.
dsrFailures :: Lens' DescribeServicesResponse [Failure]
dsrFailures = lens _dsrFailures (\s a -> s { _dsrFailures = a }) . _List

-- | The list of services described.
dsrServices :: Lens' DescribeServicesResponse [ContainerService]
dsrServices = lens _dsrServices (\s a -> s { _dsrServices = a }) . _List

instance ToPath DescribeServices where
    toPath = const "/"

instance ToQuery DescribeServices where
    toQuery = const mempty

instance ToHeaders DescribeServices

instance ToJSON DescribeServices where
    toJSON DescribeServices{..} = object
        [ "cluster"  .= _ds1Cluster
        , "services" .= _ds1Services
        ]

instance AWSRequest DescribeServices where
    type Sv DescribeServices = ECS
    type Rs DescribeServices = DescribeServicesResponse

    request  = post "DescribeServices"
    response = jsonResponse

instance FromJSON DescribeServicesResponse where
    parseJSON = withObject "DescribeServicesResponse" $ \o -> DescribeServicesResponse
        <$> o .:? "failures" .!= mempty
        <*> o .:? "services" .!= mempty
