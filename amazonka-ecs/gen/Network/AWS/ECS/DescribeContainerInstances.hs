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

-- Module      : Network.AWS.ECS.DescribeContainerInstances
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

-- | Describes Amazon EC2 Container Service container instances. Returns metadata
-- about registered and remaining resources on each container instance requested.
--
-- <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_DescribeContainerInstances.html>
module Network.AWS.ECS.DescribeContainerInstances
    (
    -- * Request
      DescribeContainerInstances
    -- ** Request constructor
    , describeContainerInstances
    -- ** Request lenses
    , dciCluster
    , dciContainerInstances

    -- * Response
    , DescribeContainerInstancesResponse
    -- ** Response constructor
    , describeContainerInstancesResponse
    -- ** Response lenses
    , dcirContainerInstances
    , dcirFailures
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ECS.Types
import qualified GHC.Exts

data DescribeContainerInstances = DescribeContainerInstances
    { _dciCluster            :: Maybe Text
    , _dciContainerInstances :: List "member" Text
    } deriving (Eq, Ord, Read, Show)

-- | 'DescribeContainerInstances' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dciCluster' @::@ 'Maybe' 'Text'
--
-- * 'dciContainerInstances' @::@ ['Text']
--
describeContainerInstances :: DescribeContainerInstances
describeContainerInstances = DescribeContainerInstances
    { _dciCluster            = Nothing
    , _dciContainerInstances = mempty
    }

-- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts
-- the container instances you want to describe. If you do not specify a
-- cluster, the default cluster is assumed.
dciCluster :: Lens' DescribeContainerInstances (Maybe Text)
dciCluster = lens _dciCluster (\s a -> s { _dciCluster = a })

-- | A space-separated list of container instance UUIDs or full Amazon Resource
-- Name (ARN) entries.
dciContainerInstances :: Lens' DescribeContainerInstances [Text]
dciContainerInstances =
    lens _dciContainerInstances (\s a -> s { _dciContainerInstances = a })
        . _List

data DescribeContainerInstancesResponse = DescribeContainerInstancesResponse
    { _dcirContainerInstances :: List "member" ContainerInstance
    , _dcirFailures           :: List "member" Failure
    } deriving (Eq, Read, Show)

-- | 'DescribeContainerInstancesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcirContainerInstances' @::@ ['ContainerInstance']
--
-- * 'dcirFailures' @::@ ['Failure']
--
describeContainerInstancesResponse :: DescribeContainerInstancesResponse
describeContainerInstancesResponse = DescribeContainerInstancesResponse
    { _dcirContainerInstances = mempty
    , _dcirFailures           = mempty
    }

-- | The list of container instances.
dcirContainerInstances :: Lens' DescribeContainerInstancesResponse [ContainerInstance]
dcirContainerInstances =
    lens _dcirContainerInstances (\s a -> s { _dcirContainerInstances = a })
        . _List

dcirFailures :: Lens' DescribeContainerInstancesResponse [Failure]
dcirFailures = lens _dcirFailures (\s a -> s { _dcirFailures = a }) . _List

instance ToPath DescribeContainerInstances where
    toPath = const "/"

instance ToQuery DescribeContainerInstances where
    toQuery DescribeContainerInstances{..} = mconcat
        [ "cluster"            =? _dciCluster
        , "containerInstances" =? _dciContainerInstances
        ]

instance ToHeaders DescribeContainerInstances

instance AWSRequest DescribeContainerInstances where
    type Sv DescribeContainerInstances = ECS
    type Rs DescribeContainerInstances = DescribeContainerInstancesResponse

    request  = post "DescribeContainerInstances"
    response = xmlResponse

instance FromXML DescribeContainerInstancesResponse where
    parseXML = withElement "DescribeContainerInstancesResult" $ \x -> DescribeContainerInstancesResponse
        <$> x .@? "containerInstances" .!@ mempty
        <*> x .@? "failures" .!@ mempty
