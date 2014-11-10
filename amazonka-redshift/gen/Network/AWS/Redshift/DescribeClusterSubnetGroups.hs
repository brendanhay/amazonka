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

-- Module      : Network.AWS.Redshift.DescribeClusterSubnetGroups
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns one or more cluster subnet group objects, which contain metadata
-- about your cluster subnet groups. By default, this operation returns
-- information about all cluster subnet groups that are defined in you AWS
-- account.
module Network.AWS.Redshift.DescribeClusterSubnetGroups
    (
    -- * Request
      DescribeClusterSubnetGroupsMessage
    -- ** Request constructor
    , describeClusterSubnetGroups
    -- ** Request lenses
    , dcsgm1ClusterSubnetGroupName
    , dcsgm1Marker
    , dcsgm1MaxRecords

    -- * Response
    , ClusterSubnetGroupMessage
    -- ** Response constructor
    , describeClusterSubnetGroupsResponse
    -- ** Response lenses
    , csgmClusterSubnetGroups
    , csgmMarker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types

data DescribeClusterSubnetGroupsMessage = DescribeClusterSubnetGroupsMessage
    { _dcsgm1ClusterSubnetGroupName :: Maybe Text
    , _dcsgm1Marker                 :: Maybe Text
    , _dcsgm1MaxRecords             :: Maybe Int
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeClusterSubnetGroupsMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcsgm1ClusterSubnetGroupName' @::@ 'Maybe' 'Text'
--
-- * 'dcsgm1Marker' @::@ 'Maybe' 'Text'
--
-- * 'dcsgm1MaxRecords' @::@ 'Maybe' 'Int'
--
describeClusterSubnetGroups :: DescribeClusterSubnetGroupsMessage
describeClusterSubnetGroups = DescribeClusterSubnetGroupsMessage
    { _dcsgm1ClusterSubnetGroupName = Nothing
    , _dcsgm1MaxRecords             = Nothing
    , _dcsgm1Marker                 = Nothing
    }

-- | The name of the cluster subnet group for which information is requested.
dcsgm1ClusterSubnetGroupName :: Lens' DescribeClusterSubnetGroupsMessage (Maybe Text)
dcsgm1ClusterSubnetGroupName =
    lens _dcsgm1ClusterSubnetGroupName
        (\s a -> s { _dcsgm1ClusterSubnetGroupName = a })

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeClusterSubnetGroups
-- request exceed the value specified in MaxRecords, AWS returns a value in
-- the Marker field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the Marker
-- parameter and retrying the request.
dcsgm1Marker :: Lens' DescribeClusterSubnetGroupsMessage (Maybe Text)
dcsgm1Marker = lens _dcsgm1Marker (\s a -> s { _dcsgm1Marker = a })

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified MaxRecords
-- value, a value is returned in a marker field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value. Default: 100 Constraints: minimum 20, maximum 100.
dcsgm1MaxRecords :: Lens' DescribeClusterSubnetGroupsMessage (Maybe Int)
dcsgm1MaxRecords = lens _dcsgm1MaxRecords (\s a -> s { _dcsgm1MaxRecords = a })

instance ToPath DescribeClusterSubnetGroupsMessage where
    toPath = const "/"

instance ToQuery DescribeClusterSubnetGroupsMessage

data ClusterSubnetGroupMessage = ClusterSubnetGroupMessage
    { _csgmClusterSubnetGroups :: [ClusterSubnetGroup]
    , _csgmMarker              :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'ClusterSubnetGroupMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csgmClusterSubnetGroups' @::@ ['ClusterSubnetGroup']
--
-- * 'csgmMarker' @::@ 'Maybe' 'Text'
--
describeClusterSubnetGroupsResponse :: ClusterSubnetGroupMessage
describeClusterSubnetGroupsResponse = ClusterSubnetGroupMessage
    { _csgmMarker              = Nothing
    , _csgmClusterSubnetGroups = mempty
    }

-- | A list of ClusterSubnetGroup instances.
csgmClusterSubnetGroups :: Lens' ClusterSubnetGroupMessage [ClusterSubnetGroup]
csgmClusterSubnetGroups =
    lens _csgmClusterSubnetGroups (\s a -> s { _csgmClusterSubnetGroups = a })

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the Marker parameter and retrying the command. If the
-- Marker field is empty, all response records have been retrieved for the
-- request.
csgmMarker :: Lens' ClusterSubnetGroupMessage (Maybe Text)
csgmMarker = lens _csgmMarker (\s a -> s { _csgmMarker = a })

instance AWSRequest DescribeClusterSubnetGroupsMessage where
    type Sv DescribeClusterSubnetGroupsMessage = Redshift
    type Rs DescribeClusterSubnetGroupsMessage = ClusterSubnetGroupMessage

    request  = post "DescribeClusterSubnetGroups"
    response = xmlResponse $ \h x -> ClusterSubnetGroupMessage
        <$> x %| "ClusterSubnetGroups"
        <*> x %| "Marker"
