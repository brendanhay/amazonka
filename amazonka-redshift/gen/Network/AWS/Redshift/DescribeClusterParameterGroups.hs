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

-- Module      : Network.AWS.Redshift.DescribeClusterParameterGroups
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of Amazon Redshift parameter groups, including parameter
-- groups you created and the default parameter group. For each parameter
-- group, the response includes the parameter group name, description, and
-- parameter group family name. You can optionally specify a name to retrieve
-- the description of a specific parameter group. For more information about
-- managing parameter groups, go to Amazon Redshift Parameter Groups in the
-- Amazon Redshift Management Guide.
module Network.AWS.Redshift.DescribeClusterParameterGroups
    (
    -- * Request
      DescribeClusterParameterGroupsMessage
    -- ** Request constructor
    , describeClusterParameterGroups
    -- ** Request lenses
    , dcpgm1Marker
    , dcpgm1MaxRecords
    , dcpgm1ParameterGroupName

    -- * Response
    , ClusterParameterGroupsMessage
    -- ** Response constructor
    , describeClusterParameterGroupsResponse
    -- ** Response lenses
    , cpgmMarker
    , cpgmParameterGroups
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types

data DescribeClusterParameterGroupsMessage = DescribeClusterParameterGroupsMessage
    { _dcpgm1Marker             :: Maybe Text
    , _dcpgm1MaxRecords         :: Maybe Int
    , _dcpgm1ParameterGroupName :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeClusterParameterGroupsMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcpgm1Marker' @::@ 'Maybe' 'Text'
--
-- * 'dcpgm1MaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'dcpgm1ParameterGroupName' @::@ 'Maybe' 'Text'
--
describeClusterParameterGroups :: DescribeClusterParameterGroupsMessage
describeClusterParameterGroups = DescribeClusterParameterGroupsMessage
    { _dcpgm1ParameterGroupName = Nothing
    , _dcpgm1MaxRecords         = Nothing
    , _dcpgm1Marker             = Nothing
    }

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeClusterParameterGroups
-- request exceed the value specified in MaxRecords, AWS returns a value in
-- the Marker field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the Marker
-- parameter and retrying the request.
dcpgm1Marker :: Lens' DescribeClusterParameterGroupsMessage (Maybe Text)
dcpgm1Marker = lens _dcpgm1Marker (\s a -> s { _dcpgm1Marker = a })

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified MaxRecords
-- value, a value is returned in a marker field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value. Default: 100 Constraints: minimum 20, maximum 100.
dcpgm1MaxRecords :: Lens' DescribeClusterParameterGroupsMessage (Maybe Int)
dcpgm1MaxRecords = lens _dcpgm1MaxRecords (\s a -> s { _dcpgm1MaxRecords = a })

-- | The name of a specific parameter group for which to return details. By
-- default, details about all parameter groups and the default parameter
-- group are returned.
dcpgm1ParameterGroupName :: Lens' DescribeClusterParameterGroupsMessage (Maybe Text)
dcpgm1ParameterGroupName =
    lens _dcpgm1ParameterGroupName
        (\s a -> s { _dcpgm1ParameterGroupName = a })

instance ToQuery DescribeClusterParameterGroupsMessage

instance ToPath DescribeClusterParameterGroupsMessage where
    toPath = const "/"

data ClusterParameterGroupsMessage = ClusterParameterGroupsMessage
    { _cpgmMarker          :: Maybe Text
    , _cpgmParameterGroups :: [ClusterParameterGroup]
    } deriving (Eq, Show, Generic)

-- | 'ClusterParameterGroupsMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpgmMarker' @::@ 'Maybe' 'Text'
--
-- * 'cpgmParameterGroups' @::@ ['ClusterParameterGroup']
--
describeClusterParameterGroupsResponse :: ClusterParameterGroupsMessage
describeClusterParameterGroupsResponse = ClusterParameterGroupsMessage
    { _cpgmMarker          = Nothing
    , _cpgmParameterGroups = mempty
    }

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the Marker parameter and retrying the command. If the
-- Marker field is empty, all response records have been retrieved for the
-- request.
cpgmMarker :: Lens' ClusterParameterGroupsMessage (Maybe Text)
cpgmMarker = lens _cpgmMarker (\s a -> s { _cpgmMarker = a })

-- | A list of ClusterParameterGroup instances. Each instance describes one
-- cluster parameter group.
cpgmParameterGroups :: Lens' ClusterParameterGroupsMessage [ClusterParameterGroup]
cpgmParameterGroups =
    lens _cpgmParameterGroups (\s a -> s { _cpgmParameterGroups = a })

instance FromXML ClusterParameterGroupsMessage where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ClusterParameterGroupsMessage"

instance AWSRequest DescribeClusterParameterGroupsMessage where
    type Sv DescribeClusterParameterGroupsMessage = Redshift
    type Rs DescribeClusterParameterGroupsMessage = ClusterParameterGroupsMessage

    request  = post "DescribeClusterParameterGroups"
    response = xmlResponse $ \h x -> ClusterParameterGroupsMessage
        <$> x %| "Marker"
        <*> x %| "ParameterGroups"
