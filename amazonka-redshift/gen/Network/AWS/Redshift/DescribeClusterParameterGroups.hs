{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

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
      DescribeClusterParameterGroups
    -- ** Request constructor
    , describeClusterParameterGroups
    -- ** Request lenses
    , dcpgMarker
    , dcpgMaxRecords
    , dcpgParameterGroupName

    -- * Response
    , DescribeClusterParameterGroupsResponse
    -- ** Response constructor
    , describeClusterParameterGroupsResponse
    -- ** Response lenses
    , dcpgrMarker
    , dcpgrParameterGroups
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types
import qualified GHC.Exts

data DescribeClusterParameterGroups = DescribeClusterParameterGroups
    { _dcpgMarker             :: Maybe Text
    , _dcpgMaxRecords         :: Maybe Int
    , _dcpgParameterGroupName :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeClusterParameterGroups' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcpgMarker' @::@ 'Maybe' 'Text'
--
-- * 'dcpgMaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'dcpgParameterGroupName' @::@ 'Maybe' 'Text'
--
describeClusterParameterGroups :: DescribeClusterParameterGroups
describeClusterParameterGroups = DescribeClusterParameterGroups
    { _dcpgParameterGroupName = Nothing
    , _dcpgMaxRecords         = Nothing
    , _dcpgMarker             = Nothing
    }

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeClusterParameterGroups
-- request exceed the value specified in MaxRecords, AWS returns a value in
-- the Marker field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the Marker
-- parameter and retrying the request.
dcpgMarker :: Lens' DescribeClusterParameterGroups (Maybe Text)
dcpgMarker = lens _dcpgMarker (\s a -> s { _dcpgMarker = a })

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified MaxRecords
-- value, a value is returned in a marker field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value. Default: 100 Constraints: minimum 20, maximum 100.
dcpgMaxRecords :: Lens' DescribeClusterParameterGroups (Maybe Int)
dcpgMaxRecords = lens _dcpgMaxRecords (\s a -> s { _dcpgMaxRecords = a })

-- | The name of a specific parameter group for which to return details. By
-- default, details about all parameter groups and the default parameter
-- group are returned.
dcpgParameterGroupName :: Lens' DescribeClusterParameterGroups (Maybe Text)
dcpgParameterGroupName =
    lens _dcpgParameterGroupName (\s a -> s { _dcpgParameterGroupName = a })

instance ToQuery DescribeClusterParameterGroups

instance ToPath DescribeClusterParameterGroups where
    toPath = const "/"

data DescribeClusterParameterGroupsResponse = DescribeClusterParameterGroupsResponse
    { _dcpgrMarker          :: Maybe Text
    , _dcpgrParameterGroups :: [ClusterParameterGroup]
    } deriving (Eq, Show, Generic)

-- | 'DescribeClusterParameterGroupsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcpgrMarker' @::@ 'Maybe' 'Text'
--
-- * 'dcpgrParameterGroups' @::@ ['ClusterParameterGroup']
--
describeClusterParameterGroupsResponse :: DescribeClusterParameterGroupsResponse
describeClusterParameterGroupsResponse = DescribeClusterParameterGroupsResponse
    { _dcpgrMarker          = Nothing
    , _dcpgrParameterGroups = mempty
    }

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the Marker parameter and retrying the command. If the
-- Marker field is empty, all response records have been retrieved for the
-- request.
dcpgrMarker :: Lens' DescribeClusterParameterGroupsResponse (Maybe Text)
dcpgrMarker = lens _dcpgrMarker (\s a -> s { _dcpgrMarker = a })

-- | A list of ClusterParameterGroup instances. Each instance describes one
-- cluster parameter group.
dcpgrParameterGroups :: Lens' DescribeClusterParameterGroupsResponse [ClusterParameterGroup]
dcpgrParameterGroups =
    lens _dcpgrParameterGroups (\s a -> s { _dcpgrParameterGroups = a })

instance AWSRequest DescribeClusterParameterGroups where
    type Sv DescribeClusterParameterGroups = Redshift
    type Rs DescribeClusterParameterGroups = DescribeClusterParameterGroupsResponse

    request  = post "DescribeClusterParameterGroups"
    response = xmlResponse $ \h x -> DescribeClusterParameterGroupsResponse
        <$> x %| "Marker"
        <*> x %| "ParameterGroups"
