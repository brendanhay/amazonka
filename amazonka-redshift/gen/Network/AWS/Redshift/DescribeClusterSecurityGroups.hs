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

-- Module      : Network.AWS.Redshift.DescribeClusterSecurityGroups
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns information about Amazon Redshift security groups. If the name of a
-- security group is specified, the response will contain only information
-- about only that security group. For information about managing security
-- groups, go to Amazon Redshift Cluster Security Groups in the Amazon
-- Redshift Management Guide.
module Network.AWS.Redshift.DescribeClusterSecurityGroups
    (
    -- * Request
      DescribeClusterSecurityGroupsMessage
    -- ** Request constructor
    , describeClusterSecurityGroups
    -- ** Request lenses
    , dcsgm2ClusterSecurityGroupName
    , dcsgm2Marker
    , dcsgm2MaxRecords

    -- * Response
    , ClusterSecurityGroupMessage
    -- ** Response constructor
    , describeClusterSecurityGroupsResponse
    -- ** Response lenses
    , csgm1ClusterSecurityGroups
    , csgm1Marker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types

data DescribeClusterSecurityGroupsMessage = DescribeClusterSecurityGroupsMessage
    { _dcsgm2ClusterSecurityGroupName :: Maybe Text
    , _dcsgm2Marker                   :: Maybe Text
    , _dcsgm2MaxRecords               :: Maybe Int
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeClusterSecurityGroupsMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcsgm2ClusterSecurityGroupName' @::@ 'Maybe' 'Text'
--
-- * 'dcsgm2Marker' @::@ 'Maybe' 'Text'
--
-- * 'dcsgm2MaxRecords' @::@ 'Maybe' 'Int'
--
describeClusterSecurityGroups :: DescribeClusterSecurityGroupsMessage
describeClusterSecurityGroups = DescribeClusterSecurityGroupsMessage
    { _dcsgm2ClusterSecurityGroupName = Nothing
    , _dcsgm2MaxRecords               = Nothing
    , _dcsgm2Marker                   = Nothing
    }

-- | The name of a cluster security group for which you are requesting
-- details. You can specify either the Marker parameter or a
-- ClusterSecurityGroupName parameter, but not both. Example:
-- securitygroup1.
dcsgm2ClusterSecurityGroupName :: Lens' DescribeClusterSecurityGroupsMessage (Maybe Text)
dcsgm2ClusterSecurityGroupName =
    lens _dcsgm2ClusterSecurityGroupName
        (\s a -> s { _dcsgm2ClusterSecurityGroupName = a })

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeClusterSecurityGroups
-- request exceed the value specified in MaxRecords, AWS returns a value in
-- the Marker field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the Marker
-- parameter and retrying the request. Constraints: You can specify either
-- the ClusterSecurityGroupName parameter or the Marker parameter, but not
-- both.
dcsgm2Marker :: Lens' DescribeClusterSecurityGroupsMessage (Maybe Text)
dcsgm2Marker = lens _dcsgm2Marker (\s a -> s { _dcsgm2Marker = a })

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified MaxRecords
-- value, a value is returned in a marker field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value. Default: 100 Constraints: minimum 20, maximum 100.
dcsgm2MaxRecords :: Lens' DescribeClusterSecurityGroupsMessage (Maybe Int)
dcsgm2MaxRecords = lens _dcsgm2MaxRecords (\s a -> s { _dcsgm2MaxRecords = a })

instance ToQuery DescribeClusterSecurityGroupsMessage

instance ToPath DescribeClusterSecurityGroupsMessage where
    toPath = const "/"

data ClusterSecurityGroupMessage = ClusterSecurityGroupMessage
    { _csgm1ClusterSecurityGroups :: [ClusterSecurityGroup]
    , _csgm1Marker                :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'ClusterSecurityGroupMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csgm1ClusterSecurityGroups' @::@ ['ClusterSecurityGroup']
--
-- * 'csgm1Marker' @::@ 'Maybe' 'Text'
--
describeClusterSecurityGroupsResponse :: ClusterSecurityGroupMessage
describeClusterSecurityGroupsResponse = ClusterSecurityGroupMessage
    { _csgm1Marker                = Nothing
    , _csgm1ClusterSecurityGroups = mempty
    }

-- | A list of ClusterSecurityGroup instances.
csgm1ClusterSecurityGroups :: Lens' ClusterSecurityGroupMessage [ClusterSecurityGroup]
csgm1ClusterSecurityGroups =
    lens _csgm1ClusterSecurityGroups
        (\s a -> s { _csgm1ClusterSecurityGroups = a })

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the Marker parameter and retrying the command. If the
-- Marker field is empty, all response records have been retrieved for the
-- request.
csgm1Marker :: Lens' ClusterSecurityGroupMessage (Maybe Text)
csgm1Marker = lens _csgm1Marker (\s a -> s { _csgm1Marker = a })

instance FromXML ClusterSecurityGroupMessage where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ClusterSecurityGroupMessage"

instance AWSRequest DescribeClusterSecurityGroupsMessage where
    type Sv DescribeClusterSecurityGroupsMessage = Redshift
    type Rs DescribeClusterSecurityGroupsMessage = ClusterSecurityGroupMessage

    request  = post "DescribeClusterSecurityGroups"
    response = xmlResponse $ \h x -> ClusterSecurityGroupMessage
        <$> x %| "ClusterSecurityGroups"
        <*> x %| "Marker"
