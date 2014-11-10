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

-- Module      : Network.AWS.RDS.DescribeDBSecurityGroups
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of DBSecurityGroup descriptions. If a DBSecurityGroupName is
-- specified, the list will contain only the descriptions of the specified DB
-- security group.
module Network.AWS.RDS.DescribeDBSecurityGroups
    (
    -- * Request
      DescribeDBSecurityGroupsMessage
    -- ** Request constructor
    , describeDBSecurityGroups
    -- ** Request lenses
    , ddbsgm1DBSecurityGroupName
    , ddbsgm1Filters
    , ddbsgm1Marker
    , ddbsgm1MaxRecords

    -- * Response
    , DBSecurityGroupMessage
    -- ** Response constructor
    , describeDBSecurityGroupsResponse
    -- ** Response lenses
    , dbsgmDBSecurityGroups
    , dbsgmMarker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types

data DescribeDBSecurityGroupsMessage = DescribeDBSecurityGroupsMessage
    { _ddbsgm1DBSecurityGroupName :: Maybe Text
    , _ddbsgm1Filters             :: [Filter]
    , _ddbsgm1Marker              :: Maybe Text
    , _ddbsgm1MaxRecords          :: Maybe Int
    } deriving (Eq, Show, Generic)

-- | 'DescribeDBSecurityGroupsMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddbsgm1DBSecurityGroupName' @::@ 'Maybe' 'Text'
--
-- * 'ddbsgm1Filters' @::@ ['Filter']
--
-- * 'ddbsgm1Marker' @::@ 'Maybe' 'Text'
--
-- * 'ddbsgm1MaxRecords' @::@ 'Maybe' 'Int'
--
describeDBSecurityGroups :: DescribeDBSecurityGroupsMessage
describeDBSecurityGroups = DescribeDBSecurityGroupsMessage
    { _ddbsgm1DBSecurityGroupName = Nothing
    , _ddbsgm1Filters             = mempty
    , _ddbsgm1MaxRecords          = Nothing
    , _ddbsgm1Marker              = Nothing
    }

-- | The name of the DB security group to return details for.
ddbsgm1DBSecurityGroupName :: Lens' DescribeDBSecurityGroupsMessage (Maybe Text)
ddbsgm1DBSecurityGroupName =
    lens _ddbsgm1DBSecurityGroupName
        (\s a -> s { _ddbsgm1DBSecurityGroupName = a })

-- | This parameter is not currently supported.
ddbsgm1Filters :: Lens' DescribeDBSecurityGroupsMessage [Filter]
ddbsgm1Filters = lens _ddbsgm1Filters (\s a -> s { _ddbsgm1Filters = a })

-- | An optional pagination token provided by a previous
-- DescribeDBSecurityGroups request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by MaxRecords.
ddbsgm1Marker :: Lens' DescribeDBSecurityGroupsMessage (Maybe Text)
ddbsgm1Marker = lens _ddbsgm1Marker (\s a -> s { _ddbsgm1Marker = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a pagination token called a
-- marker is included in the response so that the remaining results may be
-- retrieved. Default: 100 Constraints: minimum 20, maximum 100.
ddbsgm1MaxRecords :: Lens' DescribeDBSecurityGroupsMessage (Maybe Int)
ddbsgm1MaxRecords =
    lens _ddbsgm1MaxRecords (\s a -> s { _ddbsgm1MaxRecords = a })

instance ToPath DescribeDBSecurityGroupsMessage where
    toPath = const "/"

instance ToQuery DescribeDBSecurityGroupsMessage

data DBSecurityGroupMessage = DBSecurityGroupMessage
    { _dbsgmDBSecurityGroups :: [DBSecurityGroup]
    , _dbsgmMarker           :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'DBSecurityGroupMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dbsgmDBSecurityGroups' @::@ ['DBSecurityGroup']
--
-- * 'dbsgmMarker' @::@ 'Maybe' 'Text'
--
describeDBSecurityGroupsResponse :: DBSecurityGroupMessage
describeDBSecurityGroupsResponse = DBSecurityGroupMessage
    { _dbsgmMarker           = Nothing
    , _dbsgmDBSecurityGroups = mempty
    }

-- | A list of DBSecurityGroup instances.
dbsgmDBSecurityGroups :: Lens' DBSecurityGroupMessage [DBSecurityGroup]
dbsgmDBSecurityGroups =
    lens _dbsgmDBSecurityGroups (\s a -> s { _dbsgmDBSecurityGroups = a })

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by MaxRecords.
dbsgmMarker :: Lens' DBSecurityGroupMessage (Maybe Text)
dbsgmMarker = lens _dbsgmMarker (\s a -> s { _dbsgmMarker = a })

instance AWSRequest DescribeDBSecurityGroupsMessage where
    type Sv DescribeDBSecurityGroupsMessage = RDS
    type Rs DescribeDBSecurityGroupsMessage = DBSecurityGroupMessage

    request  = post "DescribeDBSecurityGroups"
    response = xmlResponse $ \h x -> DBSecurityGroupMessage
        <$> x %| "DBSecurityGroups"
        <*> x %| "Marker"
