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

-- Module      : Network.AWS.RDS.DescribeDBSubnetGroups
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of DBSubnetGroup descriptions. If a DBSubnetGroupName is
-- specified, the list will contain only the descriptions of the specified
-- DBSubnetGroup. For an overview of CIDR ranges, go to the Wikipedia
-- Tutorial.
module Network.AWS.RDS.DescribeDBSubnetGroups
    (
    -- * Request
      DescribeDBSubnetGroupsMessage
    -- ** Request constructor
    , describeDBSubnetGroupsMessage
    -- ** Request lenses
    , ddbsgmDBSubnetGroupName
    , ddbsgmFilters
    , ddbsgmMarker
    , ddbsgmMaxRecords

    -- * Response
    , DBSubnetGroupMessage
    -- ** Response constructor
    , dbsubnetGroupMessage
    -- ** Response lenses
    , dbsgm1DBSubnetGroups
    , dbsgm1Marker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types

data DescribeDBSubnetGroupsMessage = DescribeDBSubnetGroupsMessage
    { _ddbsgmDBSubnetGroupName :: Maybe Text
    , _ddbsgmFilters           :: [Filter]
    , _ddbsgmMarker            :: Maybe Text
    , _ddbsgmMaxRecords        :: Maybe Int
    } (Eq, Show, Generic)

-- | 'DescribeDBSubnetGroupsMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddbsgmDBSubnetGroupName' @::@ 'Maybe' 'Text'
--
-- * 'ddbsgmFilters' @::@ ['Filter']
--
-- * 'ddbsgmMarker' @::@ 'Maybe' 'Text'
--
-- * 'ddbsgmMaxRecords' @::@ 'Maybe' 'Int'
--
describeDBSubnetGroupsMessage :: DescribeDBSubnetGroupsMessage
describeDBSubnetGroupsMessage = DescribeDBSubnetGroupsMessage
    { _ddbsgmDBSubnetGroupName = Nothing
    , _ddbsgmFilters           = mempty
    , _ddbsgmMaxRecords        = Nothing
    , _ddbsgmMarker            = Nothing
    }

-- | The name of the DB subnet group to return details for.
ddbsgmDBSubnetGroupName :: Lens' DescribeDBSubnetGroupsMessage (Maybe Text)
ddbsgmDBSubnetGroupName =
    lens _ddbsgmDBSubnetGroupName (\s a -> s { _ddbsgmDBSubnetGroupName = a })

-- | This parameter is not currently supported.
ddbsgmFilters :: Lens' DescribeDBSubnetGroupsMessage [Filter]
ddbsgmFilters = lens _ddbsgmFilters (\s a -> s { _ddbsgmFilters = a })

-- | An optional pagination token provided by a previous
-- DescribeDBSubnetGroups request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by MaxRecords.
ddbsgmMarker :: Lens' DescribeDBSubnetGroupsMessage (Maybe Text)
ddbsgmMarker = lens _ddbsgmMarker (\s a -> s { _ddbsgmMarker = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a pagination token called a
-- marker is included in the response so that the remaining results may be
-- retrieved. Default: 100 Constraints: minimum 20, maximum 100.
ddbsgmMaxRecords :: Lens' DescribeDBSubnetGroupsMessage (Maybe Int)
ddbsgmMaxRecords = lens _ddbsgmMaxRecords (\s a -> s { _ddbsgmMaxRecords = a })
instance ToQuery DescribeDBSubnetGroupsMessage

instance ToPath DescribeDBSubnetGroupsMessage where
    toPath = const "/"

data DBSubnetGroupMessage = DBSubnetGroupMessage
    { _dbsgm1DBSubnetGroups :: [DBSubnetGroup]
    , _dbsgm1Marker         :: Maybe Text
    } (Eq, Show, Generic)

-- | 'DBSubnetGroupMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dbsgm1DBSubnetGroups' @::@ ['DBSubnetGroup']
--
-- * 'dbsgm1Marker' @::@ 'Maybe' 'Text'
--
dbsubnetGroupMessage :: DBSubnetGroupMessage
dbsubnetGroupMessage = DBSubnetGroupMessage
    { _dbsgm1Marker         = Nothing
    , _dbsgm1DBSubnetGroups = mempty
    }

-- | A list of DBSubnetGroup instances.
dbsgm1DBSubnetGroups :: Lens' DBSubnetGroupMessage [DBSubnetGroup]
dbsgm1DBSubnetGroups =
    lens _dbsgm1DBSubnetGroups (\s a -> s { _dbsgm1DBSubnetGroups = a })

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by MaxRecords.
dbsgm1Marker :: Lens' DBSubnetGroupMessage (Maybe Text)
dbsgm1Marker = lens _dbsgm1Marker (\s a -> s { _dbsgm1Marker = a })

instance FromXML DBSubnetGroupMessage where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DBSubnetGroupMessage"

instance AWSRequest DescribeDBSubnetGroupsMessage where
    type Sv DescribeDBSubnetGroupsMessage = RDS
    type Rs DescribeDBSubnetGroupsMessage = DBSubnetGroupMessage

    request  = post "DescribeDBSubnetGroups"
    response = xmlResponse $ \h x -> DBSubnetGroupMessage
        <$> x %| "DBSubnetGroups"
        <*> x %| "Marker"
