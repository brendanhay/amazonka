{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
-- Tutorial. https://rds.amazonaws.com/ ?Action=DescribeDBSubnetGroups
-- &Version=2013-05-15 &MaxRecords=100 &SignatureVersion=2
-- &SignatureMethod=HmacSHA256 &Timestamp=2011-02-15T19%3A40%3A19.926Z
-- &AWSAccessKeyId= &Signature= 990524496922 Complete description subnet_grp1
-- Active subnet-7c5b4115 us-east-1c Active subnet-7b5b4112 us-east-1b Active
-- subnet-3ea6bd57 us-east-1d 990524496922 Complete description subnet_grp2
-- Active subnet-7c5b4115 us-east-1c Active subnet-7b5b4112 us-east-1b Active
-- subnet-3ea6bd57 us-east-1d 31d0faee-229b-11e1-81f1-df3a2a803dad.
module Network.AWS.RDS.DescribeDBSubnetGroups
    (
    -- * Request
      DescribeDBSubnetGroups
    -- ** Request constructor
    , describeDBSubnetGroups
    -- ** Request lenses
    , ddbsg3DBSubnetGroupName
    , ddbsg3MaxRecords
    , ddbsg3Marker

    -- * Response
    , DescribeDBSubnetGroupsResponse
    -- ** Response constructor
    , describeDBSubnetGroupsResponse
    -- ** Response lenses
    , ddbsgrrMarker
    , ddbsgrrDBSubnetGroups
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import Network.AWS.Prelude

-- | 
data DescribeDBSubnetGroups = DescribeDBSubnetGroups
    { _ddbsg3DBSubnetGroupName :: Maybe Text
    , _ddbsg3MaxRecords :: Maybe Integer
    , _ddbsg3Marker :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeDBSubnetGroups' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DBSubnetGroupName ::@ @Maybe Text@
--
-- * @MaxRecords ::@ @Maybe Integer@
--
-- * @Marker ::@ @Maybe Text@
--
describeDBSubnetGroups :: DescribeDBSubnetGroups
describeDBSubnetGroups = DescribeDBSubnetGroups
    { _ddbsg3DBSubnetGroupName = Nothing
    , _ddbsg3MaxRecords = Nothing
    , _ddbsg3Marker = Nothing
    }

-- | The name of the DB subnet group to return details for.
ddbsg3DBSubnetGroupName :: Lens' DescribeDBSubnetGroups (Maybe Text)
ddbsg3DBSubnetGroupName =
    lens _ddbsg3DBSubnetGroupName
         (\s a -> s { _ddbsg3DBSubnetGroupName = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a pagination token called a
-- marker is included in the response so that the remaining results may be
-- retrieved. Default: 100 Constraints: minimum 20, maximum 100.
ddbsg3MaxRecords :: Lens' DescribeDBSubnetGroups (Maybe Integer)
ddbsg3MaxRecords =
    lens _ddbsg3MaxRecords (\s a -> s { _ddbsg3MaxRecords = a })

-- | An optional pagination token provided by a previous DescribeDBSubnetGroups
-- request. If this parameter is specified, the response includes only records
-- beyond the marker, up to the value specified by MaxRecords.
ddbsg3Marker :: Lens' DescribeDBSubnetGroups (Maybe Text)
ddbsg3Marker = lens _ddbsg3Marker (\s a -> s { _ddbsg3Marker = a })

instance ToQuery DescribeDBSubnetGroups where
    toQuery = genericQuery def

-- | Contains the result of a successful invocation of the
-- DescribeDBSubnetGroups action.
data DescribeDBSubnetGroupsResponse = DescribeDBSubnetGroupsResponse
    { _ddbsgrrMarker :: Maybe Text
    , _ddbsgrrDBSubnetGroups :: [DBSubnetGroup]
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeDBSubnetGroupsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Marker ::@ @Maybe Text@
--
-- * @DBSubnetGroups ::@ @[DBSubnetGroup]@
--
describeDBSubnetGroupsResponse :: DescribeDBSubnetGroupsResponse
describeDBSubnetGroupsResponse = DescribeDBSubnetGroupsResponse
    { _ddbsgrrMarker = Nothing
    , _ddbsgrrDBSubnetGroups = mempty
    }

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by MaxRecords.
ddbsgrrMarker :: Lens' DescribeDBSubnetGroupsResponse (Maybe Text)
ddbsgrrMarker = lens _ddbsgrrMarker (\s a -> s { _ddbsgrrMarker = a })

-- | A list of DBSubnetGroup instances.
ddbsgrrDBSubnetGroups :: Lens' DescribeDBSubnetGroupsResponse [DBSubnetGroup]
ddbsgrrDBSubnetGroups =
    lens _ddbsgrrDBSubnetGroups (\s a -> s { _ddbsgrrDBSubnetGroups = a })

instance FromXML DescribeDBSubnetGroupsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeDBSubnetGroups where
    type Sv DescribeDBSubnetGroups = RDS
    type Rs DescribeDBSubnetGroups = DescribeDBSubnetGroupsResponse

    request = post "DescribeDBSubnetGroups"
    response _ = xmlResponse

instance AWSPager DescribeDBSubnetGroups where
    next rq rs = (\x -> rq & ddbsg3Marker ?~ x)
        <$> (rs ^. ddbsgrrMarker)
