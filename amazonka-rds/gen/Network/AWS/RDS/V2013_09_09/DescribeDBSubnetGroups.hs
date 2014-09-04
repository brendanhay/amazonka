{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.V2013_09_09.DescribeDBSubnetGroups
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
module Network.AWS.RDS.V2013_09_09.DescribeDBSubnetGroups
    (
    -- * Request
      DescribeDBSubnetGroups
    -- ** Request constructor
    , describeDBSubnetGroups
    -- ** Request lenses
    , ddbsgpMaxRecords
    , ddbsgpDBSubnetGroupName
    , ddbsgpMarker

    -- * Response
    , DescribeDBSubnetGroupsResponse
    -- ** Response lenses
    , dbsgsDBSubnetGroups
    , dbsgsMarker
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeDBSubnetGroups' request.
describeDBSubnetGroups :: DescribeDBSubnetGroups
describeDBSubnetGroups = DescribeDBSubnetGroups
    { _ddbsgpMaxRecords = Nothing
    , _ddbsgpDBSubnetGroupName = Nothing
    , _ddbsgpMarker = Nothing
    }
{-# INLINE describeDBSubnetGroups #-}

data DescribeDBSubnetGroups = DescribeDBSubnetGroups
    { _ddbsgpMaxRecords :: Maybe Integer
      -- ^ The maximum number of records to include in the response. If more
      -- records exist than the specified MaxRecords value, a pagination
      -- token called a marker is included in the response so that the
      -- remaining results may be retrieved. Default: 100 Constraints:
      -- minimum 20, maximum 100.
    , _ddbsgpDBSubnetGroupName :: Maybe Text
      -- ^ The name of the DB subnet group to return details for.
    , _ddbsgpMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous
      -- DescribeDBSubnetGroups request. If this parameter is specified,
      -- the response includes only records beyond the marker, up to the
      -- value specified by MaxRecords.
    } deriving (Show, Generic)

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a pagination token called a
-- marker is included in the response so that the remaining results may be
-- retrieved. Default: 100 Constraints: minimum 20, maximum 100.
ddbsgpMaxRecords :: Lens' DescribeDBSubnetGroups (Maybe Integer)
ddbsgpMaxRecords f x =
    f (_ddbsgpMaxRecords x)
        <&> \y -> x { _ddbsgpMaxRecords = y }
{-# INLINE ddbsgpMaxRecords #-}

-- | The name of the DB subnet group to return details for.
ddbsgpDBSubnetGroupName :: Lens' DescribeDBSubnetGroups (Maybe Text)
ddbsgpDBSubnetGroupName f x =
    f (_ddbsgpDBSubnetGroupName x)
        <&> \y -> x { _ddbsgpDBSubnetGroupName = y }
{-# INLINE ddbsgpDBSubnetGroupName #-}

-- | An optional pagination token provided by a previous DescribeDBSubnetGroups
-- request. If this parameter is specified, the response includes only records
-- beyond the marker, up to the value specified by MaxRecords.
ddbsgpMarker :: Lens' DescribeDBSubnetGroups (Maybe Text)
ddbsgpMarker f x =
    f (_ddbsgpMarker x)
        <&> \y -> x { _ddbsgpMarker = y }
{-# INLINE ddbsgpMarker #-}

instance ToQuery DescribeDBSubnetGroups where
    toQuery = genericQuery def

data DescribeDBSubnetGroupsResponse = DescribeDBSubnetGroupsResponse
    { _dbsgsDBSubnetGroups :: [DBSubnetGroup]
      -- ^ A list of DBSubnetGroup instances.
    , _dbsgsMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous request. If
      -- this parameter is specified, the response includes only records
      -- beyond the marker, up to the value specified by MaxRecords.
    } deriving (Show, Generic)

-- | A list of DBSubnetGroup instances.
dbsgsDBSubnetGroups :: Lens' DescribeDBSubnetGroupsResponse ([DBSubnetGroup])
dbsgsDBSubnetGroups f x =
    f (_dbsgsDBSubnetGroups x)
        <&> \y -> x { _dbsgsDBSubnetGroups = y }
{-# INLINE dbsgsDBSubnetGroups #-}

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by MaxRecords.
dbsgsMarker :: Lens' DescribeDBSubnetGroupsResponse (Maybe Text)
dbsgsMarker f x =
    f (_dbsgsMarker x)
        <&> \y -> x { _dbsgsMarker = y }
{-# INLINE dbsgsMarker #-}

instance FromXML DescribeDBSubnetGroupsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeDBSubnetGroups where
    type Sv DescribeDBSubnetGroups = RDS
    type Rs DescribeDBSubnetGroups = DescribeDBSubnetGroupsResponse

    request = post "DescribeDBSubnetGroups"
    response _ = xmlResponse

instance AWSPager DescribeDBSubnetGroups where
    next rq rs = (\x -> rq { _ddbsgpMarker = Just x })
        <$> (_dbsgsMarker rs)
