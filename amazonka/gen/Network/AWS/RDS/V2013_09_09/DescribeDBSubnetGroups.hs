{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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
module Network.AWS.RDS.V2013_09_09.DescribeDBSubnetGroups where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error, Endpoint, Region)
import           Network.AWS.Request.Query
import           Network.AWS.RDS.V2013_09_09.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

-- | Minimum specification for a 'DescribeDBSubnetGroups' request.
describeDBSubnetGroups :: DescribeDBSubnetGroups
describeDBSubnetGroups = DescribeDBSubnetGroups
    { _ddbsgmMaxRecords = Nothing
    , _ddbsgmDBSubnetGroupName = Nothing
    , _ddbsgmMarker = Nothing
    }

data DescribeDBSubnetGroups = DescribeDBSubnetGroups
    { _ddbsgmMaxRecords :: Maybe Integer
      -- ^ The maximum number of records to include in the response. If more
      -- records exist than the specified MaxRecords value, a pagination
      -- token called a marker is included in the response so that the
      -- remaining results may be retrieved. Default: 100 Constraints:
      -- minimum 20, maximum 100.
    , _ddbsgmDBSubnetGroupName :: Maybe Text
      -- ^ The name of the DB subnet group to return details for.
    , _ddbsgmMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous
      -- DescribeDBSubnetGroups request. If this parameter is specified,
      -- the response includes only records beyond the marker, up to the
      -- value specified by MaxRecords.
    } deriving (Generic)

instance ToQuery DescribeDBSubnetGroups where
    toQuery = genericToQuery def

instance AWSRequest DescribeDBSubnetGroups where
    type Sv DescribeDBSubnetGroups = RDS
    type Rs DescribeDBSubnetGroups = DescribeDBSubnetGroupsResponse

    request = post "DescribeDBSubnetGroups"
    response _ = xmlResponse

instance AWSPager DescribeDBSubnetGroups where
    next rq rs = (\x -> rq { _ddbsgmMarker = Just x })
        <$> _dbsgrMarker rs

data DescribeDBSubnetGroupsResponse = DescribeDBSubnetGroupsResponse
    { _dbsgrDBSubnetGroups :: [DBSubnetGroup]
      -- ^ A list of DBSubnetGroup instances.
    , _dbsgrMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous request. If
      -- this parameter is specified, the response includes only records
      -- beyond the marker, up to the value specified by MaxRecords.
    } deriving (Generic)

instance FromXML DescribeDBSubnetGroupsResponse where
    fromXMLOptions = xmlOptions
