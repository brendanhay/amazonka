{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.V2013_09_09.DescribeDBSecurityGroups
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
-- security group. https://rds.amazonaws.com/ ?Action=DescribeDBSecurityGroups
-- &Version=2013-05-15 &MaxRecords=100 &SignatureVersion=2
-- &SignatureMethod=HmacSHA256 &Timestamp=2011-02-15T19%3A40%3A19.926Z
-- &AWSAccessKeyId= &Signature= authorized myec2securitygroup 054794666394
-- default 127.0.0.1/30 authorized 621567473609 default vpc-1ab2c3d4 My new
-- DBSecurityGroup 192.168.1.1/24 authorized 621567473609 mydbsecuritygroup
-- vpc-1ab2c3d5 My new DBSecurityGroup 621567473609 mydbsecuritygroup4
-- vpc-1ab2c3d6 bbdad154-bf42-11de-86a4-97241dfaadff.
module Network.AWS.RDS.V2013_09_09.DescribeDBSecurityGroups where

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

-- | Minimum specification for a 'DescribeDBSecurityGroups' request.
describeDBSecurityGroups :: DescribeDBSecurityGroups
describeDBSecurityGroups = DescribeDBSecurityGroups
    { _ddbsgpMaxRecords = Nothing
    , _ddbsgpMarker = Nothing
    , _ddbsgpDBSecurityGroupName = Nothing
    }

data DescribeDBSecurityGroups = DescribeDBSecurityGroups
    { _ddbsgpMaxRecords :: Maybe Integer
      -- ^ The maximum number of records to include in the response. If more
      -- records exist than the specified MaxRecords value, a pagination
      -- token called a marker is included in the response so that the
      -- remaining results may be retrieved. Default: 100 Constraints:
      -- minimum 20, maximum 100.
    , _ddbsgpMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous
      -- DescribeDBSecurityGroups request. If this parameter is specified,
      -- the response includes only records beyond the marker, up to the
      -- value specified by MaxRecords.
    , _ddbsgpDBSecurityGroupName :: Maybe Text
      -- ^ The name of the DB security group to return details for.
    } deriving (Generic)

instance ToQuery DescribeDBSecurityGroups where
    toQuery = genericToQuery def

instance AWSRequest DescribeDBSecurityGroups where
    type Sv DescribeDBSecurityGroups = RDS
    type Rs DescribeDBSecurityGroups = DescribeDBSecurityGroupsResponse

    request = post "DescribeDBSecurityGroups"
    response _ = xmlResponse

instance AWSPager DescribeDBSecurityGroups where
    next rq rs = (\x -> rq { _ddbsgpMarker = Just x })
        <$> _dbsgdeMarker rs

data DescribeDBSecurityGroupsResponse = DescribeDBSecurityGroupsResponse
    { _dbsgdeDBSecurityGroups :: [DBSecurityGroup]
      -- ^ A list of DBSecurityGroup instances.
    , _dbsgdeMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous request. If
      -- this parameter is specified, the response includes only records
      -- beyond the marker, up to the value specified by MaxRecords.
    } deriving (Generic)

instance FromXML DescribeDBSecurityGroupsResponse where
    fromXMLOptions = xmlOptions
