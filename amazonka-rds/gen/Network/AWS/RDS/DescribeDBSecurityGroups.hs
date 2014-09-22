{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
-- security group. https://rds.amazonaws.com/ ?Action=DescribeDBSecurityGroups
-- &Version=2013-05-15 &MaxRecords=100 &SignatureVersion=2
-- &SignatureMethod=HmacSHA256 &Timestamp=2011-02-15T19%3A40%3A19.926Z
-- &AWSAccessKeyId= &Signature= authorized myec2securitygroup 054794666394
-- default 127.0.0.1/30 authorized 621567473609 default vpc-1ab2c3d4 My new
-- DBSecurityGroup 192.168.1.1/24 authorized 621567473609 mydbsecuritygroup
-- vpc-1ab2c3d5 My new DBSecurityGroup 621567473609 mydbsecuritygroup4
-- vpc-1ab2c3d6 bbdad154-bf42-11de-86a4-97241dfaadff.
module Network.AWS.RDS.DescribeDBSecurityGroups
    (
    -- * Request
      DescribeDBSecurityGroups
    -- ** Request constructor
    , describeDBSecurityGroups
    -- ** Request lenses
    , ddbsg2DBSecurityGroupName
    , ddbsg2MaxRecords
    , ddbsg2Marker

    -- * Response
    , DescribeDBSecurityGroupsResponse
    -- ** Response constructor
    , describeDBSecurityGroupsResponse
    -- ** Response lenses
    , ddbsgrMarker
    , ddbsgrDBSecurityGroup
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import Network.AWS.Prelude

-- | 
data DescribeDBSecurityGroups = DescribeDBSecurityGroups
    { _ddbsg2DBSecurityGroupName :: Maybe Text
    , _ddbsg2MaxRecords :: Maybe Integer
    , _ddbsg2Marker :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeDBSecurityGroups' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DBSecurityGroupName ::@ @Maybe Text@
--
-- * @MaxRecords ::@ @Maybe Integer@
--
-- * @Marker ::@ @Maybe Text@
--
describeDBSecurityGroups :: DescribeDBSecurityGroups
describeDBSecurityGroups = DescribeDBSecurityGroups
    { _ddbsg2DBSecurityGroupName = Nothing
    , _ddbsg2MaxRecords = Nothing
    , _ddbsg2Marker = Nothing
    }

-- | The name of the DB security group to return details for.
ddbsg2DBSecurityGroupName :: Lens' DescribeDBSecurityGroups (Maybe Text)
ddbsg2DBSecurityGroupName =
    lens _ddbsg2DBSecurityGroupName
         (\s a -> s { _ddbsg2DBSecurityGroupName = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a pagination token called a
-- marker is included in the response so that the remaining results may be
-- retrieved. Default: 100 Constraints: minimum 20, maximum 100.
ddbsg2MaxRecords :: Lens' DescribeDBSecurityGroups (Maybe Integer)
ddbsg2MaxRecords =
    lens _ddbsg2MaxRecords (\s a -> s { _ddbsg2MaxRecords = a })

-- | An optional pagination token provided by a previous
-- DescribeDBSecurityGroups request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value specified
-- by MaxRecords.
ddbsg2Marker :: Lens' DescribeDBSecurityGroups (Maybe Text)
ddbsg2Marker = lens _ddbsg2Marker (\s a -> s { _ddbsg2Marker = a })

instance ToQuery DescribeDBSecurityGroups where
    toQuery = genericQuery def

-- | Contains the result of a successful invocation of the
-- DescribeDBSecurityGroups action.
data DescribeDBSecurityGroupsResponse = DescribeDBSecurityGroupsResponse
    { _ddbsgrMarker :: Maybe Text
    , _ddbsgrDBSecurityGroup :: [DBSecurityGroup]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeDBSecurityGroupsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Marker ::@ @Maybe Text@
--
-- * @DBSecurityGroup ::@ @[DBSecurityGroup]@
--
describeDBSecurityGroupsResponse :: DescribeDBSecurityGroupsResponse
describeDBSecurityGroupsResponse = DescribeDBSecurityGroupsResponse
    { _ddbsgrMarker = Nothing
    , _ddbsgrDBSecurityGroup = mempty
    }

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by MaxRecords.
ddbsgrMarker :: Lens' DescribeDBSecurityGroupsResponse (Maybe Text)
ddbsgrMarker = lens _ddbsgrMarker (\s a -> s { _ddbsgrMarker = a })

-- | A list of DBSecurityGroup instances.
ddbsgrDBSecurityGroup :: Lens' DescribeDBSecurityGroupsResponse [DBSecurityGroup]
ddbsgrDBSecurityGroup =
    lens _ddbsgrDBSecurityGroup (\s a -> s { _ddbsgrDBSecurityGroup = a })

instance FromXML DescribeDBSecurityGroupsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeDBSecurityGroups where
    type Sv DescribeDBSecurityGroups = RDS
    type Rs DescribeDBSecurityGroups = DescribeDBSecurityGroupsResponse

    request = post "DescribeDBSecurityGroups"
    response _ = xmlResponse

instance AWSPager DescribeDBSecurityGroups where
    next rq rs = (\x -> rq & ddbsg2Marker ?~ x)
        <$> (rs ^. ddbsgrMarker)
