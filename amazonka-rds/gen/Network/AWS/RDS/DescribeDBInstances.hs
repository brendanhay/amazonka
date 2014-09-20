{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.DescribeDBInstances
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns information about provisioned RDS instances. This API supports
-- pagination. https://rds.amazonaws.com/ ?Action=DescribeDBInstances
-- &Version=2013-05-15 &MaxRecords=100 &SignatureVersion=2
-- &SignatureMethod=HmacSHA256 &Timestamp=2011-05-23T06%3A54%3A55.116Z
-- &AWSAccessKeyId= &Signature= 2011-05-23T06:50:00Z mysql 1 false
-- general-public-license available 5.1.50 3306
-- simcoprod01.cu7u2t4uz396.us-east-1.rds.amazonaws.com simcoprod01 in-sync
-- default.mysql5.1 active default 00:00-00:30 true sat:07:30-sat:08:00
-- us-east-1a 2011-05-23T06:06:43.110Z 10 default.mysql5.1 in-sync db.m1.large
-- master 9135fff3-8509-11e0-bd9b-a7b1ece36d51.
module Network.AWS.RDS.DescribeDBInstances
    (
    -- * Request
      DescribeDBInstances
    -- ** Request constructor
    , describeDBInstances
    -- ** Request lenses
    , ddbi1DBInstanceIdentifier
    , ddbi1MaxRecords
    , ddbi1Marker

    -- * Response
    , DescribeDBInstancesResponse
    -- ** Response constructor
    , describeDBInstancesResponse
    -- ** Response lenses
    , ddbirrMarker
    , ddbirrDBInstances
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import Network.AWS.Prelude

-- | 
data DescribeDBInstances = DescribeDBInstances
    { _ddbi1DBInstanceIdentifier :: Maybe Text
    , _ddbi1MaxRecords :: Maybe Integer
    , _ddbi1Marker :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeDBInstances' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DBInstanceIdentifier ::@ @Maybe Text@
--
-- * @MaxRecords ::@ @Maybe Integer@
--
-- * @Marker ::@ @Maybe Text@
--
describeDBInstances :: DescribeDBInstances
describeDBInstances = DescribeDBInstances
    { _ddbi1DBInstanceIdentifier = Nothing
    , _ddbi1MaxRecords = Nothing
    , _ddbi1Marker = Nothing
    }

-- | The user-supplied instance identifier. If this parameter is specified,
-- information from only the specific DB instance is returned. This parameter
-- isn't case sensitive. Constraints: Must contain from 1 to 63 alphanumeric
-- characters or hyphens First character must be a letter Cannot end with a
-- hyphen or contain two consecutive hyphens.
ddbi1DBInstanceIdentifier :: Lens' DescribeDBInstances (Maybe Text)
ddbi1DBInstanceIdentifier =
    lens _ddbi1DBInstanceIdentifier
         (\s a -> s { _ddbi1DBInstanceIdentifier = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a pagination token called a
-- marker is included in the response so that the remaining results may be
-- retrieved. Default: 100 Constraints: minimum 20, maximum 100.
ddbi1MaxRecords :: Lens' DescribeDBInstances (Maybe Integer)
ddbi1MaxRecords = lens _ddbi1MaxRecords (\s a -> s { _ddbi1MaxRecords = a })

-- | An optional pagination token provided by a previous DescribeDBInstances
-- request. If this parameter is specified, the response includes only records
-- beyond the marker, up to the value specified by MaxRecords .
ddbi1Marker :: Lens' DescribeDBInstances (Maybe Text)
ddbi1Marker = lens _ddbi1Marker (\s a -> s { _ddbi1Marker = a })

instance ToQuery DescribeDBInstances where
    toQuery = genericQuery def

-- | Contains the result of a successful invocation of the DescribeDBInstances
-- action.
data DescribeDBInstancesResponse = DescribeDBInstancesResponse
    { _ddbirrMarker :: Maybe Text
    , _ddbirrDBInstances :: [DBInstance]
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeDBInstancesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Marker ::@ @Maybe Text@
--
-- * @DBInstances ::@ @[DBInstance]@
--
describeDBInstancesResponse :: DescribeDBInstancesResponse
describeDBInstancesResponse = DescribeDBInstancesResponse
    { _ddbirrMarker = Nothing
    , _ddbirrDBInstances = mempty
    }

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by MaxRecords .
ddbirrMarker :: Lens' DescribeDBInstancesResponse (Maybe Text)
ddbirrMarker = lens _ddbirrMarker (\s a -> s { _ddbirrMarker = a })

-- | A list of DBInstance instances.
ddbirrDBInstances :: Lens' DescribeDBInstancesResponse [DBInstance]
ddbirrDBInstances =
    lens _ddbirrDBInstances (\s a -> s { _ddbirrDBInstances = a })

instance FromXML DescribeDBInstancesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeDBInstances where
    type Sv DescribeDBInstances = RDS
    type Rs DescribeDBInstances = DescribeDBInstancesResponse

    request = post "DescribeDBInstances"
    response _ = xmlResponse

instance AWSPager DescribeDBInstances where
    next rq rs = (\x -> rq & ddbi1Marker ?~ x)
        <$> (rs ^. ddbirrMarker)
