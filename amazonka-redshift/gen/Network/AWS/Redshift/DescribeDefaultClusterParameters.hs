{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.DescribeDefaultClusterParameters
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of parameter settings for the specified parameter group
-- family. For more information about managing parameter groups, go to Amazon
-- Redshift Parameter Groups in the Amazon Redshift Management Guide.
-- https://redshift.us-east-1.amazonaws.com/
-- ?Action=DescribeDefaultClusterParameters &ParameterGroupFamily=redshift-1.0
-- &Version=2012-12-01 &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20121207/us-east-1/redshift/aws4_request
-- &x-amz-date=20121207T231708Z
-- &x-amz-signedheaders=content-type;host;x-amz-date redshift-1.0 ISO, MDY
-- string engine-default true Sets the display format for date and time
-- values. datestyle 0 integer engine-default true Sets the number of digits
-- displayed for floating-point values -15-2 extra_float_digits default string
-- engine-default true This parameter applies a user-defined label to a group
-- of queries that are run during the same session.. query_group false boolean
-- engine-default true require ssl for all databaseconnections true,false
-- require_ssl $user, public string engine-default true Sets the schema search
-- order for names that are not schema-qualified. search_path 0 integer
-- engine-default true Aborts any statement that takes over the specified
-- number of milliseconds. statement_timeout
-- [{&quot;query_concurrency&quot;:5}] string engine-default true wlm json
-- configuration wlm_json_configuration 396df00b-40c4-11e2-82cf-0b45b05c0221.
module Network.AWS.Redshift.DescribeDefaultClusterParameters
    (
    -- * Request
      DescribeDefaultClusterParameters
    -- ** Request constructor
    , describeDefaultClusterParameters
    -- ** Request lenses
    , ddcpParameterGroupFamily
    , ddcpMaxRecords
    , ddcpMarker

    -- * Response
    , DescribeDefaultClusterParametersResponse
    -- ** Response constructor
    , describeDefaultClusterParametersResponse
    -- ** Response lenses
    , ddcprDefaultClusterParameters
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.Types
import Network.AWS.Prelude

-- | 
data DescribeDefaultClusterParameters = DescribeDefaultClusterParameters
    { _ddcpParameterGroupFamily :: Text
    , _ddcpMaxRecords :: Maybe Integer
    , _ddcpMarker :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeDefaultClusterParameters' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ParameterGroupFamily ::@ @Text@
--
-- * @MaxRecords ::@ @Maybe Integer@
--
-- * @Marker ::@ @Maybe Text@
--
describeDefaultClusterParameters :: Text -- ^ 'ddcpParameterGroupFamily'
                                   -> DescribeDefaultClusterParameters
describeDefaultClusterParameters p1 = DescribeDefaultClusterParameters
    { _ddcpParameterGroupFamily = p1
    , _ddcpMaxRecords = Nothing
    , _ddcpMarker = Nothing
    }

-- | The name of the cluster parameter group family.
ddcpParameterGroupFamily :: Lens' DescribeDefaultClusterParameters Text
ddcpParameterGroupFamily =
    lens _ddcpParameterGroupFamily
         (\s a -> s { _ddcpParameterGroupFamily = a })

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified MaxRecords
-- value, a value is returned in a marker field of the response. You can
-- retrieve the next set of records by retrying the command with the returned
-- marker value. Default: 100 Constraints: minimum 20, maximum 100.
ddcpMaxRecords :: Lens' DescribeDefaultClusterParameters (Maybe Integer)
ddcpMaxRecords = lens _ddcpMaxRecords (\s a -> s { _ddcpMaxRecords = a })

-- | An optional parameter that specifies the starting point to return a set of
-- response records. When the results of a DescribeDefaultClusterParameters
-- request exceed the value specified in MaxRecords, AWS returns a value in
-- the Marker field of the response. You can retrieve the next set of response
-- records by providing the returned marker value in the Marker parameter and
-- retrying the request.
ddcpMarker :: Lens' DescribeDefaultClusterParameters (Maybe Text)
ddcpMarker = lens _ddcpMarker (\s a -> s { _ddcpMarker = a })

instance ToQuery DescribeDefaultClusterParameters where
    toQuery = genericQuery def

newtype DescribeDefaultClusterParametersResponse = DescribeDefaultClusterParametersResponse
    { _ddcprDefaultClusterParameters :: DefaultClusterParameters
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeDefaultClusterParametersResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DefaultClusterParameters ::@ @DefaultClusterParameters@
--
describeDefaultClusterParametersResponse :: DefaultClusterParameters -- ^ 'ddcprDefaultClusterParameters'
                                           -> DescribeDefaultClusterParametersResponse
describeDefaultClusterParametersResponse p1 = DescribeDefaultClusterParametersResponse
    { _ddcprDefaultClusterParameters = p1
    }

-- | Describes the default cluster parameters for a parameter group family.
ddcprDefaultClusterParameters :: Lens' DescribeDefaultClusterParametersResponse DefaultClusterParameters
ddcprDefaultClusterParameters =
    lens _ddcprDefaultClusterParameters
         (\s a -> s { _ddcprDefaultClusterParameters = a })

instance FromXML DescribeDefaultClusterParametersResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeDefaultClusterParameters where
    type Sv DescribeDefaultClusterParameters = Redshift
    type Rs DescribeDefaultClusterParameters = DescribeDefaultClusterParametersResponse

    request = post "DescribeDefaultClusterParameters"
    response _ = xmlResponse

instance AWSPager DescribeDefaultClusterParameters where
    next rq rs = (\x -> rq & ddcpMarker ?~ x)
        <$> (rs ^. ddcprDefaultClusterParameters . dcp1Marker)
