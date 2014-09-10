{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.DescribeClusterParameters
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a detailed list of parameters contained within the specified Amazon
-- Redshift parameter group. For each parameter the response includes
-- information such as parameter name, description, data type, value, whether
-- the parameter value is modifiable, and so on. You can specify source filter
-- to retrieve parameters of only specific type. For example, to retrieve
-- parameters that were modified by a user action such as from
-- ModifyClusterParameterGroup, you can specify source equal to user. For more
-- information about managing parameter groups, go to Amazon Redshift
-- Parameter Groups in the Amazon Redshift Management Guide.
-- https://redshift.us-east-1.amazonaws.com/ ?Action=DescribeClusterParameters
-- &ParameterGroupName=parametergroup1 &Version=2012-12-01
-- &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20121208/us-east-1/redshift/aws4_request
-- &x-amz-date=20121208T010408Z
-- &x-amz-signedheaders=content-type;host;x-amz-date ISO, MDY string
-- engine-default true Sets the display format for date and time values.
-- datestyle 0 integer engine-default true Sets the number of digits displayed
-- for floating-point values -15-2 extra_float_digits default string
-- engine-default true This parameter applies a user-defined label to a group
-- of queries that are run during the same session.. query_group false boolean
-- engine-default true require ssl for all databaseconnections true,false
-- require_ssl $user, public string engine-default true Sets the schema search
-- order for names that are not schema-qualified. search_path 0 integer
-- engine-default true Aborts any statement that takes over the specified
-- number of milliseconds. statement_timeout
-- [{&quot;query_concurrency&quot;:5}] string engine-default true wlm json
-- configuration wlm_json_configuration 2ba35df4-40d3-11e2-82cf-0b45b05c0221.
module Network.AWS.Redshift.DescribeClusterParameters
    (
    -- * Request
      DescribeClusterParameters
    -- ** Request constructor
    , mkDescribeClusterParameters
    -- ** Request lenses
    , dcpParameterGroupName
    , dcpSource
    , dcpMaxRecords
    , dcpMarker

    -- * Response
    , DescribeClusterParametersResponse
    -- ** Response constructor
    , mkDescribeClusterParametersResponse
    -- ** Response lenses
    , dcprParameters
    , dcprMarker
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.Types
import Network.AWS.Prelude

data DescribeClusterParameters = DescribeClusterParameters
    { _dcpParameterGroupName :: !Text
    , _dcpSource :: !(Maybe Text)
    , _dcpMaxRecords :: !(Maybe Integer)
    , _dcpMarker :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeClusterParameters' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ParameterGroupName ::@ @Text@
--
-- * @Source ::@ @Maybe Text@
--
-- * @MaxRecords ::@ @Maybe Integer@
--
-- * @Marker ::@ @Maybe Text@
--
mkDescribeClusterParameters :: Text -- ^ 'dcpParameterGroupName'
                            -> DescribeClusterParameters
mkDescribeClusterParameters p1 = DescribeClusterParameters
    { _dcpParameterGroupName = p1
    , _dcpSource = Nothing
    , _dcpMaxRecords = Nothing
    , _dcpMarker = Nothing
    }

-- | The name of a cluster parameter group for which to return details.
dcpParameterGroupName :: Lens' DescribeClusterParameters Text
dcpParameterGroupName =
    lens _dcpParameterGroupName (\s a -> s { _dcpParameterGroupName = a })

-- | The parameter types to return. Specify user to show parameters that are
-- different form the default. Similarly, specify engine-default to show
-- parameters that are the same as the default parameter group. Default: All
-- parameter types returned. Valid Values: user | engine-default.
dcpSource :: Lens' DescribeClusterParameters (Maybe Text)
dcpSource = lens _dcpSource (\s a -> s { _dcpSource = a })

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified MaxRecords
-- value, a value is returned in a marker field of the response. You can
-- retrieve the next set of records by retrying the command with the returned
-- marker value. Default: 100 Constraints: minimum 20, maximum 100.
dcpMaxRecords :: Lens' DescribeClusterParameters (Maybe Integer)
dcpMaxRecords = lens _dcpMaxRecords (\s a -> s { _dcpMaxRecords = a })

-- | An optional parameter that specifies the starting point to return a set of
-- response records. When the results of a DescribeClusterParameters request
-- exceed the value specified in MaxRecords, AWS returns a value in the Marker
-- field of the response. You can retrieve the next set of response records by
-- providing the returned marker value in the Marker parameter and retrying
-- the request.
dcpMarker :: Lens' DescribeClusterParameters (Maybe Text)
dcpMarker = lens _dcpMarker (\s a -> s { _dcpMarker = a })

instance ToQuery DescribeClusterParameters where
    toQuery = genericQuery def

-- | Contains the output from the DescribeClusterParameters action.
data DescribeClusterParametersResponse = DescribeClusterParametersResponse
    { _dcprParameters :: [Parameter]
    , _dcprMarker :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeClusterParametersResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Parameters ::@ @[Parameter]@
--
-- * @Marker ::@ @Maybe Text@
--
mkDescribeClusterParametersResponse :: DescribeClusterParametersResponse
mkDescribeClusterParametersResponse = DescribeClusterParametersResponse
    { _dcprParameters = mempty
    , _dcprMarker = Nothing
    }

-- | A list of Parameter instances. Each instance lists the parameters of one
-- cluster parameter group.
dcprParameters :: Lens' DescribeClusterParametersResponse [Parameter]
dcprParameters = lens _dcprParameters (\s a -> s { _dcprParameters = a })

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response, you
-- can retrieve the next set of records by providing this returned marker
-- value in the Marker parameter and retrying the command. If the Marker field
-- is empty, all response records have been retrieved for the request.
dcprMarker :: Lens' DescribeClusterParametersResponse (Maybe Text)
dcprMarker = lens _dcprMarker (\s a -> s { _dcprMarker = a })

instance FromXML DescribeClusterParametersResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeClusterParameters where
    type Sv DescribeClusterParameters = Redshift
    type Rs DescribeClusterParameters = DescribeClusterParametersResponse

    request = post "DescribeClusterParameters"
    response _ = xmlResponse

instance AWSPager DescribeClusterParameters where
    next rq rs = (\x -> rq & dcpMarker ?~ x)
        <$> (rs ^. dcprMarker)
