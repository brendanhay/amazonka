{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.V2012_12_01.DescribeClusterParameters
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
module Network.AWS.Redshift.V2012_12_01.DescribeClusterParameters
    (
    -- * Request
      DescribeClusterParameters
    -- ** Request constructor
    , describeClusterParameters
    -- ** Request lenses
    , dcpmParameterGroupName
    , dcpmMaxRecords
    , dcpmSource
    , dcpmMarker

    -- * Response
    , DescribeClusterParametersResponse
    -- ** Response lenses
    , cpgdParameters
    , cpgdMarker
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeClusterParameters' request.
describeClusterParameters :: Text -- ^ 'dcpmParameterGroupName'
                          -> DescribeClusterParameters
describeClusterParameters p1 = DescribeClusterParameters
    { _dcpmParameterGroupName = p1
    , _dcpmMaxRecords = Nothing
    , _dcpmSource = Nothing
    , _dcpmMarker = Nothing
    }

data DescribeClusterParameters = DescribeClusterParameters
    { _dcpmParameterGroupName :: Text
      -- ^ The name of a cluster parameter group for which to return
      -- details.
    , _dcpmMaxRecords :: Maybe Integer
      -- ^ The maximum number of response records to return in each call. If
      -- the number of remaining response records exceeds the specified
      -- MaxRecords value, a value is returned in a marker field of the
      -- response. You can retrieve the next set of records by retrying
      -- the command with the returned marker value. Default: 100
      -- Constraints: minimum 20, maximum 100.
    , _dcpmSource :: Maybe Text
      -- ^ The parameter types to return. Specify user to show parameters
      -- that are different form the default. Similarly, specify
      -- engine-default to show parameters that are the same as the
      -- default parameter group. Default: All parameter types returned.
      -- Valid Values: user | engine-default.
    , _dcpmMarker :: Maybe Text
      -- ^ An optional parameter that specifies the starting point to return
      -- a set of response records. When the results of a
      -- DescribeClusterParameters request exceed the value specified in
      -- MaxRecords, AWS returns a value in the Marker field of the
      -- response. You can retrieve the next set of response records by
      -- providing the returned marker value in the Marker parameter and
      -- retrying the request.
    } deriving (Show, Generic)

-- | The name of a cluster parameter group for which to return details.
dcpmParameterGroupName
    :: Functor f
    => (Text
    -> f (Text))
    -> DescribeClusterParameters
    -> f DescribeClusterParameters
dcpmParameterGroupName f x =
    (\y -> x { _dcpmParameterGroupName = y })
       <$> f (_dcpmParameterGroupName x)
{-# INLINE dcpmParameterGroupName #-}

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified MaxRecords
-- value, a value is returned in a marker field of the response. You can
-- retrieve the next set of records by retrying the command with the returned
-- marker value. Default: 100 Constraints: minimum 20, maximum 100.
dcpmMaxRecords
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DescribeClusterParameters
    -> f DescribeClusterParameters
dcpmMaxRecords f x =
    (\y -> x { _dcpmMaxRecords = y })
       <$> f (_dcpmMaxRecords x)
{-# INLINE dcpmMaxRecords #-}

-- | The parameter types to return. Specify user to show parameters that are
-- different form the default. Similarly, specify engine-default to show
-- parameters that are the same as the default parameter group. Default: All
-- parameter types returned. Valid Values: user | engine-default.
dcpmSource
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeClusterParameters
    -> f DescribeClusterParameters
dcpmSource f x =
    (\y -> x { _dcpmSource = y })
       <$> f (_dcpmSource x)
{-# INLINE dcpmSource #-}

-- | An optional parameter that specifies the starting point to return a set of
-- response records. When the results of a DescribeClusterParameters request
-- exceed the value specified in MaxRecords, AWS returns a value in the Marker
-- field of the response. You can retrieve the next set of response records by
-- providing the returned marker value in the Marker parameter and retrying
-- the request.
dcpmMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeClusterParameters
    -> f DescribeClusterParameters
dcpmMarker f x =
    (\y -> x { _dcpmMarker = y })
       <$> f (_dcpmMarker x)
{-# INLINE dcpmMarker #-}

instance ToQuery DescribeClusterParameters where
    toQuery = genericQuery def

data DescribeClusterParametersResponse = DescribeClusterParametersResponse
    { _cpgdParameters :: [Parameter]
      -- ^ A list of Parameter instances. Each instance lists the parameters
      -- of one cluster parameter group.
    , _cpgdMarker :: Maybe Text
      -- ^ A value that indicates the starting point for the next set of
      -- response records in a subsequent request. If a value is returned
      -- in a response, you can retrieve the next set of records by
      -- providing this returned marker value in the Marker parameter and
      -- retrying the command. If the Marker field is empty, all response
      -- records have been retrieved for the request.
    } deriving (Show, Generic)

-- | A list of Parameter instances. Each instance lists the parameters of one
-- cluster parameter group.
cpgdParameters
    :: Functor f
    => ([Parameter]
    -> f ([Parameter]))
    -> DescribeClusterParametersResponse
    -> f DescribeClusterParametersResponse
cpgdParameters f x =
    (\y -> x { _cpgdParameters = y })
       <$> f (_cpgdParameters x)
{-# INLINE cpgdParameters #-}

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response, you
-- can retrieve the next set of records by providing this returned marker
-- value in the Marker parameter and retrying the command. If the Marker field
-- is empty, all response records have been retrieved for the request.
cpgdMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeClusterParametersResponse
    -> f DescribeClusterParametersResponse
cpgdMarker f x =
    (\y -> x { _cpgdMarker = y })
       <$> f (_cpgdMarker x)
{-# INLINE cpgdMarker #-}

instance FromXML DescribeClusterParametersResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeClusterParameters where
    type Sv DescribeClusterParameters = Redshift
    type Rs DescribeClusterParameters = DescribeClusterParametersResponse

    request = post "DescribeClusterParameters"
    response _ = xmlResponse

instance AWSPager DescribeClusterParameters where
    next rq rs = (\x -> rq { _dcpmMarker = Just x })
        <$> (_cpgdMarker rs)
