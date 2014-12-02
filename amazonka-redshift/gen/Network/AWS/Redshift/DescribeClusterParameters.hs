{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
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
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns a detailed list of parameters contained within the specified Amazon
-- Redshift parameter group. For each parameter the response includes
-- information such as parameter name, description, data type, value, whether
-- the parameter value is modifiable, and so on.
--
-- You can specify /source/ filter to retrieve parameters of only specific type.
-- For example, to retrieve parameters that were modified by a user action such
-- as from 'ModifyClusterParameterGroup', you can specify /source/ equal to /user/.
--
-- For more information about managing parameter groups, go to <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon RedshiftParameter Groups> in the /Amazon Redshift Cluster Management Guide/.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_DescribeClusterParameters.html>
module Network.AWS.Redshift.DescribeClusterParameters
    (
    -- * Request
      DescribeClusterParameters
    -- ** Request constructor
    , describeClusterParameters
    -- ** Request lenses
    , dcp1Marker
    , dcp1MaxRecords
    , dcp1ParameterGroupName
    , dcp1Source

    -- * Response
    , DescribeClusterParametersResponse
    -- ** Response constructor
    , describeClusterParametersResponse
    -- ** Response lenses
    , dcprMarker
    , dcprParameters
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types
import qualified GHC.Exts

data DescribeClusterParameters = DescribeClusterParameters
    { _dcp1Marker             :: Maybe Text
    , _dcp1MaxRecords         :: Maybe Int
    , _dcp1ParameterGroupName :: Text
    , _dcp1Source             :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'DescribeClusterParameters' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcp1Marker' @::@ 'Maybe' 'Text'
--
-- * 'dcp1MaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'dcp1ParameterGroupName' @::@ 'Text'
--
-- * 'dcp1Source' @::@ 'Maybe' 'Text'
--
describeClusterParameters :: Text -- ^ 'dcp1ParameterGroupName'
                          -> DescribeClusterParameters
describeClusterParameters p1 = DescribeClusterParameters
    { _dcp1ParameterGroupName = p1
    , _dcp1Source             = Nothing
    , _dcp1MaxRecords         = Nothing
    , _dcp1Marker             = Nothing
    }

-- | An optional parameter that specifies the starting point to return a set of
-- response records. When the results of a 'DescribeClusterParameters' request
-- exceed the value specified in 'MaxRecords', AWS returns a value in the 'Marker'
-- field of the response. You can retrieve the next set of response records by
-- providing the returned marker value in the 'Marker' parameter and retrying the
-- request.
dcp1Marker :: Lens' DescribeClusterParameters (Maybe Text)
dcp1Marker = lens _dcp1Marker (\s a -> s { _dcp1Marker = a })

-- | The maximum number of response records to return in each call. If the number
-- of remaining response records exceeds the specified 'MaxRecords' value, a value
-- is returned in a 'marker' field of the response. You can retrieve the next set
-- of records by retrying the command with the returned marker value.
--
-- Default: '100'
--
-- Constraints: minimum 20, maximum 100.
dcp1MaxRecords :: Lens' DescribeClusterParameters (Maybe Int)
dcp1MaxRecords = lens _dcp1MaxRecords (\s a -> s { _dcp1MaxRecords = a })

-- | The name of a cluster parameter group for which to return details.
dcp1ParameterGroupName :: Lens' DescribeClusterParameters Text
dcp1ParameterGroupName =
    lens _dcp1ParameterGroupName (\s a -> s { _dcp1ParameterGroupName = a })

-- | The parameter types to return. Specify 'user' to show parameters that are
-- different form the default. Similarly, specify 'engine-default' to show
-- parameters that are the same as the default parameter group.
--
-- Default: All parameter types returned.
--
-- Valid Values: 'user' | 'engine-default'
dcp1Source :: Lens' DescribeClusterParameters (Maybe Text)
dcp1Source = lens _dcp1Source (\s a -> s { _dcp1Source = a })

data DescribeClusterParametersResponse = DescribeClusterParametersResponse
    { _dcprMarker     :: Maybe Text
    , _dcprParameters :: List "member" Parameter
    } deriving (Eq, Show)

-- | 'DescribeClusterParametersResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcprMarker' @::@ 'Maybe' 'Text'
--
-- * 'dcprParameters' @::@ ['Parameter']
--
describeClusterParametersResponse :: DescribeClusterParametersResponse
describeClusterParametersResponse = DescribeClusterParametersResponse
    { _dcprParameters = mempty
    , _dcprMarker     = Nothing
    }

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response, you
-- can retrieve the next set of records by providing this returned marker value
-- in the 'Marker' parameter and retrying the command. If the 'Marker' field is
-- empty, all response records have been retrieved for the request.
dcprMarker :: Lens' DescribeClusterParametersResponse (Maybe Text)
dcprMarker = lens _dcprMarker (\s a -> s { _dcprMarker = a })

-- | A list of 'Parameter' instances. Each instance lists the parameters of one
-- cluster parameter group.
dcprParameters :: Lens' DescribeClusterParametersResponse [Parameter]
dcprParameters = lens _dcprParameters (\s a -> s { _dcprParameters = a }) . _List

instance ToPath DescribeClusterParameters where
    toPath = const "/"

instance ToQuery DescribeClusterParameters where
    toQuery DescribeClusterParameters{..} = mconcat
        [ "Marker"             =? _dcp1Marker
        , "MaxRecords"         =? _dcp1MaxRecords
        , "ParameterGroupName" =? _dcp1ParameterGroupName
        , "Source"             =? _dcp1Source
        ]

instance ToHeaders DescribeClusterParameters

instance AWSRequest DescribeClusterParameters where
    type Sv DescribeClusterParameters = Redshift
    type Rs DescribeClusterParameters = DescribeClusterParametersResponse

    request  = post "DescribeClusterParameters"
    response = xmlResponse

instance FromXML DescribeClusterParametersResponse where
    parseXML = withElement "DescribeClusterParametersResult" $ \x -> DescribeClusterParametersResponse
        <$> x .@? "Marker"
        <*> x .@? "Parameters" .!@ mempty

instance AWSPager DescribeClusterParameters where
    page rq rs
        | stop (rq ^. dcp1Marker) = Nothing
        | otherwise = (\x -> rq & dcp1Marker ?~ x)
            <$> (rs ^. dcprMarker)
