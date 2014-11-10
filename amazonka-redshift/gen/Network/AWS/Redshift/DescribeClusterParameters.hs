{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
module Network.AWS.Redshift.DescribeClusterParameters
    (
    -- * Request
      DescribeClusterParametersMessage
    -- ** Request constructor
    , describeClusterParameters
    -- ** Request lenses
    , dcpmMarker
    , dcpmMaxRecords
    , dcpmParameterGroupName
    , dcpmSource

    -- * Response
    , ClusterParameterGroupDetails
    -- ** Response constructor
    , describeClusterParametersResponse
    -- ** Response lenses
    , cpgdMarker
    , cpgdParameters
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types

data DescribeClusterParametersMessage = DescribeClusterParametersMessage
    { _dcpmMarker             :: Maybe Text
    , _dcpmMaxRecords         :: Maybe Int
    , _dcpmParameterGroupName :: Text
    , _dcpmSource             :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeClusterParametersMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcpmMarker' @::@ 'Maybe' 'Text'
--
-- * 'dcpmMaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'dcpmParameterGroupName' @::@ 'Text'
--
-- * 'dcpmSource' @::@ 'Maybe' 'Text'
--
describeClusterParameters :: Text -- ^ 'dcpmParameterGroupName'
                          -> DescribeClusterParametersMessage
describeClusterParameters p1 = DescribeClusterParametersMessage
    { _dcpmParameterGroupName = p1
    , _dcpmSource             = Nothing
    , _dcpmMaxRecords         = Nothing
    , _dcpmMarker             = Nothing
    }

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeClusterParameters
-- request exceed the value specified in MaxRecords, AWS returns a value in
-- the Marker field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the Marker
-- parameter and retrying the request.
dcpmMarker :: Lens' DescribeClusterParametersMessage (Maybe Text)
dcpmMarker = lens _dcpmMarker (\s a -> s { _dcpmMarker = a })

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified MaxRecords
-- value, a value is returned in a marker field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value. Default: 100 Constraints: minimum 20, maximum 100.
dcpmMaxRecords :: Lens' DescribeClusterParametersMessage (Maybe Int)
dcpmMaxRecords = lens _dcpmMaxRecords (\s a -> s { _dcpmMaxRecords = a })

-- | The name of a cluster parameter group for which to return details.
dcpmParameterGroupName :: Lens' DescribeClusterParametersMessage Text
dcpmParameterGroupName =
    lens _dcpmParameterGroupName (\s a -> s { _dcpmParameterGroupName = a })

-- | The parameter types to return. Specify user to show parameters that are
-- different form the default. Similarly, specify engine-default to show
-- parameters that are the same as the default parameter group. Default: All
-- parameter types returned. Valid Values: user | engine-default.
dcpmSource :: Lens' DescribeClusterParametersMessage (Maybe Text)
dcpmSource = lens _dcpmSource (\s a -> s { _dcpmSource = a })

instance ToPath DescribeClusterParametersMessage where
    toPath = const "/"

instance ToQuery DescribeClusterParametersMessage

data ClusterParameterGroupDetails = ClusterParameterGroupDetails
    { _cpgdMarker     :: Maybe Text
    , _cpgdParameters :: [Parameter]
    } deriving (Eq, Show, Generic)

-- | 'ClusterParameterGroupDetails' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpgdMarker' @::@ 'Maybe' 'Text'
--
-- * 'cpgdParameters' @::@ ['Parameter']
--
describeClusterParametersResponse :: ClusterParameterGroupDetails
describeClusterParametersResponse = ClusterParameterGroupDetails
    { _cpgdParameters = mempty
    , _cpgdMarker     = Nothing
    }

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the Marker parameter and retrying the command. If the
-- Marker field is empty, all response records have been retrieved for the
-- request.
cpgdMarker :: Lens' ClusterParameterGroupDetails (Maybe Text)
cpgdMarker = lens _cpgdMarker (\s a -> s { _cpgdMarker = a })

-- | A list of Parameter instances. Each instance lists the parameters of one
-- cluster parameter group.
cpgdParameters :: Lens' ClusterParameterGroupDetails [Parameter]
cpgdParameters = lens _cpgdParameters (\s a -> s { _cpgdParameters = a })

instance AWSRequest DescribeClusterParametersMessage where
    type Sv DescribeClusterParametersMessage = Redshift
    type Rs DescribeClusterParametersMessage = ClusterParameterGroupDetails

    request  = post "DescribeClusterParameters"
    response = xmlResponse $ \h x -> ClusterParameterGroupDetails
        <$> x %| "Marker"
        <*> x %| "Parameters"
