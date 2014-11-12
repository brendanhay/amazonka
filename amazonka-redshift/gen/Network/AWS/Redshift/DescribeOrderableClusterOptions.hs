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

-- Module      : Network.AWS.Redshift.DescribeOrderableClusterOptions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of orderable cluster options. Before you create a new
-- cluster you can use this operation to find what options are available, such
-- as the EC2 Availability Zones (AZ) in the specific AWS region that you can
-- specify, and the node types you can request. The node types differ by
-- available storage, memory, CPU and price. With the cost involved you might
-- want to obtain a list of cluster options in the specific region and specify
-- values when creating a cluster. For more information about managing
-- clusters, go to Amazon Redshift Clusters in the Amazon Redshift Management
-- Guide.
module Network.AWS.Redshift.DescribeOrderableClusterOptions
    (
    -- * Request
      DescribeOrderableClusterOptionsMessage
    -- ** Request constructor
    , describeOrderableClusterOptionsMessage
    -- ** Request lenses
    , docomClusterVersion
    , docomMarker
    , docomMaxRecords
    , docomNodeType

    -- * Response
    , OrderableClusterOptionsMessage
    -- ** Response constructor
    , orderableClusterOptionsMessage
    -- ** Response lenses
    , ocomMarker
    , ocomOrderableClusterOptions
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types

data DescribeOrderableClusterOptionsMessage = DescribeOrderableClusterOptionsMessage
    { _docomClusterVersion :: Maybe Text
    , _docomMarker         :: Maybe Text
    , _docomMaxRecords     :: Maybe Int
    , _docomNodeType       :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeOrderableClusterOptionsMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'docomClusterVersion' @::@ 'Maybe' 'Text'
--
-- * 'docomMarker' @::@ 'Maybe' 'Text'
--
-- * 'docomMaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'docomNodeType' @::@ 'Maybe' 'Text'
--
describeOrderableClusterOptionsMessage :: DescribeOrderableClusterOptionsMessage
describeOrderableClusterOptionsMessage = DescribeOrderableClusterOptionsMessage
    { _docomClusterVersion = Nothing
    , _docomNodeType       = Nothing
    , _docomMaxRecords     = Nothing
    , _docomMarker         = Nothing
    }

-- | The version filter value. Specify this parameter to show only the
-- available offerings matching the specified version. Default: All
-- versions. Constraints: Must be one of the version returned from
-- DescribeClusterVersions.
docomClusterVersion :: Lens' DescribeOrderableClusterOptionsMessage (Maybe Text)
docomClusterVersion =
    lens _docomClusterVersion (\s a -> s { _docomClusterVersion = a })

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a
-- DescribeOrderableClusterOptions request exceed the value specified in
-- MaxRecords, AWS returns a value in the Marker field of the response. You
-- can retrieve the next set of response records by providing the returned
-- marker value in the Marker parameter and retrying the request.
docomMarker :: Lens' DescribeOrderableClusterOptionsMessage (Maybe Text)
docomMarker = lens _docomMarker (\s a -> s { _docomMarker = a })

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified MaxRecords
-- value, a value is returned in a marker field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value. Default: 100 Constraints: minimum 20, maximum 100.
docomMaxRecords :: Lens' DescribeOrderableClusterOptionsMessage (Maybe Int)
docomMaxRecords = lens _docomMaxRecords (\s a -> s { _docomMaxRecords = a })

-- | The node type filter value. Specify this parameter to show only the
-- available offerings matching the specified node type.
docomNodeType :: Lens' DescribeOrderableClusterOptionsMessage (Maybe Text)
docomNodeType = lens _docomNodeType (\s a -> s { _docomNodeType = a })

instance ToQuery DescribeOrderableClusterOptionsMessage

instance ToPath DescribeOrderableClusterOptionsMessage where
    toPath = const "/"

data OrderableClusterOptionsMessage = OrderableClusterOptionsMessage
    { _ocomMarker                  :: Maybe Text
    , _ocomOrderableClusterOptions :: [OrderableClusterOption]
    } deriving (Eq, Show, Generic)

-- | 'OrderableClusterOptionsMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ocomMarker' @::@ 'Maybe' 'Text'
--
-- * 'ocomOrderableClusterOptions' @::@ ['OrderableClusterOption']
--
orderableClusterOptionsMessage :: OrderableClusterOptionsMessage
orderableClusterOptionsMessage = OrderableClusterOptionsMessage
    { _ocomOrderableClusterOptions = mempty
    , _ocomMarker                  = Nothing
    }

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the Marker parameter and retrying the command. If the
-- Marker field is empty, all response records have been retrieved for the
-- request.
ocomMarker :: Lens' OrderableClusterOptionsMessage (Maybe Text)
ocomMarker = lens _ocomMarker (\s a -> s { _ocomMarker = a })

-- | An OrderableClusterOption structure containing information about
-- orderable options for the Cluster.
ocomOrderableClusterOptions :: Lens' OrderableClusterOptionsMessage [OrderableClusterOption]
ocomOrderableClusterOptions =
    lens _ocomOrderableClusterOptions
        (\s a -> s { _ocomOrderableClusterOptions = a })

instance FromXML OrderableClusterOptionsMessage where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "OrderableClusterOptionsMessage"

instance AWSRequest DescribeOrderableClusterOptionsMessage where
    type Sv DescribeOrderableClusterOptionsMessage = Redshift
    type Rs DescribeOrderableClusterOptionsMessage = OrderableClusterOptionsMessage

    request  = post "DescribeOrderableClusterOptions"
    response = xmlResponse $ \h x -> OrderableClusterOptionsMessage
        <$> x %| "Marker"
        <*> x %| "OrderableClusterOptions"
