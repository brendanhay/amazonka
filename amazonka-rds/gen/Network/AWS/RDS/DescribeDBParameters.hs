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

-- Module      : Network.AWS.RDS.DescribeDBParameters
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the detailed parameter list for a particular DB parameter group.
module Network.AWS.RDS.DescribeDBParameters
    (
    -- * Request
      DescribeDBParametersMessage
    -- ** Request constructor
    , describeDBParametersMessage
    -- ** Request lenses
    , ddbpmDBParameterGroupName
    , ddbpmFilters
    , ddbpmMarker
    , ddbpmMaxRecords
    , ddbpmSource

    -- * Response
    , DBParameterGroupDetails
    -- ** Response constructor
    , dbparameterGroupDetails
    -- ** Response lenses
    , dbpgdMarker
    , dbpgdParameters
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types

data DescribeDBParametersMessage = DescribeDBParametersMessage
    { _ddbpmDBParameterGroupName :: Text
    , _ddbpmFilters              :: [Filter]
    , _ddbpmMarker               :: Maybe Text
    , _ddbpmMaxRecords           :: Maybe Int
    , _ddbpmSource               :: Maybe Text
    } (Eq, Show, Generic)

-- | 'DescribeDBParametersMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddbpmDBParameterGroupName' @::@ 'Text'
--
-- * 'ddbpmFilters' @::@ ['Filter']
--
-- * 'ddbpmMarker' @::@ 'Maybe' 'Text'
--
-- * 'ddbpmMaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'ddbpmSource' @::@ 'Maybe' 'Text'
--
describeDBParametersMessage :: Text -- ^ 'ddbpmDBParameterGroupName'
                            -> DescribeDBParametersMessage
describeDBParametersMessage p1 = DescribeDBParametersMessage
    { _ddbpmDBParameterGroupName = p1
    , _ddbpmSource               = Nothing
    , _ddbpmFilters              = mempty
    , _ddbpmMaxRecords           = Nothing
    , _ddbpmMarker               = Nothing
    }

-- | The name of a specific DB parameter group to return details for.
-- Constraints: Must be 1 to 255 alphanumeric characters First character
-- must be a letter Cannot end with a hyphen or contain two consecutive
-- hyphens.
ddbpmDBParameterGroupName :: Lens' DescribeDBParametersMessage Text
ddbpmDBParameterGroupName =
    lens _ddbpmDBParameterGroupName
        (\s a -> s { _ddbpmDBParameterGroupName = a })

-- | This parameter is not currently supported.
ddbpmFilters :: Lens' DescribeDBParametersMessage [Filter]
ddbpmFilters = lens _ddbpmFilters (\s a -> s { _ddbpmFilters = a })

-- | An optional pagination token provided by a previous DescribeDBParameters
-- request. If this parameter is specified, the response includes only
-- records beyond the marker, up to the value specified by MaxRecords.
ddbpmMarker :: Lens' DescribeDBParametersMessage (Maybe Text)
ddbpmMarker = lens _ddbpmMarker (\s a -> s { _ddbpmMarker = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a pagination token called a
-- marker is included in the response so that the remaining results may be
-- retrieved. Default: 100 Constraints: minimum 20, maximum 100.
ddbpmMaxRecords :: Lens' DescribeDBParametersMessage (Maybe Int)
ddbpmMaxRecords = lens _ddbpmMaxRecords (\s a -> s { _ddbpmMaxRecords = a })

-- | The parameter types to return. Default: All parameter types returned
-- Valid Values: user | system | engine-default.
ddbpmSource :: Lens' DescribeDBParametersMessage (Maybe Text)
ddbpmSource = lens _ddbpmSource (\s a -> s { _ddbpmSource = a })
instance ToQuery DescribeDBParametersMessage

instance ToPath DescribeDBParametersMessage where
    toPath = const "/"

data DBParameterGroupDetails = DBParameterGroupDetails
    { _dbpgdMarker     :: Maybe Text
    , _dbpgdParameters :: [Parameter]
    } (Eq, Show, Generic)

-- | 'DBParameterGroupDetails' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dbpgdMarker' @::@ 'Maybe' 'Text'
--
-- * 'dbpgdParameters' @::@ ['Parameter']
--
dbparameterGroupDetails :: DBParameterGroupDetails
dbparameterGroupDetails = DBParameterGroupDetails
    { _dbpgdParameters = mempty
    , _dbpgdMarker     = Nothing
    }

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by MaxRecords.
dbpgdMarker :: Lens' DBParameterGroupDetails (Maybe Text)
dbpgdMarker = lens _dbpgdMarker (\s a -> s { _dbpgdMarker = a })

-- | A list of Parameter values.
dbpgdParameters :: Lens' DBParameterGroupDetails [Parameter]
dbpgdParameters = lens _dbpgdParameters (\s a -> s { _dbpgdParameters = a })

instance FromXML DBParameterGroupDetails where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DBParameterGroupDetails"

instance AWSRequest DescribeDBParametersMessage where
    type Sv DescribeDBParametersMessage = RDS
    type Rs DescribeDBParametersMessage = DBParameterGroupDetails

    request  = post "DescribeDBParameters"
    response = xmlResponse $ \h x -> DBParameterGroupDetails
        <$> x %| "Marker"
        <*> x %| "Parameters"
