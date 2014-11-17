{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.DescribeEngineDefaultParameters
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the default engine and system parameter information for the
-- specified database engine.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DescribeEngineDefaultParameters.html>
module Network.AWS.RDS.DescribeEngineDefaultParameters
    (
    -- * Request
      DescribeEngineDefaultParameters
    -- ** Request constructor
    , describeEngineDefaultParameters
    -- ** Request lenses
    , dedpDBParameterGroupFamily
    , dedpFilters
    , dedpMarker
    , dedpMaxRecords

    -- * Response
    , DescribeEngineDefaultParametersResponse
    -- ** Response constructor
    , describeEngineDefaultParametersResponse
    -- ** Response lenses
    , dedprEngineDefaults
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import qualified GHC.Exts

data DescribeEngineDefaultParameters = DescribeEngineDefaultParameters
    { _dedpDBParameterGroupFamily :: Text
    , _dedpFilters                :: [Filter]
    , _dedpMarker                 :: Maybe Text
    , _dedpMaxRecords             :: Maybe Int
    } deriving (Eq, Show, Generic)

-- | 'DescribeEngineDefaultParameters' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dedpDBParameterGroupFamily' @::@ 'Text'
--
-- * 'dedpFilters' @::@ ['Filter']
--
-- * 'dedpMarker' @::@ 'Maybe' 'Text'
--
-- * 'dedpMaxRecords' @::@ 'Maybe' 'Int'
--
describeEngineDefaultParameters :: Text -- ^ 'dedpDBParameterGroupFamily'
                                -> DescribeEngineDefaultParameters
describeEngineDefaultParameters p1 = DescribeEngineDefaultParameters
    { _dedpDBParameterGroupFamily = p1
    , _dedpFilters                = mempty
    , _dedpMaxRecords             = Nothing
    , _dedpMarker                 = Nothing
    }

-- | The name of the DB parameter group family.
dedpDBParameterGroupFamily :: Lens' DescribeEngineDefaultParameters Text
dedpDBParameterGroupFamily =
    lens _dedpDBParameterGroupFamily
        (\s a -> s { _dedpDBParameterGroupFamily = a })

-- | Not currently supported.
dedpFilters :: Lens' DescribeEngineDefaultParameters [Filter]
dedpFilters = lens _dedpFilters (\s a -> s { _dedpFilters = a })

-- | An optional pagination token provided by a previous
-- DescribeEngineDefaultParameters request. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by MaxRecords.
dedpMarker :: Lens' DescribeEngineDefaultParameters (Maybe Text)
dedpMarker = lens _dedpMarker (\s a -> s { _dedpMarker = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a pagination token called a
-- marker is included in the response so that the remaining results may be
-- retrieved. Default: 100 Constraints: minimum 20, maximum 100.
dedpMaxRecords :: Lens' DescribeEngineDefaultParameters (Maybe Int)
dedpMaxRecords = lens _dedpMaxRecords (\s a -> s { _dedpMaxRecords = a })

newtype DescribeEngineDefaultParametersResponse = DescribeEngineDefaultParametersResponse
    { _dedprEngineDefaults :: Maybe EngineDefaults
    } deriving (Eq, Show, Generic)

-- | 'DescribeEngineDefaultParametersResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dedprEngineDefaults' @::@ 'Maybe' 'EngineDefaults'
--
describeEngineDefaultParametersResponse :: DescribeEngineDefaultParametersResponse
describeEngineDefaultParametersResponse = DescribeEngineDefaultParametersResponse
    { _dedprEngineDefaults = Nothing
    }

dedprEngineDefaults :: Lens' DescribeEngineDefaultParametersResponse (Maybe EngineDefaults)
dedprEngineDefaults =
    lens _dedprEngineDefaults (\s a -> s { _dedprEngineDefaults = a })

instance ToPath DescribeEngineDefaultParameters where
    toPath = const "/"

instance ToQuery DescribeEngineDefaultParameters

instance ToHeaders DescribeEngineDefaultParameters

instance AWSRequest DescribeEngineDefaultParameters where
    type Sv DescribeEngineDefaultParameters = RDS
    type Rs DescribeEngineDefaultParameters = DescribeEngineDefaultParametersResponse

    request  = post "DescribeEngineDefaultParameters"
    response = xmlResponse

instance FromXML DescribeEngineDefaultParametersResponse where
    parseXML c = DescribeEngineDefaultParametersResponse
        <$> c .:? "EngineDefaults"
