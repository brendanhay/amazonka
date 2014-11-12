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
module Network.AWS.RDS.DescribeEngineDefaultParameters
    (
    -- * Request
      DescribeEngineDefaultParametersMessage
    -- ** Request constructor
    , describeEngineDefaultParameters
    -- ** Request lenses
    , dedpmDBParameterGroupFamily
    , dedpmFilters
    , dedpmMarker
    , dedpmMaxRecords

    -- * Response
    , DescribeEngineDefaultParametersResult
    -- ** Response constructor
    , describeEngineDefaultParametersResponse
    -- ** Response lenses
    , dedprEngineDefaults
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types

data DescribeEngineDefaultParametersMessage = DescribeEngineDefaultParametersMessage
    { _dedpmDBParameterGroupFamily :: Text
    , _dedpmFilters                :: [Filter]
    , _dedpmMarker                 :: Maybe Text
    , _dedpmMaxRecords             :: Maybe Int
    } deriving (Eq, Show, Generic)

-- | 'DescribeEngineDefaultParametersMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dedpmDBParameterGroupFamily' @::@ 'Text'
--
-- * 'dedpmFilters' @::@ ['Filter']
--
-- * 'dedpmMarker' @::@ 'Maybe' 'Text'
--
-- * 'dedpmMaxRecords' @::@ 'Maybe' 'Int'
--
describeEngineDefaultParameters :: Text -- ^ 'dedpmDBParameterGroupFamily'
                                -> DescribeEngineDefaultParametersMessage
describeEngineDefaultParameters p1 = DescribeEngineDefaultParametersMessage
    { _dedpmDBParameterGroupFamily = p1
    , _dedpmFilters                = mempty
    , _dedpmMaxRecords             = Nothing
    , _dedpmMarker                 = Nothing
    }

-- | The name of the DB parameter group family.
dedpmDBParameterGroupFamily :: Lens' DescribeEngineDefaultParametersMessage Text
dedpmDBParameterGroupFamily =
    lens _dedpmDBParameterGroupFamily
        (\s a -> s { _dedpmDBParameterGroupFamily = a })

-- | Not currently supported.
dedpmFilters :: Lens' DescribeEngineDefaultParametersMessage [Filter]
dedpmFilters = lens _dedpmFilters (\s a -> s { _dedpmFilters = a })

-- | An optional pagination token provided by a previous
-- DescribeEngineDefaultParameters request. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by MaxRecords.
dedpmMarker :: Lens' DescribeEngineDefaultParametersMessage (Maybe Text)
dedpmMarker = lens _dedpmMarker (\s a -> s { _dedpmMarker = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a pagination token called a
-- marker is included in the response so that the remaining results may be
-- retrieved. Default: 100 Constraints: minimum 20, maximum 100.
dedpmMaxRecords :: Lens' DescribeEngineDefaultParametersMessage (Maybe Int)
dedpmMaxRecords = lens _dedpmMaxRecords (\s a -> s { _dedpmMaxRecords = a })

instance ToQuery DescribeEngineDefaultParametersMessage

instance ToPath DescribeEngineDefaultParametersMessage where
    toPath = const "/"

newtype DescribeEngineDefaultParametersResult = DescribeEngineDefaultParametersResult
    { _dedprEngineDefaults :: Maybe EngineDefaults
    } deriving (Eq, Show, Generic)

-- | 'DescribeEngineDefaultParametersResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dedprEngineDefaults' @::@ 'Maybe' 'EngineDefaults'
--
describeEngineDefaultParametersResponse :: DescribeEngineDefaultParametersResult
describeEngineDefaultParametersResponse = DescribeEngineDefaultParametersResult
    { _dedprEngineDefaults = Nothing
    }

dedprEngineDefaults :: Lens' DescribeEngineDefaultParametersResult (Maybe EngineDefaults)
dedprEngineDefaults =
    lens _dedprEngineDefaults (\s a -> s { _dedprEngineDefaults = a })

instance FromXML DescribeEngineDefaultParametersResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DescribeEngineDefaultParametersResult"

instance AWSRequest DescribeEngineDefaultParametersMessage where
    type Sv DescribeEngineDefaultParametersMessage = RDS
    type Rs DescribeEngineDefaultParametersMessage = DescribeEngineDefaultParametersResult

    request  = post "DescribeEngineDefaultParameters"
    response = xmlResponse $ \h x -> DescribeEngineDefaultParametersResult
        <$> x %| "EngineDefaults"
