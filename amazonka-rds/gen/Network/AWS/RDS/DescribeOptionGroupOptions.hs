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

-- Module      : Network.AWS.RDS.DescribeOptionGroupOptions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes all available options.
module Network.AWS.RDS.DescribeOptionGroupOptions
    (
    -- * Request
      DescribeOptionGroupOptionsMessage
    -- ** Request constructor
    , describeOptionGroupOptions
    -- ** Request lenses
    , dogomEngineName
    , dogomFilters
    , dogomMajorEngineVersion
    , dogomMarker
    , dogomMaxRecords

    -- * Response
    , OptionGroupOptionsMessage
    -- ** Response constructor
    , describeOptionGroupOptionsResponse
    -- ** Response lenses
    , ogomMarker
    , ogomOptionGroupOptions
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types

data DescribeOptionGroupOptionsMessage = DescribeOptionGroupOptionsMessage
    { _dogomEngineName         :: Text
    , _dogomFilters            :: [Filter]
    , _dogomMajorEngineVersion :: Maybe Text
    , _dogomMarker             :: Maybe Text
    , _dogomMaxRecords         :: Maybe Int
    } deriving (Eq, Show, Generic)

-- | 'DescribeOptionGroupOptionsMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dogomEngineName' @::@ 'Text'
--
-- * 'dogomFilters' @::@ ['Filter']
--
-- * 'dogomMajorEngineVersion' @::@ 'Maybe' 'Text'
--
-- * 'dogomMarker' @::@ 'Maybe' 'Text'
--
-- * 'dogomMaxRecords' @::@ 'Maybe' 'Int'
--
describeOptionGroupOptions :: Text -- ^ 'dogomEngineName'
                           -> DescribeOptionGroupOptionsMessage
describeOptionGroupOptions p1 = DescribeOptionGroupOptionsMessage
    { _dogomEngineName         = p1
    , _dogomMajorEngineVersion = Nothing
    , _dogomFilters            = mempty
    , _dogomMaxRecords         = Nothing
    , _dogomMarker             = Nothing
    }

-- | A required parameter. Options available for the given Engine name will be
-- described.
dogomEngineName :: Lens' DescribeOptionGroupOptionsMessage Text
dogomEngineName = lens _dogomEngineName (\s a -> s { _dogomEngineName = a })

-- | This parameter is not currently supported.
dogomFilters :: Lens' DescribeOptionGroupOptionsMessage [Filter]
dogomFilters = lens _dogomFilters (\s a -> s { _dogomFilters = a })

-- | If specified, filters the results to include only options for the
-- specified major engine version.
dogomMajorEngineVersion :: Lens' DescribeOptionGroupOptionsMessage (Maybe Text)
dogomMajorEngineVersion =
    lens _dogomMajorEngineVersion (\s a -> s { _dogomMajorEngineVersion = a })

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by MaxRecords.
dogomMarker :: Lens' DescribeOptionGroupOptionsMessage (Maybe Text)
dogomMarker = lens _dogomMarker (\s a -> s { _dogomMarker = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a pagination token called a
-- marker is included in the response so that the remaining results can be
-- retrieved. Default: 100 Constraints: minimum 20, maximum 100.
dogomMaxRecords :: Lens' DescribeOptionGroupOptionsMessage (Maybe Int)
dogomMaxRecords = lens _dogomMaxRecords (\s a -> s { _dogomMaxRecords = a })

instance ToQuery DescribeOptionGroupOptionsMessage

instance ToPath DescribeOptionGroupOptionsMessage where
    toPath = const "/"

data OptionGroupOptionsMessage = OptionGroupOptionsMessage
    { _ogomMarker             :: Maybe Text
    , _ogomOptionGroupOptions :: [OptionGroupOption]
    } deriving (Eq, Show, Generic)

-- | 'OptionGroupOptionsMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ogomMarker' @::@ 'Maybe' 'Text'
--
-- * 'ogomOptionGroupOptions' @::@ ['OptionGroupOption']
--
describeOptionGroupOptionsResponse :: OptionGroupOptionsMessage
describeOptionGroupOptionsResponse = OptionGroupOptionsMessage
    { _ogomOptionGroupOptions = mempty
    , _ogomMarker             = Nothing
    }

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by MaxRecords.
ogomMarker :: Lens' OptionGroupOptionsMessage (Maybe Text)
ogomMarker = lens _ogomMarker (\s a -> s { _ogomMarker = a })

ogomOptionGroupOptions :: Lens' OptionGroupOptionsMessage [OptionGroupOption]
ogomOptionGroupOptions =
    lens _ogomOptionGroupOptions (\s a -> s { _ogomOptionGroupOptions = a })

instance FromXML OptionGroupOptionsMessage where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "OptionGroupOptionsMessage"

instance AWSRequest DescribeOptionGroupOptionsMessage where
    type Sv DescribeOptionGroupOptionsMessage = RDS
    type Rs DescribeOptionGroupOptionsMessage = OptionGroupOptionsMessage

    request  = post "DescribeOptionGroupOptions"
    response = xmlResponse $ \h x -> OptionGroupOptionsMessage
        <$> x %| "Marker"
        <*> x %| "OptionGroupOptions"
