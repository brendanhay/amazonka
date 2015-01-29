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

-- Module      : Network.AWS.RDS.DescribeOptionGroupOptions
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Describes all available options.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DescribeOptionGroupOptions.html>
module Network.AWS.RDS.DescribeOptionGroupOptions
    (
    -- * Request
      DescribeOptionGroupOptions
    -- ** Request constructor
    , describeOptionGroupOptions
    -- ** Request lenses
    , dogoEngineName
    , dogoFilters
    , dogoMajorEngineVersion
    , dogoMarker
    , dogoMaxRecords

    -- * Response
    , DescribeOptionGroupOptionsResponse
    -- ** Response constructor
    , describeOptionGroupOptionsResponse
    -- ** Response lenses
    , dogorMarker
    , dogorOptionGroupOptions
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import qualified GHC.Exts

data DescribeOptionGroupOptions = DescribeOptionGroupOptions
    { _dogoEngineName         :: Text
    , _dogoFilters            :: List "member" Filter
    , _dogoMajorEngineVersion :: Maybe Text
    , _dogoMarker             :: Maybe Text
    , _dogoMaxRecords         :: Maybe Int
    } deriving (Eq, Read, Show)

-- | 'DescribeOptionGroupOptions' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dogoEngineName' @::@ 'Text'
--
-- * 'dogoFilters' @::@ ['Filter']
--
-- * 'dogoMajorEngineVersion' @::@ 'Maybe' 'Text'
--
-- * 'dogoMarker' @::@ 'Maybe' 'Text'
--
-- * 'dogoMaxRecords' @::@ 'Maybe' 'Int'
--
describeOptionGroupOptions :: Text -- ^ 'dogoEngineName'
                           -> DescribeOptionGroupOptions
describeOptionGroupOptions p1 = DescribeOptionGroupOptions
    { _dogoEngineName         = p1
    , _dogoMajorEngineVersion = Nothing
    , _dogoFilters            = mempty
    , _dogoMaxRecords         = Nothing
    , _dogoMarker             = Nothing
    }

-- | A required parameter. Options available for the given Engine name will be
-- described.
dogoEngineName :: Lens' DescribeOptionGroupOptions Text
dogoEngineName = lens _dogoEngineName (\s a -> s { _dogoEngineName = a })

-- | This parameter is not currently supported.
dogoFilters :: Lens' DescribeOptionGroupOptions [Filter]
dogoFilters = lens _dogoFilters (\s a -> s { _dogoFilters = a }) . _List

-- | If specified, filters the results to include only options for the specified
-- major engine version.
dogoMajorEngineVersion :: Lens' DescribeOptionGroupOptions (Maybe Text)
dogoMajorEngineVersion =
    lens _dogoMajorEngineVersion (\s a -> s { _dogoMajorEngineVersion = a })

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the marker,
-- up to the value specified by 'MaxRecords'.
dogoMarker :: Lens' DescribeOptionGroupOptions (Maybe Text)
dogoMarker = lens _dogoMarker (\s a -> s { _dogoMarker = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified 'MaxRecords' value, a pagination token called a marker
-- is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20, maximum 100
dogoMaxRecords :: Lens' DescribeOptionGroupOptions (Maybe Int)
dogoMaxRecords = lens _dogoMaxRecords (\s a -> s { _dogoMaxRecords = a })

data DescribeOptionGroupOptionsResponse = DescribeOptionGroupOptionsResponse
    { _dogorMarker             :: Maybe Text
    , _dogorOptionGroupOptions :: List "member" OptionGroupOption
    } deriving (Eq, Read, Show)

-- | 'DescribeOptionGroupOptionsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dogorMarker' @::@ 'Maybe' 'Text'
--
-- * 'dogorOptionGroupOptions' @::@ ['OptionGroupOption']
--
describeOptionGroupOptionsResponse :: DescribeOptionGroupOptionsResponse
describeOptionGroupOptionsResponse = DescribeOptionGroupOptionsResponse
    { _dogorOptionGroupOptions = mempty
    , _dogorMarker             = Nothing
    }

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the marker,
-- up to the value specified by 'MaxRecords'.
dogorMarker :: Lens' DescribeOptionGroupOptionsResponse (Maybe Text)
dogorMarker = lens _dogorMarker (\s a -> s { _dogorMarker = a })

dogorOptionGroupOptions :: Lens' DescribeOptionGroupOptionsResponse [OptionGroupOption]
dogorOptionGroupOptions =
    lens _dogorOptionGroupOptions (\s a -> s { _dogorOptionGroupOptions = a })
        . _List

instance ToPath DescribeOptionGroupOptions where
    toPath = const "/"

instance ToQuery DescribeOptionGroupOptions where
    toQuery DescribeOptionGroupOptions{..} = mconcat
        [ "EngineName"         =? _dogoEngineName
        , "Filters"            =? _dogoFilters
        , "MajorEngineVersion" =? _dogoMajorEngineVersion
        , "Marker"             =? _dogoMarker
        , "MaxRecords"         =? _dogoMaxRecords
        ]

instance ToHeaders DescribeOptionGroupOptions

instance AWSRequest DescribeOptionGroupOptions where
    type Sv DescribeOptionGroupOptions = RDS
    type Rs DescribeOptionGroupOptions = DescribeOptionGroupOptionsResponse

    request  = post "DescribeOptionGroupOptions"
    response = xmlResponse

instance FromXML DescribeOptionGroupOptionsResponse where
    parseXML = withElement "DescribeOptionGroupOptionsResult" $ \x -> DescribeOptionGroupOptionsResponse
        <$> x .@? "Marker"
        <*> x .@? "OptionGroupOptions" .!@ mempty

instance AWSPager DescribeOptionGroupOptions where
    page rq rs
        | stop (rs ^. dogorMarker) = Nothing
        | otherwise = (\x -> rq & dogoMarker ?~ x)
            <$> (rs ^. dogorMarker)
