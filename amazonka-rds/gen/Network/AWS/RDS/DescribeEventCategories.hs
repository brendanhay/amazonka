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

-- Module      : Network.AWS.RDS.DescribeEventCategories
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

-- | Displays a list of categories for all event source types, or, if specified,
-- for a specified source type. You can see a list of the event categories and
-- source types in the <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Events.html  Events> topic in the Amazon RDS User Guide.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DescribeEventCategories.html>
module Network.AWS.RDS.DescribeEventCategories
    (
    -- * Request
      DescribeEventCategories
    -- ** Request constructor
    , describeEventCategories
    -- ** Request lenses
    , decFilters
    , decSourceType

    -- * Response
    , DescribeEventCategoriesResponse
    -- ** Response constructor
    , describeEventCategoriesResponse
    -- ** Response lenses
    , decrEventCategoriesMapList
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import qualified GHC.Exts

data DescribeEventCategories = DescribeEventCategories
    { _decFilters    :: List "member" Filter
    , _decSourceType :: Maybe Text
    } deriving (Eq, Show)

-- | 'DescribeEventCategories' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'decFilters' @::@ ['Filter']
--
-- * 'decSourceType' @::@ 'Maybe' 'Text'
--
describeEventCategories :: DescribeEventCategories
describeEventCategories = DescribeEventCategories
    { _decSourceType = Nothing
    , _decFilters    = mempty
    }

-- | This parameter is not currently supported.
decFilters :: Lens' DescribeEventCategories [Filter]
decFilters = lens _decFilters (\s a -> s { _decFilters = a }) . _List

-- | The type of source that will be generating the events.
--
-- Valid values: db-instance | db-parameter-group | db-security-group |
-- db-snapshot
decSourceType :: Lens' DescribeEventCategories (Maybe Text)
decSourceType = lens _decSourceType (\s a -> s { _decSourceType = a })

newtype DescribeEventCategoriesResponse = DescribeEventCategoriesResponse
    { _decrEventCategoriesMapList :: List "member" EventCategoriesMap
    } deriving (Eq, Show, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeEventCategoriesResponse where
    type Item DescribeEventCategoriesResponse = EventCategoriesMap

    fromList = DescribeEventCategoriesResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _decrEventCategoriesMapList

-- | 'DescribeEventCategoriesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'decrEventCategoriesMapList' @::@ ['EventCategoriesMap']
--
describeEventCategoriesResponse :: DescribeEventCategoriesResponse
describeEventCategoriesResponse = DescribeEventCategoriesResponse
    { _decrEventCategoriesMapList = mempty
    }

-- | A list of EventCategoriesMap data types.
decrEventCategoriesMapList :: Lens' DescribeEventCategoriesResponse [EventCategoriesMap]
decrEventCategoriesMapList =
    lens _decrEventCategoriesMapList
        (\s a -> s { _decrEventCategoriesMapList = a })
            . _List

instance ToPath DescribeEventCategories where
    toPath = const "/"

instance ToQuery DescribeEventCategories where
    toQuery DescribeEventCategories{..} = mconcat
        [ "Filters"    =? _decFilters
        , "SourceType" =? _decSourceType
        ]

instance ToHeaders DescribeEventCategories

instance AWSRequest DescribeEventCategories where
    type Sv DescribeEventCategories = RDS
    type Rs DescribeEventCategories = DescribeEventCategoriesResponse

    request  = post "DescribeEventCategories"
    response = xmlResponse

instance FromXML DescribeEventCategoriesResponse where
    parseXML = withElement "DescribeEventCategoriesResult" $ \x -> DescribeEventCategoriesResponse
        <$> x .@  "EventCategoriesMapList"
