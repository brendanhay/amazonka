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

-- Module      : Network.AWS.RDS.DescribeEventCategories
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Displays a list of categories for all event source types, or, if specified,
-- for a specified source type. You can see a list of the event categories and
-- source types in the Events topic in the Amazon RDS User Guide.
module Network.AWS.RDS.DescribeEventCategories
    (
    -- * Request
      DescribeEventCategoriesMessage
    -- ** Request constructor
    , describeEventCategoriesMessage
    -- ** Request lenses
    , decmFilters
    , decmSourceType

    -- * Response
    , EventCategoriesMessage
    -- ** Response constructor
    , eventCategoriesMessage
    -- ** Response lenses
    , ecmEventCategoriesMapList
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types

data DescribeEventCategoriesMessage = DescribeEventCategoriesMessage
    { _decmFilters    :: [Filter]
    , _decmSourceType :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'DescribeEventCategoriesMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'decmFilters' @::@ ['Filter']
--
-- * 'decmSourceType' @::@ 'Maybe' 'Text'
--
describeEventCategoriesMessage :: DescribeEventCategoriesMessage
describeEventCategoriesMessage = DescribeEventCategoriesMessage
    { _decmSourceType = Nothing
    , _decmFilters    = mempty
    }

-- | This parameter is not currently supported.
decmFilters :: Lens' DescribeEventCategoriesMessage [Filter]
decmFilters = lens _decmFilters (\s a -> s { _decmFilters = a })

-- | The type of source that will be generating the events. Valid values:
-- db-instance | db-parameter-group | db-security-group | db-snapshot.
decmSourceType :: Lens' DescribeEventCategoriesMessage (Maybe Text)
decmSourceType = lens _decmSourceType (\s a -> s { _decmSourceType = a })

instance ToQuery DescribeEventCategoriesMessage

instance ToPath DescribeEventCategoriesMessage where
    toPath = const "/"

newtype EventCategoriesMessage = EventCategoriesMessage
    { _ecmEventCategoriesMapList :: [EventCategoriesMap]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance IsList EventCategoriesMessage
    type Item EventCategoriesMessage = EventCategoriesMap

    fromList = EventCategoriesMessage . fromList
    toList   = toList . _ecmEventCategoriesMapList

-- | 'EventCategoriesMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ecmEventCategoriesMapList' @::@ ['EventCategoriesMap']
--
eventCategoriesMessage :: EventCategoriesMessage
eventCategoriesMessage = EventCategoriesMessage
    { _ecmEventCategoriesMapList = mempty
    }

-- | A list of EventCategoriesMap data types.
ecmEventCategoriesMapList :: Lens' EventCategoriesMessage [EventCategoriesMap]
ecmEventCategoriesMapList =
    lens _ecmEventCategoriesMapList
        (\s a -> s { _ecmEventCategoriesMapList = a })

instance FromXML EventCategoriesMessage where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EventCategoriesMessage"

instance AWSRequest DescribeEventCategoriesMessage where
    type Sv DescribeEventCategoriesMessage = RDS
    type Rs DescribeEventCategoriesMessage = EventCategoriesMessage

    request  = post "DescribeEventCategories"
    response = xmlResponse $ \h x -> EventCategoriesMessage
        <$> x %| "EventCategoriesMapList"
