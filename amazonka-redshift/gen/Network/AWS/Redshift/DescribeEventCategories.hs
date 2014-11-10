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

-- Module      : Network.AWS.Redshift.DescribeEventCategories
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Displays a list of event categories for all event source types, or for a
-- specified source type. For a list of the event categories and source types,
-- go to Amazon Redshift Event Notifications.
module Network.AWS.Redshift.DescribeEventCategories
    (
    -- * Request
      DescribeEventCategoriesMessage
    -- ** Request constructor
    , describeEventCategories
    -- ** Request lenses
    , decmSourceType

    -- * Response
    , EventCategoriesMessage
    -- ** Response constructor
    , describeEventCategoriesResponse
    -- ** Response lenses
    , ecmEventCategoriesMapList
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types

newtype DescribeEventCategoriesMessage = DescribeEventCategoriesMessage
    { _decmSourceType :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeEventCategoriesMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'decmSourceType' @::@ 'Maybe' 'Text'
--
describeEventCategories :: DescribeEventCategoriesMessage
describeEventCategories = DescribeEventCategoriesMessage
    { _decmSourceType = Nothing
    }

-- | The source type, such as cluster or parameter group, to which the
-- described event categories apply. Valid values: cluster, snapshot,
-- parameter group, and security group.
decmSourceType :: Lens' DescribeEventCategoriesMessage (Maybe Text)
decmSourceType = lens _decmSourceType (\s a -> s { _decmSourceType = a })

instance ToPath DescribeEventCategoriesMessage where
    toPath = const "/"

instance ToQuery DescribeEventCategoriesMessage

newtype EventCategoriesMessage = EventCategoriesMessage
    { _ecmEventCategoriesMapList :: [EventCategoriesMap]
    } deriving (Eq, Show, Generic, Monoid)

-- | 'EventCategoriesMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ecmEventCategoriesMapList' @::@ ['EventCategoriesMap']
--
describeEventCategoriesResponse :: EventCategoriesMessage
describeEventCategoriesResponse = EventCategoriesMessage
    { _ecmEventCategoriesMapList = mempty
    }

-- | A list of event categories descriptions.
ecmEventCategoriesMapList :: Lens' EventCategoriesMessage [EventCategoriesMap]
ecmEventCategoriesMapList =
    lens _ecmEventCategoriesMapList
        (\s a -> s { _ecmEventCategoriesMapList = a })

instance AWSRequest DescribeEventCategoriesMessage where
    type Sv DescribeEventCategoriesMessage = Redshift
    type Rs DescribeEventCategoriesMessage = EventCategoriesMessage

    request  = post "DescribeEventCategories"
    response = xmlResponse $ \h x -> EventCategoriesMessage
        <$> x %| "EventCategoriesMapList"
