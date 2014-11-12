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
    , describeEventCategoriesMessage
    -- ** Request lenses
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
import Network.AWS.Redshift.Types

newtype DescribeEventCategoriesMessage = DescribeEventCategoriesMessage
    { _decmSourceType :: Maybe Text
    } (Eq, Ord, Show, Generic, Monoid)

-- | 'DescribeEventCategoriesMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'decmSourceType' @::@ 'Maybe' 'Text'
--
describeEventCategoriesMessage :: DescribeEventCategoriesMessage
describeEventCategoriesMessage = DescribeEventCategoriesMessage
    { _decmSourceType = Nothing
    }

-- | The source type, such as cluster or parameter group, to which the
-- described event categories apply. Valid values: cluster, snapshot,
-- parameter group, and security group.
decmSourceType :: Lens' DescribeEventCategoriesMessage (Maybe Text)
decmSourceType = lens _decmSourceType (\s a -> s { _decmSourceType = a })
instance ToQuery DescribeEventCategoriesMessage

instance ToPath DescribeEventCategoriesMessage where
    toPath = const "/"

newtype EventCategoriesMessage = EventCategoriesMessage
    { _ecmEventCategoriesMapList :: [EventCategoriesMap]
    } (Eq, Show, Generic, Foldable, Traversable, Monoid, Semigroup)

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

-- | A list of event categories descriptions.
ecmEventCategoriesMapList :: Lens' EventCategoriesMessage [EventCategoriesMap]
ecmEventCategoriesMapList =
    lens _ecmEventCategoriesMapList
        (\s a -> s { _ecmEventCategoriesMapList = a })

instance FromXML EventCategoriesMessage where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EventCategoriesMessage"

instance AWSRequest DescribeEventCategoriesMessage where
    type Sv DescribeEventCategoriesMessage = Redshift
    type Rs DescribeEventCategoriesMessage = EventCategoriesMessage

    request  = post "DescribeEventCategories"
    response = xmlResponse $ \h x -> EventCategoriesMessage
        <$> x %| "EventCategoriesMapList"
