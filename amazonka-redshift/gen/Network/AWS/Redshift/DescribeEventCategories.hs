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
-- go to <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-event-notifications.html Amazon Redshift Event Notifications>.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_DescribeEventCategories.html>
module Network.AWS.Redshift.DescribeEventCategories
    (
    -- * Request
      DescribeEventCategories
    -- ** Request constructor
    , describeEventCategories
    -- ** Request lenses
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
import Network.AWS.Redshift.Types
import qualified GHC.Exts

newtype DescribeEventCategories = DescribeEventCategories
    { _decSourceType :: Maybe Text
    } deriving (Eq, Ord, Show, Monoid)

-- | 'DescribeEventCategories' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'decSourceType' @::@ 'Maybe' 'Text'
--
describeEventCategories :: DescribeEventCategories
describeEventCategories = DescribeEventCategories
    { _decSourceType = Nothing
    }

-- | The source type, such as cluster or parameter group, to which the described
-- event categories apply.
--
-- Valid values: cluster, snapshot, parameter group, and security group.
decSourceType :: Lens' DescribeEventCategories (Maybe Text)
decSourceType = lens _decSourceType (\s a -> s { _decSourceType = a })

newtype DescribeEventCategoriesResponse = DescribeEventCategoriesResponse
    { _decrEventCategoriesMapList :: List "EventCategoriesMap" EventCategoriesMap
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

-- | A list of event categories descriptions.
decrEventCategoriesMapList :: Lens' DescribeEventCategoriesResponse [EventCategoriesMap]
decrEventCategoriesMapList =
    lens _decrEventCategoriesMapList
        (\s a -> s { _decrEventCategoriesMapList = a })
            . _List

instance ToPath DescribeEventCategories where
    toPath = const "/"

instance ToQuery DescribeEventCategories where
    toQuery DescribeEventCategories{..} = mconcat
        [ "SourceType" =? _decSourceType
        ]

instance ToHeaders DescribeEventCategories

instance AWSRequest DescribeEventCategories where
    type Sv DescribeEventCategories = Redshift
    type Rs DescribeEventCategories = DescribeEventCategoriesResponse

    request  = post "DescribeEventCategories"
    response = xmlResponse

instance FromXML DescribeEventCategoriesResponse where
    parseXML = withElement "DescribeEventCategoriesResult" $ \x -> DescribeEventCategoriesResponse
        <$> x .@  "EventCategoriesMapList"
