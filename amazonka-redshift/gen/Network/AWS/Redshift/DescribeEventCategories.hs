{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Redshift.DescribeEventCategories
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

-- | Displays a list of event categories for all event source types, or for a
-- specified source type. For a list of the event categories and source
-- types, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-event-notifications.html Amazon Redshift Event Notifications>.
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
import Network.AWS.Redshift.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeEventCategories' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'decSourceType'
newtype DescribeEventCategories = DescribeEventCategories'{_decSourceType :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DescribeEventCategories' smart constructor.
describeEventCategories :: DescribeEventCategories
describeEventCategories = DescribeEventCategories'{_decSourceType = Nothing};

-- | The source type, such as cluster or parameter group, to which the
-- described event categories apply.
--
-- Valid values: cluster, snapshot, parameter group, and security group.
decSourceType :: Lens' DescribeEventCategories (Maybe Text)
decSourceType = lens _decSourceType (\ s a -> s{_decSourceType = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest DescribeEventCategories where
        type Sv DescribeEventCategories = Redshift
        type Rs DescribeEventCategories =
             DescribeEventCategoriesResponse
        request = post
        response
          = receiveXMLWrapper "DescribeEventCategoriesResult"
              (\ s h x ->
                 DescribeEventCategoriesResponse' <$>
                   (x .@? "EventCategoriesMapList" .!@ mempty >>=
                      may (parseXMLList "EventCategoriesMap")))

instance ToHeaders DescribeEventCategories where
        toHeaders = const mempty

instance ToPath DescribeEventCategories where
        toPath = const "/"

instance ToQuery DescribeEventCategories where
        toQuery DescribeEventCategories'{..}
          = mconcat
              ["Action" =:
                 ("DescribeEventCategories" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "SourceType" =: _decSourceType]

-- | /See:/ 'describeEventCategoriesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'decrEventCategoriesMapList'
newtype DescribeEventCategoriesResponse = DescribeEventCategoriesResponse'{_decrEventCategoriesMapList :: Maybe [EventCategoriesMap]} deriving (Eq, Read, Show)

-- | 'DescribeEventCategoriesResponse' smart constructor.
describeEventCategoriesResponse :: DescribeEventCategoriesResponse
describeEventCategoriesResponse = DescribeEventCategoriesResponse'{_decrEventCategoriesMapList = Nothing};

-- | A list of event categories descriptions.
decrEventCategoriesMapList :: Lens' DescribeEventCategoriesResponse [EventCategoriesMap]
decrEventCategoriesMapList = lens _decrEventCategoriesMapList (\ s a -> s{_decrEventCategoriesMapList = a}) . _Default;
