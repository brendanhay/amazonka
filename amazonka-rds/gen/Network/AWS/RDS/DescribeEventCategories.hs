{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeEventCategories
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Displays a list of categories for all event source types, or, if
-- specified, for a specified source type. You can see a list of the event
-- categories and source types in the
-- <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Events.html Events>
-- topic in the Amazon RDS User Guide.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DescribeEventCategories.html>
module Network.AWS.RDS.DescribeEventCategories
    (
    -- * Request
      DescribeEventCategories
    -- ** Request constructor
    , describeEventCategories
    -- ** Request lenses
    , decSourceType
    , decFilters

    -- * Response
    , DescribeEventCategoriesResponse
    -- ** Response constructor
    , describeEventCategoriesResponse
    -- ** Response lenses
    , decrEventCategoriesMapList
    , decrStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'describeEventCategories' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'decSourceType'
--
-- * 'decFilters'
data DescribeEventCategories = DescribeEventCategories'
    { _decSourceType :: !(Maybe Text)
    , _decFilters    :: !(Maybe [Filter])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeEventCategories' smart constructor.
describeEventCategories :: DescribeEventCategories
describeEventCategories =
    DescribeEventCategories'
    { _decSourceType = Nothing
    , _decFilters = Nothing
    }

-- | The type of source that will be generating the events.
--
-- Valid values: db-instance | db-parameter-group | db-security-group |
-- db-snapshot
decSourceType :: Lens' DescribeEventCategories (Maybe Text)
decSourceType = lens _decSourceType (\ s a -> s{_decSourceType = a});

-- | This parameter is not currently supported.
decFilters :: Lens' DescribeEventCategories [Filter]
decFilters = lens _decFilters (\ s a -> s{_decFilters = a}) . _Default;

instance AWSRequest DescribeEventCategories where
        type Sv DescribeEventCategories = RDS
        type Rs DescribeEventCategories =
             DescribeEventCategoriesResponse
        request = post
        response
          = receiveXMLWrapper "DescribeEventCategoriesResult"
              (\ s h x ->
                 DescribeEventCategoriesResponse' <$>
                   (x .@? "EventCategoriesMapList" .!@ mempty >>=
                      may (parseXMLList "EventCategoriesMap"))
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeEventCategories where
        toHeaders = const mempty

instance ToPath DescribeEventCategories where
        toPath = const "/"

instance ToQuery DescribeEventCategories where
        toQuery DescribeEventCategories'{..}
          = mconcat
              ["Action" =:
                 ("DescribeEventCategories" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "SourceType" =: _decSourceType,
               "Filters" =:
                 toQuery (toQueryList "Filter" <$> _decFilters)]

-- | Data returned from the __DescribeEventCategories__ action.
--
-- /See:/ 'describeEventCategoriesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'decrEventCategoriesMapList'
--
-- * 'decrStatus'
data DescribeEventCategoriesResponse = DescribeEventCategoriesResponse'
    { _decrEventCategoriesMapList :: !(Maybe [EventCategoriesMap])
    , _decrStatus                 :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeEventCategoriesResponse' smart constructor.
describeEventCategoriesResponse :: Int -> DescribeEventCategoriesResponse
describeEventCategoriesResponse pStatus =
    DescribeEventCategoriesResponse'
    { _decrEventCategoriesMapList = Nothing
    , _decrStatus = pStatus
    }

-- | A list of EventCategoriesMap data types.
decrEventCategoriesMapList :: Lens' DescribeEventCategoriesResponse [EventCategoriesMap]
decrEventCategoriesMapList = lens _decrEventCategoriesMapList (\ s a -> s{_decrEventCategoriesMapList = a}) . _Default;

-- | FIXME: Undocumented member.
decrStatus :: Lens' DescribeEventCategoriesResponse Int
decrStatus = lens _decrStatus (\ s a -> s{_decrStatus = a});
