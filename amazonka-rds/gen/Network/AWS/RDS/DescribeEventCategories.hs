{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    , decrqSourceType
    , decrqFilters

    -- * Response
    , DescribeEventCategoriesResponse
    -- ** Response constructor
    , describeEventCategoriesResponse
    -- ** Response lenses
    , decrsEventCategoriesMapList
    , decrsStatus
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
-- * 'decrqSourceType'
--
-- * 'decrqFilters'
data DescribeEventCategories = DescribeEventCategories'
    { _decrqSourceType :: !(Maybe Text)
    , _decrqFilters    :: !(Maybe [Filter])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeEventCategories' smart constructor.
describeEventCategories :: DescribeEventCategories
describeEventCategories =
    DescribeEventCategories'
    { _decrqSourceType = Nothing
    , _decrqFilters = Nothing
    }

-- | The type of source that will be generating the events.
--
-- Valid values: db-instance | db-parameter-group | db-security-group |
-- db-snapshot
decrqSourceType :: Lens' DescribeEventCategories (Maybe Text)
decrqSourceType = lens _decrqSourceType (\ s a -> s{_decrqSourceType = a});

-- | This parameter is not currently supported.
decrqFilters :: Lens' DescribeEventCategories [Filter]
decrqFilters = lens _decrqFilters (\ s a -> s{_decrqFilters = a}) . _Default;

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
               "SourceType" =: _decrqSourceType,
               "Filters" =:
                 toQuery (toQueryList "Filter" <$> _decrqFilters)]

-- | Data returned from the __DescribeEventCategories__ action.
--
-- /See:/ 'describeEventCategoriesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'decrsEventCategoriesMapList'
--
-- * 'decrsStatus'
data DescribeEventCategoriesResponse = DescribeEventCategoriesResponse'
    { _decrsEventCategoriesMapList :: !(Maybe [EventCategoriesMap])
    , _decrsStatus                 :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeEventCategoriesResponse' smart constructor.
describeEventCategoriesResponse :: Int -> DescribeEventCategoriesResponse
describeEventCategoriesResponse pStatus =
    DescribeEventCategoriesResponse'
    { _decrsEventCategoriesMapList = Nothing
    , _decrsStatus = pStatus
    }

-- | A list of EventCategoriesMap data types.
decrsEventCategoriesMapList :: Lens' DescribeEventCategoriesResponse [EventCategoriesMap]
decrsEventCategoriesMapList = lens _decrsEventCategoriesMapList (\ s a -> s{_decrsEventCategoriesMapList = a}) . _Default;

-- | FIXME: Undocumented member.
decrsStatus :: Lens' DescribeEventCategoriesResponse Int
decrsStatus = lens _decrsStatus (\ s a -> s{_decrsStatus = a});
