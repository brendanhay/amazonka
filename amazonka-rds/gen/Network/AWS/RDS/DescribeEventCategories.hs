{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeEventCategories
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays a list of categories for all event source types, or, if
-- specified, for a specified source type. You can see a list of the event
-- categories and source types in the
-- <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Events.html Events>
-- topic in the /Amazon RDS User Guide./
--
-- /See:/ <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DescribeEventCategories.html AWS API Reference> for DescribeEventCategories.
module Network.AWS.RDS.DescribeEventCategories
    (
    -- * Creating a Request
      describeEventCategories
    , DescribeEventCategories
    -- * Request Lenses
    , decSourceType
    , decFilters

    -- * Destructuring the Response
    , describeEventCategoriesResponse
    , DescribeEventCategoriesResponse
    -- * Response Lenses
    , decrsEventCategoriesMapList
    , decrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.RDS.Types.Product
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'describeEventCategories' smart constructor.
data DescribeEventCategories = DescribeEventCategories'
    { _decSourceType :: !(Maybe Text)
    , _decFilters    :: !(Maybe [Filter])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeEventCategories' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'decSourceType'
--
-- * 'decFilters'
describeEventCategories
    :: DescribeEventCategories
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
decFilters = lens _decFilters (\ s a -> s{_decFilters = a}) . _Default . _Coerce;

instance AWSRequest DescribeEventCategories where
        type Rs DescribeEventCategories =
             DescribeEventCategoriesResponse
        request = postQuery rDS
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
data DescribeEventCategoriesResponse = DescribeEventCategoriesResponse'
    { _decrsEventCategoriesMapList :: !(Maybe [EventCategoriesMap])
    , _decrsStatus                 :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeEventCategoriesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'decrsEventCategoriesMapList'
--
-- * 'decrsStatus'
describeEventCategoriesResponse
    :: Int -- ^ 'decrsStatus'
    -> DescribeEventCategoriesResponse
describeEventCategoriesResponse pStatus_ =
    DescribeEventCategoriesResponse'
    { _decrsEventCategoriesMapList = Nothing
    , _decrsStatus = pStatus_
    }

-- | A list of EventCategoriesMap data types.
decrsEventCategoriesMapList :: Lens' DescribeEventCategoriesResponse [EventCategoriesMap]
decrsEventCategoriesMapList = lens _decrsEventCategoriesMapList (\ s a -> s{_decrsEventCategoriesMapList = a}) . _Default . _Coerce;

-- | The response status code.
decrsStatus :: Lens' DescribeEventCategoriesResponse Int
decrsStatus = lens _decrsStatus (\ s a -> s{_decrsStatus = a});
