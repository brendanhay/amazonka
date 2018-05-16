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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays a list of categories for all event source types, or, if specified, for a specified source type. You can see a list of the event categories and source types in the <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Events.html Events> topic in the /Amazon RDS User Guide./
--
--
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
    , decrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.RDS.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'describeEventCategories' smart constructor.
data DescribeEventCategories = DescribeEventCategories'
  { _decSourceType :: !(Maybe Text)
  , _decFilters    :: !(Maybe [Filter])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEventCategories' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'decSourceType' - The type of source that is generating the events. Valid values: db-instance | db-parameter-group | db-security-group | db-snapshot
--
-- * 'decFilters' - This parameter is not currently supported.
describeEventCategories
    :: DescribeEventCategories
describeEventCategories =
  DescribeEventCategories' {_decSourceType = Nothing, _decFilters = Nothing}


-- | The type of source that is generating the events. Valid values: db-instance | db-parameter-group | db-security-group | db-snapshot
decSourceType :: Lens' DescribeEventCategories (Maybe Text)
decSourceType = lens _decSourceType (\ s a -> s{_decSourceType = a})

-- | This parameter is not currently supported.
decFilters :: Lens' DescribeEventCategories [Filter]
decFilters = lens _decFilters (\ s a -> s{_decFilters = a}) . _Default . _Coerce

instance AWSRequest DescribeEventCategories where
        type Rs DescribeEventCategories =
             DescribeEventCategoriesResponse
        request = postQuery rds
        response
          = receiveXMLWrapper "DescribeEventCategoriesResult"
              (\ s h x ->
                 DescribeEventCategoriesResponse' <$>
                   (x .@? "EventCategoriesMapList" .!@ mempty >>=
                      may (parseXMLList "EventCategoriesMap"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeEventCategories where

instance NFData DescribeEventCategories where

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
--
--
-- /See:/ 'describeEventCategoriesResponse' smart constructor.
data DescribeEventCategoriesResponse = DescribeEventCategoriesResponse'
  { _decrsEventCategoriesMapList :: !(Maybe [EventCategoriesMap])
  , _decrsResponseStatus         :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEventCategoriesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'decrsEventCategoriesMapList' - A list of EventCategoriesMap data types.
--
-- * 'decrsResponseStatus' - -- | The response status code.
describeEventCategoriesResponse
    :: Int -- ^ 'decrsResponseStatus'
    -> DescribeEventCategoriesResponse
describeEventCategoriesResponse pResponseStatus_ =
  DescribeEventCategoriesResponse'
    { _decrsEventCategoriesMapList = Nothing
    , _decrsResponseStatus = pResponseStatus_
    }


-- | A list of EventCategoriesMap data types.
decrsEventCategoriesMapList :: Lens' DescribeEventCategoriesResponse [EventCategoriesMap]
decrsEventCategoriesMapList = lens _decrsEventCategoriesMapList (\ s a -> s{_decrsEventCategoriesMapList = a}) . _Default . _Coerce

-- | -- | The response status code.
decrsResponseStatus :: Lens' DescribeEventCategoriesResponse Int
decrsResponseStatus = lens _decrsResponseStatus (\ s a -> s{_decrsResponseStatus = a})

instance NFData DescribeEventCategoriesResponse where
