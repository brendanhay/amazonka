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
-- Module      : Network.AWS.DMS.DescribeEventCategories
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists categories for all event source types, or, if specified, for a specified source type. You can see a list of the event categories and source types in <http://docs.aws.amazon.com/dms/latest/userguide/CHAP_Events.html Working with Events and Notifications > in the AWS Database Migration Service User Guide.
--
--
module Network.AWS.DMS.DescribeEventCategories
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
    , decrsEventCategoryGroupList
    , decrsResponseStatus
    ) where

import Network.AWS.DMS.Types
import Network.AWS.DMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
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
-- * 'decSourceType' - The type of AWS DMS resource that generates events.  Valid values: replication-instance | migration-task
--
-- * 'decFilters' - Filters applied to the action.
describeEventCategories
    :: DescribeEventCategories
describeEventCategories =
  DescribeEventCategories' {_decSourceType = Nothing, _decFilters = Nothing}


-- | The type of AWS DMS resource that generates events.  Valid values: replication-instance | migration-task
decSourceType :: Lens' DescribeEventCategories (Maybe Text)
decSourceType = lens _decSourceType (\ s a -> s{_decSourceType = a})

-- | Filters applied to the action.
decFilters :: Lens' DescribeEventCategories [Filter]
decFilters = lens _decFilters (\ s a -> s{_decFilters = a}) . _Default . _Coerce

instance AWSRequest DescribeEventCategories where
        type Rs DescribeEventCategories =
             DescribeEventCategoriesResponse
        request = postJSON dms
        response
          = receiveJSON
              (\ s h x ->
                 DescribeEventCategoriesResponse' <$>
                   (x .?> "EventCategoryGroupList" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable DescribeEventCategories where

instance NFData DescribeEventCategories where

instance ToHeaders DescribeEventCategories where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonDMSv20160101.DescribeEventCategories" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeEventCategories where
        toJSON DescribeEventCategories'{..}
          = object
              (catMaybes
                 [("SourceType" .=) <$> _decSourceType,
                  ("Filters" .=) <$> _decFilters])

instance ToPath DescribeEventCategories where
        toPath = const "/"

instance ToQuery DescribeEventCategories where
        toQuery = const mempty

-- |
--
--
--
-- /See:/ 'describeEventCategoriesResponse' smart constructor.
data DescribeEventCategoriesResponse = DescribeEventCategoriesResponse'
  { _decrsEventCategoryGroupList :: !(Maybe [EventCategoryGroup])
  , _decrsResponseStatus         :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEventCategoriesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'decrsEventCategoryGroupList' - A list of event categories.
--
-- * 'decrsResponseStatus' - -- | The response status code.
describeEventCategoriesResponse
    :: Int -- ^ 'decrsResponseStatus'
    -> DescribeEventCategoriesResponse
describeEventCategoriesResponse pResponseStatus_ =
  DescribeEventCategoriesResponse'
    { _decrsEventCategoryGroupList = Nothing
    , _decrsResponseStatus = pResponseStatus_
    }


-- | A list of event categories.
decrsEventCategoryGroupList :: Lens' DescribeEventCategoriesResponse [EventCategoryGroup]
decrsEventCategoryGroupList = lens _decrsEventCategoryGroupList (\ s a -> s{_decrsEventCategoryGroupList = a}) . _Default . _Coerce

-- | -- | The response status code.
decrsResponseStatus :: Lens' DescribeEventCategoriesResponse Int
decrsResponseStatus = lens _decrsResponseStatus (\ s a -> s{_decrsResponseStatus = a})

instance NFData DescribeEventCategoriesResponse where
