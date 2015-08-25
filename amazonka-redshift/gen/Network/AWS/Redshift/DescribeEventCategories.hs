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
-- Module      : Network.AWS.Redshift.DescribeEventCategories
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays a list of event categories for all event source types, or for a
-- specified source type. For a list of the event categories and source
-- types, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-event-notifications.html Amazon Redshift Event Notifications>.
--
-- /See:/ <http://docs.aws.amazon.com/redshift/latest/APIReference/API_DescribeEventCategories.html AWS API Reference> for DescribeEventCategories.
module Network.AWS.Redshift.DescribeEventCategories
    (
    -- * Creating a Request
      describeEventCategories
    , DescribeEventCategories
    -- * Request Lenses
    , decSourceType

    -- * Destructuring the Response
    , describeEventCategoriesResponse
    , DescribeEventCategoriesResponse
    -- * Response Lenses
    , decrsEventCategoriesMapList
    , decrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types
import           Network.AWS.Redshift.Types.Product
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'describeEventCategories' smart constructor.
newtype DescribeEventCategories = DescribeEventCategories'
    { _decSourceType :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeEventCategories' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'decSourceType'
describeEventCategories
    :: DescribeEventCategories
describeEventCategories =
    DescribeEventCategories'
    { _decSourceType = Nothing
    }

-- | The source type, such as cluster or parameter group, to which the
-- described event categories apply.
--
-- Valid values: cluster, snapshot, parameter group, and security group.
decSourceType :: Lens' DescribeEventCategories (Maybe Text)
decSourceType = lens _decSourceType (\ s a -> s{_decSourceType = a});

instance AWSRequest DescribeEventCategories where
        type Rs DescribeEventCategories =
             DescribeEventCategoriesResponse
        request = postQuery redshift
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
               "Version" =: ("2012-12-01" :: ByteString),
               "SourceType" =: _decSourceType]

-- |
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

-- | A list of event categories descriptions.
decrsEventCategoriesMapList :: Lens' DescribeEventCategoriesResponse [EventCategoriesMap]
decrsEventCategoriesMapList = lens _decrsEventCategoriesMapList (\ s a -> s{_decrsEventCategoriesMapList = a}) . _Default . _Coerce;

-- | The response status code.
decrsStatus :: Lens' DescribeEventCategoriesResponse Int
decrsStatus = lens _decrsStatus (\ s a -> s{_decrsStatus = a});
