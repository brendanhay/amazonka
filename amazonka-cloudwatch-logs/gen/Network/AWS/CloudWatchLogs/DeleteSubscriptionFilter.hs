{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CloudWatchLogs.DeleteSubscriptionFilter
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

-- | Deletes a subscription filter associated with the specified log group.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DeleteSubscriptionFilter.html>
module Network.AWS.CloudWatchLogs.DeleteSubscriptionFilter
    (
    -- * Request
      DeleteSubscriptionFilter
    -- ** Request constructor
    , deleteSubscriptionFilter
    -- ** Request lenses
    , delLogGroupName
    , delFilterName

    -- * Response
    , DeleteSubscriptionFilterResponse
    -- ** Response constructor
    , deleteSubscriptionFilterResponse
    ) where

import           Network.AWS.CloudWatchLogs.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteSubscriptionFilter' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'delLogGroupName'
--
-- * 'delFilterName'
data DeleteSubscriptionFilter = DeleteSubscriptionFilter'
    { _delLogGroupName :: !Text
    , _delFilterName   :: !Text
    } deriving (Eq,Read,Show)

-- | 'DeleteSubscriptionFilter' smart constructor.
deleteSubscriptionFilter :: Text -> Text -> DeleteSubscriptionFilter
deleteSubscriptionFilter pLogGroupName pFilterName =
    DeleteSubscriptionFilter'
    { _delLogGroupName = pLogGroupName
    , _delFilterName = pFilterName
    }

-- | The name of the log group that is associated with the subscription
-- filter to delete.
delLogGroupName :: Lens' DeleteSubscriptionFilter Text
delLogGroupName = lens _delLogGroupName (\ s a -> s{_delLogGroupName = a});

-- | The name of the subscription filter to delete.
delFilterName :: Lens' DeleteSubscriptionFilter Text
delFilterName = lens _delFilterName (\ s a -> s{_delFilterName = a});

instance AWSRequest DeleteSubscriptionFilter where
        type Sv DeleteSubscriptionFilter = CloudWatchLogs
        type Rs DeleteSubscriptionFilter =
             DeleteSubscriptionFilterResponse
        request = postJSON
        response
          = receiveNull DeleteSubscriptionFilterResponse'

instance ToHeaders DeleteSubscriptionFilter where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Logs_20140328.DeleteSubscriptionFilter" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteSubscriptionFilter where
        toJSON DeleteSubscriptionFilter'{..}
          = object
              ["logGroupName" .= _delLogGroupName,
               "filterName" .= _delFilterName]

instance ToPath DeleteSubscriptionFilter where
        toPath = const "/"

instance ToQuery DeleteSubscriptionFilter where
        toQuery = const mempty

-- | /See:/ 'deleteSubscriptionFilterResponse' smart constructor.
data DeleteSubscriptionFilterResponse =
    DeleteSubscriptionFilterResponse'
    deriving (Eq,Read,Show)

-- | 'DeleteSubscriptionFilterResponse' smart constructor.
deleteSubscriptionFilterResponse :: DeleteSubscriptionFilterResponse
deleteSubscriptionFilterResponse = DeleteSubscriptionFilterResponse'
