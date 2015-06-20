{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudWatchLogs.DeleteMetricFilter
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

-- | Deletes a metric filter associated with the specified log group.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DeleteMetricFilter.html>
module Network.AWS.CloudWatchLogs.DeleteMetricFilter
    (
    -- * Request
      DeleteMetricFilter
    -- ** Request constructor
    , deleteMetricFilter
    -- ** Request lenses
    , dLogGroupName
    , dFilterName

    -- * Response
    , DeleteMetricFilterResponse
    -- ** Response constructor
    , deleteMetricFilterResponse
    ) where

import Network.AWS.CloudWatchLogs.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteMetricFilter' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dLogGroupName'
--
-- * 'dFilterName'
data DeleteMetricFilter = DeleteMetricFilter'{_dLogGroupName :: Text, _dFilterName :: Text} deriving (Eq, Read, Show)

-- | 'DeleteMetricFilter' smart constructor.
deleteMetricFilter :: Text -> Text -> DeleteMetricFilter
deleteMetricFilter pLogGroupName pFilterName = DeleteMetricFilter'{_dLogGroupName = pLogGroupName, _dFilterName = pFilterName};

-- | The name of the log group that is associated with the metric filter to
-- delete.
dLogGroupName :: Lens' DeleteMetricFilter Text
dLogGroupName = lens _dLogGroupName (\ s a -> s{_dLogGroupName = a});

-- | The name of the metric filter to delete.
dFilterName :: Lens' DeleteMetricFilter Text
dFilterName = lens _dFilterName (\ s a -> s{_dFilterName = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest DeleteMetricFilter where
        type Sv DeleteMetricFilter = CloudWatchLogs
        type Rs DeleteMetricFilter =
             DeleteMetricFilterResponse
        request = postJSON
        response = receiveNull DeleteMetricFilterResponse'

instance ToHeaders DeleteMetricFilter where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Logs_20140328.DeleteMetricFilter" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteMetricFilter where
        toJSON DeleteMetricFilter'{..}
          = object
              ["logGroupName" .= _dLogGroupName,
               "filterName" .= _dFilterName]

instance ToPath DeleteMetricFilter where
        toPath = const "/"

instance ToQuery DeleteMetricFilter where
        toQuery = const mempty

-- | /See:/ 'deleteMetricFilterResponse' smart constructor.
data DeleteMetricFilterResponse = DeleteMetricFilterResponse' deriving (Eq, Read, Show)

-- | 'DeleteMetricFilterResponse' smart constructor.
deleteMetricFilterResponse :: DeleteMetricFilterResponse
deleteMetricFilterResponse = DeleteMetricFilterResponse';
