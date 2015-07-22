{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.DeleteMetricFilter
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes a metric filter associated with the specified log group.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DeleteMetricFilter.html>
module Network.AWS.CloudWatchLogs.DeleteMetricFilter
    (
    -- * Request
      DeleteMetricFilter
    -- ** Request constructor
    , deleteMetricFilter
    -- ** Request lenses
    , delrqLogGroupName
    , delrqFilterName

    -- * Response
    , DeleteMetricFilterResponse
    -- ** Response constructor
    , deleteMetricFilterResponse
    ) where

import           Network.AWS.CloudWatchLogs.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteMetricFilter' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'delrqLogGroupName'
--
-- * 'delrqFilterName'
data DeleteMetricFilter = DeleteMetricFilter'
    { _delrqLogGroupName :: !Text
    , _delrqFilterName   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteMetricFilter' smart constructor.
deleteMetricFilter :: Text -> Text -> DeleteMetricFilter
deleteMetricFilter pLogGroupName pFilterName =
    DeleteMetricFilter'
    { _delrqLogGroupName = pLogGroupName
    , _delrqFilterName = pFilterName
    }

-- | The name of the log group that is associated with the metric filter to
-- delete.
delrqLogGroupName :: Lens' DeleteMetricFilter Text
delrqLogGroupName = lens _delrqLogGroupName (\ s a -> s{_delrqLogGroupName = a});

-- | The name of the metric filter to delete.
delrqFilterName :: Lens' DeleteMetricFilter Text
delrqFilterName = lens _delrqFilterName (\ s a -> s{_delrqFilterName = a});

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
              ["logGroupName" .= _delrqLogGroupName,
               "filterName" .= _delrqFilterName]

instance ToPath DeleteMetricFilter where
        toPath = const "/"

instance ToQuery DeleteMetricFilter where
        toQuery = const mempty

-- | /See:/ 'deleteMetricFilterResponse' smart constructor.
data DeleteMetricFilterResponse =
    DeleteMetricFilterResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteMetricFilterResponse' smart constructor.
deleteMetricFilterResponse :: DeleteMetricFilterResponse
deleteMetricFilterResponse = DeleteMetricFilterResponse'
