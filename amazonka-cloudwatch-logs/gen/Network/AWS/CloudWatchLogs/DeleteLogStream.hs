{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.DeleteLogStream
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes a log stream and permanently deletes all the archived log events
-- associated with it.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DeleteLogStream.html>
module Network.AWS.CloudWatchLogs.DeleteLogStream
    (
    -- * Request
      DeleteLogStream
    -- ** Request constructor
    , deleteLogStream
    -- ** Request lenses
    , dlsLogGroupName
    , dlsLogStreamName

    -- * Response
    , DeleteLogStreamResponse
    -- ** Response constructor
    , deleteLogStreamResponse
    ) where

import           Network.AWS.CloudWatchLogs.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteLogStream' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlsLogGroupName'
--
-- * 'dlsLogStreamName'
data DeleteLogStream = DeleteLogStream'
    { _dlsLogGroupName  :: !Text
    , _dlsLogStreamName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteLogStream' smart constructor.
deleteLogStream :: Text -> Text -> DeleteLogStream
deleteLogStream pLogGroupName pLogStreamName =
    DeleteLogStream'
    { _dlsLogGroupName = pLogGroupName
    , _dlsLogStreamName = pLogStreamName
    }

-- | The name of the log group under which the log stream to delete belongs.
dlsLogGroupName :: Lens' DeleteLogStream Text
dlsLogGroupName = lens _dlsLogGroupName (\ s a -> s{_dlsLogGroupName = a});

-- | The name of the log stream to delete.
dlsLogStreamName :: Lens' DeleteLogStream Text
dlsLogStreamName = lens _dlsLogStreamName (\ s a -> s{_dlsLogStreamName = a});

instance AWSRequest DeleteLogStream where
        type Sv DeleteLogStream = CloudWatchLogs
        type Rs DeleteLogStream = DeleteLogStreamResponse
        request = postJSON
        response = receiveNull DeleteLogStreamResponse'

instance ToHeaders DeleteLogStream where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Logs_20140328.DeleteLogStream" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteLogStream where
        toJSON DeleteLogStream'{..}
          = object
              ["logGroupName" .= _dlsLogGroupName,
               "logStreamName" .= _dlsLogStreamName]

instance ToPath DeleteLogStream where
        toPath = const "/"

instance ToQuery DeleteLogStream where
        toQuery = const mempty

-- | /See:/ 'deleteLogStreamResponse' smart constructor.
data DeleteLogStreamResponse =
    DeleteLogStreamResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteLogStreamResponse' smart constructor.
deleteLogStreamResponse :: DeleteLogStreamResponse
deleteLogStreamResponse = DeleteLogStreamResponse'
