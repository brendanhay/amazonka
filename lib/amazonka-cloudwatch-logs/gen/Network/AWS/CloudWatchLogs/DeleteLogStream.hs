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
-- Module      : Network.AWS.CloudWatchLogs.DeleteLogStream
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified log stream and permanently deletes all the archived log events associated with the log stream.
--
--
module Network.AWS.CloudWatchLogs.DeleteLogStream
    (
    -- * Creating a Request
      deleteLogStream
    , DeleteLogStream
    -- * Request Lenses
    , dlsLogGroupName
    , dlsLogStreamName

    -- * Destructuring the Response
    , deleteLogStreamResponse
    , DeleteLogStreamResponse
    ) where

import Network.AWS.CloudWatchLogs.Types
import Network.AWS.CloudWatchLogs.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteLogStream' smart constructor.
data DeleteLogStream = DeleteLogStream'
  { _dlsLogGroupName  :: !Text
  , _dlsLogStreamName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteLogStream' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlsLogGroupName' - The name of the log group.
--
-- * 'dlsLogStreamName' - The name of the log stream.
deleteLogStream
    :: Text -- ^ 'dlsLogGroupName'
    -> Text -- ^ 'dlsLogStreamName'
    -> DeleteLogStream
deleteLogStream pLogGroupName_ pLogStreamName_ =
  DeleteLogStream'
    {_dlsLogGroupName = pLogGroupName_, _dlsLogStreamName = pLogStreamName_}


-- | The name of the log group.
dlsLogGroupName :: Lens' DeleteLogStream Text
dlsLogGroupName = lens _dlsLogGroupName (\ s a -> s{_dlsLogGroupName = a})

-- | The name of the log stream.
dlsLogStreamName :: Lens' DeleteLogStream Text
dlsLogStreamName = lens _dlsLogStreamName (\ s a -> s{_dlsLogStreamName = a})

instance AWSRequest DeleteLogStream where
        type Rs DeleteLogStream = DeleteLogStreamResponse
        request = postJSON cloudWatchLogs
        response = receiveNull DeleteLogStreamResponse'

instance Hashable DeleteLogStream where

instance NFData DeleteLogStream where

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
              (catMaybes
                 [Just ("logGroupName" .= _dlsLogGroupName),
                  Just ("logStreamName" .= _dlsLogStreamName)])

instance ToPath DeleteLogStream where
        toPath = const "/"

instance ToQuery DeleteLogStream where
        toQuery = const mempty

-- | /See:/ 'deleteLogStreamResponse' smart constructor.
data DeleteLogStreamResponse =
  DeleteLogStreamResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteLogStreamResponse' with the minimum fields required to make a request.
--
deleteLogStreamResponse
    :: DeleteLogStreamResponse
deleteLogStreamResponse = DeleteLogStreamResponse'


instance NFData DeleteLogStreamResponse where
