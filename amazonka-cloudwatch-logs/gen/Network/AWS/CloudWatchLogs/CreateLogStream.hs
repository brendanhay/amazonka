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
-- Module      : Network.AWS.CloudWatchLogs.CreateLogStream
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new log stream in the specified log group. The name of the log
-- stream must be unique within the log group. There is no limit on the
-- number of log streams that can exist in a log group.
--
-- You must use the following guidelines when naming a log stream:
--
-- -   Log stream names can be between 1 and 512 characters long.
-- -   The \':\' colon character is not allowed.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_CreateLogStream.html AWS API Reference> for CreateLogStream.
module Network.AWS.CloudWatchLogs.CreateLogStream
    (
    -- * Creating a Request
      createLogStream
    , CreateLogStream
    -- * Request Lenses
    , clsLogGroupName
    , clsLogStreamName

    -- * Destructuring the Response
    , createLogStreamResponse
    , CreateLogStreamResponse
    ) where

import           Network.AWS.CloudWatchLogs.Types
import           Network.AWS.CloudWatchLogs.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createLogStream' smart constructor.
data CreateLogStream = CreateLogStream'
    { _clsLogGroupName  :: !Text
    , _clsLogStreamName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateLogStream' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clsLogGroupName'
--
-- * 'clsLogStreamName'
createLogStream
    :: Text -- ^ 'clsLogGroupName'
    -> Text -- ^ 'clsLogStreamName'
    -> CreateLogStream
createLogStream pLogGroupName_ pLogStreamName_ =
    CreateLogStream'
    { _clsLogGroupName = pLogGroupName_
    , _clsLogStreamName = pLogStreamName_
    }

-- | The name of the log group under which the log stream is to be created.
clsLogGroupName :: Lens' CreateLogStream Text
clsLogGroupName = lens _clsLogGroupName (\ s a -> s{_clsLogGroupName = a});

-- | The name of the log stream to create.
clsLogStreamName :: Lens' CreateLogStream Text
clsLogStreamName = lens _clsLogStreamName (\ s a -> s{_clsLogStreamName = a});

instance AWSRequest CreateLogStream where
        type Rs CreateLogStream = CreateLogStreamResponse
        request = postJSON cloudWatchLogs
        response = receiveNull CreateLogStreamResponse'

instance ToHeaders CreateLogStream where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Logs_20140328.CreateLogStream" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateLogStream where
        toJSON CreateLogStream'{..}
          = object
              (catMaybes
                 [Just ("logGroupName" .= _clsLogGroupName),
                  Just ("logStreamName" .= _clsLogStreamName)])

instance ToPath CreateLogStream where
        toPath = const "/"

instance ToQuery CreateLogStream where
        toQuery = const mempty

-- | /See:/ 'createLogStreamResponse' smart constructor.
data CreateLogStreamResponse =
    CreateLogStreamResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateLogStreamResponse' with the minimum fields required to make a request.
--
createLogStreamResponse
    :: CreateLogStreamResponse
createLogStreamResponse = CreateLogStreamResponse'
