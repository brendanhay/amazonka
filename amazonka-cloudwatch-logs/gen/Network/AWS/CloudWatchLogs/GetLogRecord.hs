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
-- Module      : Network.AWS.CloudWatchLogs.GetLogRecord
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all the fields and values of a single log event. All fields are retrieved, even if the original query that produced the @logRecordPointer@ retrieved only a subset of fields. Fields are returned as field name/field value pairs.
--
--
-- Additionally, the entire unparsed log event is returned within @@message@ .
--
module Network.AWS.CloudWatchLogs.GetLogRecord
    (
    -- * Creating a Request
      getLogRecord
    , GetLogRecord
    -- * Request Lenses
    , glrLogRecordPointer

    -- * Destructuring the Response
    , getLogRecordResponse
    , GetLogRecordResponse
    -- * Response Lenses
    , glrrsLogRecord
    , glrrsResponseStatus
    ) where

import Network.AWS.CloudWatchLogs.Types
import Network.AWS.CloudWatchLogs.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getLogRecord' smart constructor.
newtype GetLogRecord = GetLogRecord'
  { _glrLogRecordPointer :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetLogRecord' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'glrLogRecordPointer' - The pointer corresponding to the log event record you want to retrieve. You get this from the response of a @GetQueryResults@ operation. In that response, the value of the @@ptr@ field for a log event is the value to use as @logRecordPointer@ to retrieve that complete log event record.
getLogRecord
    :: Text -- ^ 'glrLogRecordPointer'
    -> GetLogRecord
getLogRecord pLogRecordPointer_ =
  GetLogRecord' {_glrLogRecordPointer = pLogRecordPointer_}


-- | The pointer corresponding to the log event record you want to retrieve. You get this from the response of a @GetQueryResults@ operation. In that response, the value of the @@ptr@ field for a log event is the value to use as @logRecordPointer@ to retrieve that complete log event record.
glrLogRecordPointer :: Lens' GetLogRecord Text
glrLogRecordPointer = lens _glrLogRecordPointer (\ s a -> s{_glrLogRecordPointer = a})

instance AWSRequest GetLogRecord where
        type Rs GetLogRecord = GetLogRecordResponse
        request = postJSON cloudWatchLogs
        response
          = receiveJSON
              (\ s h x ->
                 GetLogRecordResponse' <$>
                   (x .?> "logRecord" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable GetLogRecord where

instance NFData GetLogRecord where

instance ToHeaders GetLogRecord where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Logs_20140328.GetLogRecord" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetLogRecord where
        toJSON GetLogRecord'{..}
          = object
              (catMaybes
                 [Just ("logRecordPointer" .= _glrLogRecordPointer)])

instance ToPath GetLogRecord where
        toPath = const "/"

instance ToQuery GetLogRecord where
        toQuery = const mempty

-- | /See:/ 'getLogRecordResponse' smart constructor.
data GetLogRecordResponse = GetLogRecordResponse'
  { _glrrsLogRecord      :: !(Maybe (Map Text Text))
  , _glrrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetLogRecordResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'glrrsLogRecord' - The requested log event, as a JSON string.
--
-- * 'glrrsResponseStatus' - -- | The response status code.
getLogRecordResponse
    :: Int -- ^ 'glrrsResponseStatus'
    -> GetLogRecordResponse
getLogRecordResponse pResponseStatus_ =
  GetLogRecordResponse'
    {_glrrsLogRecord = Nothing, _glrrsResponseStatus = pResponseStatus_}


-- | The requested log event, as a JSON string.
glrrsLogRecord :: Lens' GetLogRecordResponse (HashMap Text Text)
glrrsLogRecord = lens _glrrsLogRecord (\ s a -> s{_glrrsLogRecord = a}) . _Default . _Map

-- | -- | The response status code.
glrrsResponseStatus :: Lens' GetLogRecordResponse Int
glrrsResponseStatus = lens _glrrsResponseStatus (\ s a -> s{_glrrsResponseStatus = a})

instance NFData GetLogRecordResponse where
