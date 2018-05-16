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
-- Module      : Network.AWS.Glue.StopCrawlerSchedule
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the schedule state of the specified crawler to @NOT_SCHEDULED@ , but does not stop the crawler if it is already running.
--
--
module Network.AWS.Glue.StopCrawlerSchedule
    (
    -- * Creating a Request
      stopCrawlerSchedule
    , StopCrawlerSchedule
    -- * Request Lenses
    , sCrawlerName

    -- * Destructuring the Response
    , stopCrawlerScheduleResponse
    , StopCrawlerScheduleResponse
    -- * Response Lenses
    , storsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'stopCrawlerSchedule' smart constructor.
newtype StopCrawlerSchedule = StopCrawlerSchedule'
  { _sCrawlerName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopCrawlerSchedule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sCrawlerName' - Name of the crawler whose schedule state to set.
stopCrawlerSchedule
    :: Text -- ^ 'sCrawlerName'
    -> StopCrawlerSchedule
stopCrawlerSchedule pCrawlerName_ =
  StopCrawlerSchedule' {_sCrawlerName = pCrawlerName_}


-- | Name of the crawler whose schedule state to set.
sCrawlerName :: Lens' StopCrawlerSchedule Text
sCrawlerName = lens _sCrawlerName (\ s a -> s{_sCrawlerName = a})

instance AWSRequest StopCrawlerSchedule where
        type Rs StopCrawlerSchedule =
             StopCrawlerScheduleResponse
        request = postJSON glue
        response
          = receiveEmpty
              (\ s h x ->
                 StopCrawlerScheduleResponse' <$> (pure (fromEnum s)))

instance Hashable StopCrawlerSchedule where

instance NFData StopCrawlerSchedule where

instance ToHeaders StopCrawlerSchedule where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.StopCrawlerSchedule" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StopCrawlerSchedule where
        toJSON StopCrawlerSchedule'{..}
          = object
              (catMaybes [Just ("CrawlerName" .= _sCrawlerName)])

instance ToPath StopCrawlerSchedule where
        toPath = const "/"

instance ToQuery StopCrawlerSchedule where
        toQuery = const mempty

-- | /See:/ 'stopCrawlerScheduleResponse' smart constructor.
newtype StopCrawlerScheduleResponse = StopCrawlerScheduleResponse'
  { _storsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopCrawlerScheduleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'storsResponseStatus' - -- | The response status code.
stopCrawlerScheduleResponse
    :: Int -- ^ 'storsResponseStatus'
    -> StopCrawlerScheduleResponse
stopCrawlerScheduleResponse pResponseStatus_ =
  StopCrawlerScheduleResponse' {_storsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
storsResponseStatus :: Lens' StopCrawlerScheduleResponse Int
storsResponseStatus = lens _storsResponseStatus (\ s a -> s{_storsResponseStatus = a})

instance NFData StopCrawlerScheduleResponse where
