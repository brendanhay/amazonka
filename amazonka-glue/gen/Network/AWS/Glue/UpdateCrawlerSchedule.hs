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
-- Module      : Network.AWS.Glue.UpdateCrawlerSchedule
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the schedule of a crawler using a @cron@ expression.
--
--
module Network.AWS.Glue.UpdateCrawlerSchedule
    (
    -- * Creating a Request
      updateCrawlerSchedule
    , UpdateCrawlerSchedule
    -- * Request Lenses
    , ucsSchedule
    , ucsCrawlerName

    -- * Destructuring the Response
    , updateCrawlerScheduleResponse
    , UpdateCrawlerScheduleResponse
    -- * Response Lenses
    , ucsrsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateCrawlerSchedule' smart constructor.
data UpdateCrawlerSchedule = UpdateCrawlerSchedule'
  { _ucsSchedule    :: !(Maybe Text)
  , _ucsCrawlerName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateCrawlerSchedule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucsSchedule' - The updated @cron@ expression used to specify the schedule (see <http://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers> . For example, to run something every day at 12:15 UTC, you would specify: @cron(15 12 * * ? *)@ .
--
-- * 'ucsCrawlerName' - Name of the crawler whose schedule to update.
updateCrawlerSchedule
    :: Text -- ^ 'ucsCrawlerName'
    -> UpdateCrawlerSchedule
updateCrawlerSchedule pCrawlerName_ =
  UpdateCrawlerSchedule'
    {_ucsSchedule = Nothing, _ucsCrawlerName = pCrawlerName_}


-- | The updated @cron@ expression used to specify the schedule (see <http://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers> . For example, to run something every day at 12:15 UTC, you would specify: @cron(15 12 * * ? *)@ .
ucsSchedule :: Lens' UpdateCrawlerSchedule (Maybe Text)
ucsSchedule = lens _ucsSchedule (\ s a -> s{_ucsSchedule = a})

-- | Name of the crawler whose schedule to update.
ucsCrawlerName :: Lens' UpdateCrawlerSchedule Text
ucsCrawlerName = lens _ucsCrawlerName (\ s a -> s{_ucsCrawlerName = a})

instance AWSRequest UpdateCrawlerSchedule where
        type Rs UpdateCrawlerSchedule =
             UpdateCrawlerScheduleResponse
        request = postJSON glue
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateCrawlerScheduleResponse' <$>
                   (pure (fromEnum s)))

instance Hashable UpdateCrawlerSchedule where

instance NFData UpdateCrawlerSchedule where

instance ToHeaders UpdateCrawlerSchedule where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.UpdateCrawlerSchedule" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateCrawlerSchedule where
        toJSON UpdateCrawlerSchedule'{..}
          = object
              (catMaybes
                 [("Schedule" .=) <$> _ucsSchedule,
                  Just ("CrawlerName" .= _ucsCrawlerName)])

instance ToPath UpdateCrawlerSchedule where
        toPath = const "/"

instance ToQuery UpdateCrawlerSchedule where
        toQuery = const mempty

-- | /See:/ 'updateCrawlerScheduleResponse' smart constructor.
newtype UpdateCrawlerScheduleResponse = UpdateCrawlerScheduleResponse'
  { _ucsrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateCrawlerScheduleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucsrsResponseStatus' - -- | The response status code.
updateCrawlerScheduleResponse
    :: Int -- ^ 'ucsrsResponseStatus'
    -> UpdateCrawlerScheduleResponse
updateCrawlerScheduleResponse pResponseStatus_ =
  UpdateCrawlerScheduleResponse' {_ucsrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
ucsrsResponseStatus :: Lens' UpdateCrawlerScheduleResponse Int
ucsrsResponseStatus = lens _ucsrsResponseStatus (\ s a -> s{_ucsrsResponseStatus = a})

instance NFData UpdateCrawlerScheduleResponse where
