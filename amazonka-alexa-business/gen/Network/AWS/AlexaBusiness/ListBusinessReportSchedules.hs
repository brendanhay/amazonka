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
-- Module      : Network.AWS.AlexaBusiness.ListBusinessReportSchedules
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the details of the schedules that a user configured.
--
--
--
-- This operation returns paginated results.
module Network.AWS.AlexaBusiness.ListBusinessReportSchedules
    (
    -- * Creating a Request
      listBusinessReportSchedules
    , ListBusinessReportSchedules
    -- * Request Lenses
    , lbrsNextToken
    , lbrsMaxResults

    -- * Destructuring the Response
    , listBusinessReportSchedulesResponse
    , ListBusinessReportSchedulesResponse
    -- * Response Lenses
    , lbrsrsBusinessReportSchedules
    , lbrsrsNextToken
    , lbrsrsResponseStatus
    ) where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.AlexaBusiness.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listBusinessReportSchedules' smart constructor.
data ListBusinessReportSchedules = ListBusinessReportSchedules'
  { _lbrsNextToken  :: !(Maybe Text)
  , _lbrsMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListBusinessReportSchedules' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbrsNextToken' - The token used to list the remaining schedules from the previous API call.
--
-- * 'lbrsMaxResults' - The maximum number of schedules listed in the call.
listBusinessReportSchedules
    :: ListBusinessReportSchedules
listBusinessReportSchedules =
  ListBusinessReportSchedules'
    {_lbrsNextToken = Nothing, _lbrsMaxResults = Nothing}


-- | The token used to list the remaining schedules from the previous API call.
lbrsNextToken :: Lens' ListBusinessReportSchedules (Maybe Text)
lbrsNextToken = lens _lbrsNextToken (\ s a -> s{_lbrsNextToken = a})

-- | The maximum number of schedules listed in the call.
lbrsMaxResults :: Lens' ListBusinessReportSchedules (Maybe Natural)
lbrsMaxResults = lens _lbrsMaxResults (\ s a -> s{_lbrsMaxResults = a}) . mapping _Nat

instance AWSPager ListBusinessReportSchedules where
        page rq rs
          | stop (rs ^. lbrsrsNextToken) = Nothing
          | stop (rs ^. lbrsrsBusinessReportSchedules) =
            Nothing
          | otherwise =
            Just $ rq & lbrsNextToken .~ rs ^. lbrsrsNextToken

instance AWSRequest ListBusinessReportSchedules where
        type Rs ListBusinessReportSchedules =
             ListBusinessReportSchedulesResponse
        request = postJSON alexaBusiness
        response
          = receiveJSON
              (\ s h x ->
                 ListBusinessReportSchedulesResponse' <$>
                   (x .?> "BusinessReportSchedules" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListBusinessReportSchedules where

instance NFData ListBusinessReportSchedules where

instance ToHeaders ListBusinessReportSchedules where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AlexaForBusiness.ListBusinessReportSchedules" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListBusinessReportSchedules where
        toJSON ListBusinessReportSchedules'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _lbrsNextToken,
                  ("MaxResults" .=) <$> _lbrsMaxResults])

instance ToPath ListBusinessReportSchedules where
        toPath = const "/"

instance ToQuery ListBusinessReportSchedules where
        toQuery = const mempty

-- | /See:/ 'listBusinessReportSchedulesResponse' smart constructor.
data ListBusinessReportSchedulesResponse = ListBusinessReportSchedulesResponse'
  { _lbrsrsBusinessReportSchedules :: !(Maybe [BusinessReportSchedule])
  , _lbrsrsNextToken               :: !(Maybe Text)
  , _lbrsrsResponseStatus          :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListBusinessReportSchedulesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbrsrsBusinessReportSchedules' - The schedule of the reports.
--
-- * 'lbrsrsNextToken' - The token used to list the remaining schedules from the previous API call.
--
-- * 'lbrsrsResponseStatus' - -- | The response status code.
listBusinessReportSchedulesResponse
    :: Int -- ^ 'lbrsrsResponseStatus'
    -> ListBusinessReportSchedulesResponse
listBusinessReportSchedulesResponse pResponseStatus_ =
  ListBusinessReportSchedulesResponse'
    { _lbrsrsBusinessReportSchedules = Nothing
    , _lbrsrsNextToken = Nothing
    , _lbrsrsResponseStatus = pResponseStatus_
    }


-- | The schedule of the reports.
lbrsrsBusinessReportSchedules :: Lens' ListBusinessReportSchedulesResponse [BusinessReportSchedule]
lbrsrsBusinessReportSchedules = lens _lbrsrsBusinessReportSchedules (\ s a -> s{_lbrsrsBusinessReportSchedules = a}) . _Default . _Coerce

-- | The token used to list the remaining schedules from the previous API call.
lbrsrsNextToken :: Lens' ListBusinessReportSchedulesResponse (Maybe Text)
lbrsrsNextToken = lens _lbrsrsNextToken (\ s a -> s{_lbrsrsNextToken = a})

-- | -- | The response status code.
lbrsrsResponseStatus :: Lens' ListBusinessReportSchedulesResponse Int
lbrsrsResponseStatus = lens _lbrsrsResponseStatus (\ s a -> s{_lbrsrsResponseStatus = a})

instance NFData ListBusinessReportSchedulesResponse
         where
