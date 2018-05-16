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
-- Module      : Network.AWS.Glue.GetJobs
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all current job definitions.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetJobs
    (
    -- * Creating a Request
      getJobs
    , GetJobs
    -- * Request Lenses
    , gjNextToken
    , gjMaxResults

    -- * Destructuring the Response
    , getJobsResponse
    , GetJobsResponse
    -- * Response Lenses
    , gjsrsNextToken
    , gjsrsJobs
    , gjsrsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getJobs' smart constructor.
data GetJobs = GetJobs'
  { _gjNextToken  :: !(Maybe Text)
  , _gjMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetJobs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gjNextToken' - A continuation token, if this is a continuation call.
--
-- * 'gjMaxResults' - The maximum size of the response.
getJobs
    :: GetJobs
getJobs = GetJobs' {_gjNextToken = Nothing, _gjMaxResults = Nothing}


-- | A continuation token, if this is a continuation call.
gjNextToken :: Lens' GetJobs (Maybe Text)
gjNextToken = lens _gjNextToken (\ s a -> s{_gjNextToken = a})

-- | The maximum size of the response.
gjMaxResults :: Lens' GetJobs (Maybe Natural)
gjMaxResults = lens _gjMaxResults (\ s a -> s{_gjMaxResults = a}) . mapping _Nat

instance AWSPager GetJobs where
        page rq rs
          | stop (rs ^. gjsrsNextToken) = Nothing
          | stop (rs ^. gjsrsJobs) = Nothing
          | otherwise =
            Just $ rq & gjNextToken .~ rs ^. gjsrsNextToken

instance AWSRequest GetJobs where
        type Rs GetJobs = GetJobsResponse
        request = postJSON glue
        response
          = receiveJSON
              (\ s h x ->
                 GetJobsResponse' <$>
                   (x .?> "NextToken") <*> (x .?> "Jobs" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable GetJobs where

instance NFData GetJobs where

instance ToHeaders GetJobs where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =# ("AWSGlue.GetJobs" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetJobs where
        toJSON GetJobs'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _gjNextToken,
                  ("MaxResults" .=) <$> _gjMaxResults])

instance ToPath GetJobs where
        toPath = const "/"

instance ToQuery GetJobs where
        toQuery = const mempty

-- | /See:/ 'getJobsResponse' smart constructor.
data GetJobsResponse = GetJobsResponse'
  { _gjsrsNextToken      :: !(Maybe Text)
  , _gjsrsJobs           :: !(Maybe [Job])
  , _gjsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetJobsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gjsrsNextToken' - A continuation token, if not all job definitions have yet been returned.
--
-- * 'gjsrsJobs' - A list of job definitions.
--
-- * 'gjsrsResponseStatus' - -- | The response status code.
getJobsResponse
    :: Int -- ^ 'gjsrsResponseStatus'
    -> GetJobsResponse
getJobsResponse pResponseStatus_ =
  GetJobsResponse'
    { _gjsrsNextToken = Nothing
    , _gjsrsJobs = Nothing
    , _gjsrsResponseStatus = pResponseStatus_
    }


-- | A continuation token, if not all job definitions have yet been returned.
gjsrsNextToken :: Lens' GetJobsResponse (Maybe Text)
gjsrsNextToken = lens _gjsrsNextToken (\ s a -> s{_gjsrsNextToken = a})

-- | A list of job definitions.
gjsrsJobs :: Lens' GetJobsResponse [Job]
gjsrsJobs = lens _gjsrsJobs (\ s a -> s{_gjsrsJobs = a}) . _Default . _Coerce

-- | -- | The response status code.
gjsrsResponseStatus :: Lens' GetJobsResponse Int
gjsrsResponseStatus = lens _gjsrsResponseStatus (\ s a -> s{_gjsrsResponseStatus = a})

instance NFData GetJobsResponse where
