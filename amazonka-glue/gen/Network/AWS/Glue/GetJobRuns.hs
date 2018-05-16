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
-- Module      : Network.AWS.Glue.GetJobRuns
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves metadata for all runs of a given job definition.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetJobRuns
    (
    -- * Creating a Request
      getJobRuns
    , GetJobRuns
    -- * Request Lenses
    , gjrNextToken
    , gjrMaxResults
    , gjrJobName

    -- * Destructuring the Response
    , getJobRunsResponse
    , GetJobRunsResponse
    -- * Response Lenses
    , gjrrsNextToken
    , gjrrsJobRuns
    , gjrrsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getJobRuns' smart constructor.
data GetJobRuns = GetJobRuns'
  { _gjrNextToken  :: !(Maybe Text)
  , _gjrMaxResults :: !(Maybe Nat)
  , _gjrJobName    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetJobRuns' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gjrNextToken' - A continuation token, if this is a continuation call.
--
-- * 'gjrMaxResults' - The maximum size of the response.
--
-- * 'gjrJobName' - The name of the job definition for which to retrieve all job runs.
getJobRuns
    :: Text -- ^ 'gjrJobName'
    -> GetJobRuns
getJobRuns pJobName_ =
  GetJobRuns'
    {_gjrNextToken = Nothing, _gjrMaxResults = Nothing, _gjrJobName = pJobName_}


-- | A continuation token, if this is a continuation call.
gjrNextToken :: Lens' GetJobRuns (Maybe Text)
gjrNextToken = lens _gjrNextToken (\ s a -> s{_gjrNextToken = a})

-- | The maximum size of the response.
gjrMaxResults :: Lens' GetJobRuns (Maybe Natural)
gjrMaxResults = lens _gjrMaxResults (\ s a -> s{_gjrMaxResults = a}) . mapping _Nat

-- | The name of the job definition for which to retrieve all job runs.
gjrJobName :: Lens' GetJobRuns Text
gjrJobName = lens _gjrJobName (\ s a -> s{_gjrJobName = a})

instance AWSPager GetJobRuns where
        page rq rs
          | stop (rs ^. gjrrsNextToken) = Nothing
          | stop (rs ^. gjrrsJobRuns) = Nothing
          | otherwise =
            Just $ rq & gjrNextToken .~ rs ^. gjrrsNextToken

instance AWSRequest GetJobRuns where
        type Rs GetJobRuns = GetJobRunsResponse
        request = postJSON glue
        response
          = receiveJSON
              (\ s h x ->
                 GetJobRunsResponse' <$>
                   (x .?> "NextToken") <*> (x .?> "JobRuns" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetJobRuns where

instance NFData GetJobRuns where

instance ToHeaders GetJobRuns where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.GetJobRuns" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetJobRuns where
        toJSON GetJobRuns'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _gjrNextToken,
                  ("MaxResults" .=) <$> _gjrMaxResults,
                  Just ("JobName" .= _gjrJobName)])

instance ToPath GetJobRuns where
        toPath = const "/"

instance ToQuery GetJobRuns where
        toQuery = const mempty

-- | /See:/ 'getJobRunsResponse' smart constructor.
data GetJobRunsResponse = GetJobRunsResponse'
  { _gjrrsNextToken      :: !(Maybe Text)
  , _gjrrsJobRuns        :: !(Maybe [JobRun])
  , _gjrrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetJobRunsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gjrrsNextToken' - A continuation token, if not all reequested job runs have been returned.
--
-- * 'gjrrsJobRuns' - A list of job-run metatdata objects.
--
-- * 'gjrrsResponseStatus' - -- | The response status code.
getJobRunsResponse
    :: Int -- ^ 'gjrrsResponseStatus'
    -> GetJobRunsResponse
getJobRunsResponse pResponseStatus_ =
  GetJobRunsResponse'
    { _gjrrsNextToken = Nothing
    , _gjrrsJobRuns = Nothing
    , _gjrrsResponseStatus = pResponseStatus_
    }


-- | A continuation token, if not all reequested job runs have been returned.
gjrrsNextToken :: Lens' GetJobRunsResponse (Maybe Text)
gjrrsNextToken = lens _gjrrsNextToken (\ s a -> s{_gjrrsNextToken = a})

-- | A list of job-run metatdata objects.
gjrrsJobRuns :: Lens' GetJobRunsResponse [JobRun]
gjrrsJobRuns = lens _gjrrsJobRuns (\ s a -> s{_gjrrsJobRuns = a}) . _Default . _Coerce

-- | -- | The response status code.
gjrrsResponseStatus :: Lens' GetJobRunsResponse Int
gjrrsResponseStatus = lens _gjrrsResponseStatus (\ s a -> s{_gjrrsResponseStatus = a})

instance NFData GetJobRunsResponse where
