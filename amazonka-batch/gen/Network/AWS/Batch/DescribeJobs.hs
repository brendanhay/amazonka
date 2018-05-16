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
-- Module      : Network.AWS.Batch.DescribeJobs
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a list of AWS Batch jobs.
--
--
module Network.AWS.Batch.DescribeJobs
    (
    -- * Creating a Request
      describeJobs
    , DescribeJobs
    -- * Request Lenses
    , djJobs

    -- * Destructuring the Response
    , describeJobsResponse
    , DescribeJobsResponse
    -- * Response Lenses
    , djrsJobs
    , djrsResponseStatus
    ) where

import Network.AWS.Batch.Types
import Network.AWS.Batch.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeJobs' smart constructor.
newtype DescribeJobs = DescribeJobs'
  { _djJobs :: [Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeJobs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'djJobs' - A space-separated list of up to 100 job IDs.
describeJobs
    :: DescribeJobs
describeJobs = DescribeJobs' {_djJobs = mempty}


-- | A space-separated list of up to 100 job IDs.
djJobs :: Lens' DescribeJobs [Text]
djJobs = lens _djJobs (\ s a -> s{_djJobs = a}) . _Coerce

instance AWSRequest DescribeJobs where
        type Rs DescribeJobs = DescribeJobsResponse
        request = postJSON batch
        response
          = receiveJSON
              (\ s h x ->
                 DescribeJobsResponse' <$>
                   (x .?> "jobs" .!@ mempty) <*> (pure (fromEnum s)))

instance Hashable DescribeJobs where

instance NFData DescribeJobs where

instance ToHeaders DescribeJobs where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeJobs where
        toJSON DescribeJobs'{..}
          = object (catMaybes [Just ("jobs" .= _djJobs)])

instance ToPath DescribeJobs where
        toPath = const "/v1/describejobs"

instance ToQuery DescribeJobs where
        toQuery = const mempty

-- | /See:/ 'describeJobsResponse' smart constructor.
data DescribeJobsResponse = DescribeJobsResponse'
  { _djrsJobs           :: !(Maybe [JobDetail])
  , _djrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeJobsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'djrsJobs' - The list of jobs.
--
-- * 'djrsResponseStatus' - -- | The response status code.
describeJobsResponse
    :: Int -- ^ 'djrsResponseStatus'
    -> DescribeJobsResponse
describeJobsResponse pResponseStatus_ =
  DescribeJobsResponse'
    {_djrsJobs = Nothing, _djrsResponseStatus = pResponseStatus_}


-- | The list of jobs.
djrsJobs :: Lens' DescribeJobsResponse [JobDetail]
djrsJobs = lens _djrsJobs (\ s a -> s{_djrsJobs = a}) . _Default . _Coerce

-- | -- | The response status code.
djrsResponseStatus :: Lens' DescribeJobsResponse Int
djrsResponseStatus = lens _djrsResponseStatus (\ s a -> s{_djrsResponseStatus = a})

instance NFData DescribeJobsResponse where
