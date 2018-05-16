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
-- Module      : Network.AWS.IoT.DescribeJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a job.
--
--
module Network.AWS.IoT.DescribeJob
    (
    -- * Creating a Request
      describeJob
    , DescribeJob
    -- * Request Lenses
    , djJobId

    -- * Destructuring the Response
    , describeJobResponse
    , DescribeJobResponse
    -- * Response Lenses
    , djrsDocumentSource
    , djrsJob
    , djrsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeJob' smart constructor.
newtype DescribeJob = DescribeJob'
  { _djJobId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'djJobId' - The unique identifier you assigned to this job when it was created.
describeJob
    :: Text -- ^ 'djJobId'
    -> DescribeJob
describeJob pJobId_ = DescribeJob' {_djJobId = pJobId_}


-- | The unique identifier you assigned to this job when it was created.
djJobId :: Lens' DescribeJob Text
djJobId = lens _djJobId (\ s a -> s{_djJobId = a})

instance AWSRequest DescribeJob where
        type Rs DescribeJob = DescribeJobResponse
        request = get ioT
        response
          = receiveJSON
              (\ s h x ->
                 DescribeJobResponse' <$>
                   (x .?> "documentSource") <*> (x .?> "job") <*>
                     (pure (fromEnum s)))

instance Hashable DescribeJob where

instance NFData DescribeJob where

instance ToHeaders DescribeJob where
        toHeaders = const mempty

instance ToPath DescribeJob where
        toPath DescribeJob'{..}
          = mconcat ["/jobs/", toBS _djJobId]

instance ToQuery DescribeJob where
        toQuery = const mempty

-- | /See:/ 'describeJobResponse' smart constructor.
data DescribeJobResponse = DescribeJobResponse'
  { _djrsDocumentSource :: !(Maybe Text)
  , _djrsJob            :: !(Maybe Job)
  , _djrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'djrsDocumentSource' - An S3 link to the job document.
--
-- * 'djrsJob' - Information about the job.
--
-- * 'djrsResponseStatus' - -- | The response status code.
describeJobResponse
    :: Int -- ^ 'djrsResponseStatus'
    -> DescribeJobResponse
describeJobResponse pResponseStatus_ =
  DescribeJobResponse'
    { _djrsDocumentSource = Nothing
    , _djrsJob = Nothing
    , _djrsResponseStatus = pResponseStatus_
    }


-- | An S3 link to the job document.
djrsDocumentSource :: Lens' DescribeJobResponse (Maybe Text)
djrsDocumentSource = lens _djrsDocumentSource (\ s a -> s{_djrsDocumentSource = a})

-- | Information about the job.
djrsJob :: Lens' DescribeJobResponse (Maybe Job)
djrsJob = lens _djrsJob (\ s a -> s{_djrsJob = a})

-- | -- | The response status code.
djrsResponseStatus :: Lens' DescribeJobResponse Int
djrsResponseStatus = lens _djrsResponseStatus (\ s a -> s{_djrsResponseStatus = a})

instance NFData DescribeJobResponse where
