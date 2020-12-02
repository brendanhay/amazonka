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
-- Module      : Network.AWS.Snowball.DescribeJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific job including shipping information, job status, and other important metadata.
--
--
module Network.AWS.Snowball.DescribeJob
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
    , djrsJobMetadata
    , djrsSubJobMetadata
    , djrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Snowball.Types
import Network.AWS.Snowball.Types.Product

-- | /See:/ 'describeJob' smart constructor.
newtype DescribeJob = DescribeJob'
  { _djJobId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'djJobId' - The automatically generated ID for a job, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
describeJob
    :: Text -- ^ 'djJobId'
    -> DescribeJob
describeJob pJobId_ = DescribeJob' {_djJobId = pJobId_}


-- | The automatically generated ID for a job, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
djJobId :: Lens' DescribeJob Text
djJobId = lens _djJobId (\ s a -> s{_djJobId = a})

instance AWSRequest DescribeJob where
        type Rs DescribeJob = DescribeJobResponse
        request = postJSON snowball
        response
          = receiveJSON
              (\ s h x ->
                 DescribeJobResponse' <$>
                   (x .?> "JobMetadata") <*>
                     (x .?> "SubJobMetadata" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeJob where

instance NFData DescribeJob where

instance ToHeaders DescribeJob where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSIESnowballJobManagementService.DescribeJob" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeJob where
        toJSON DescribeJob'{..}
          = object (catMaybes [Just ("JobId" .= _djJobId)])

instance ToPath DescribeJob where
        toPath = const "/"

instance ToQuery DescribeJob where
        toQuery = const mempty

-- | /See:/ 'describeJobResponse' smart constructor.
data DescribeJobResponse = DescribeJobResponse'
  { _djrsJobMetadata    :: !(Maybe JobMetadata)
  , _djrsSubJobMetadata :: !(Maybe [JobMetadata])
  , _djrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'djrsJobMetadata' - Information about a specific job, including shipping information, job status, and other important metadata.
--
-- * 'djrsSubJobMetadata' - Information about a specific job part (in the case of an export job), including shipping information, job status, and other important metadata.
--
-- * 'djrsResponseStatus' - -- | The response status code.
describeJobResponse
    :: Int -- ^ 'djrsResponseStatus'
    -> DescribeJobResponse
describeJobResponse pResponseStatus_ =
  DescribeJobResponse'
    { _djrsJobMetadata = Nothing
    , _djrsSubJobMetadata = Nothing
    , _djrsResponseStatus = pResponseStatus_
    }


-- | Information about a specific job, including shipping information, job status, and other important metadata.
djrsJobMetadata :: Lens' DescribeJobResponse (Maybe JobMetadata)
djrsJobMetadata = lens _djrsJobMetadata (\ s a -> s{_djrsJobMetadata = a})

-- | Information about a specific job part (in the case of an export job), including shipping information, job status, and other important metadata.
djrsSubJobMetadata :: Lens' DescribeJobResponse [JobMetadata]
djrsSubJobMetadata = lens _djrsSubJobMetadata (\ s a -> s{_djrsSubJobMetadata = a}) . _Default . _Coerce

-- | -- | The response status code.
djrsResponseStatus :: Lens' DescribeJobResponse Int
djrsResponseStatus = lens _djrsResponseStatus (\ s a -> s{_djrsResponseStatus = a})

instance NFData DescribeJobResponse where
