{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.BatchGetJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of resource metadata for a given list of job names. After calling the @ListJobs@ operation, you can call this operation to access the data to which you have been granted permissions. This operation supports all IAM permissions, including permission conditions that uses tags.
module Network.AWS.Glue.BatchGetJobs
  ( -- * Creating a Request
    batchGetJobs,
    BatchGetJobs,

    -- * Request Lenses
    bgjJobNames,

    -- * Destructuring the Response
    batchGetJobsResponse,
    BatchGetJobsResponse,

    -- * Response Lenses
    bgjrsJobs,
    bgjrsJobsNotFound,
    bgjrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'batchGetJobs' smart constructor.
newtype BatchGetJobs = BatchGetJobs' {_bgjJobNames :: [Text]}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchGetJobs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bgjJobNames' - A list of job names, which might be the names returned from the @ListJobs@ operation.
batchGetJobs ::
  BatchGetJobs
batchGetJobs = BatchGetJobs' {_bgjJobNames = mempty}

-- | A list of job names, which might be the names returned from the @ListJobs@ operation.
bgjJobNames :: Lens' BatchGetJobs [Text]
bgjJobNames = lens _bgjJobNames (\s a -> s {_bgjJobNames = a}) . _Coerce

instance AWSRequest BatchGetJobs where
  type Rs BatchGetJobs = BatchGetJobsResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          BatchGetJobsResponse'
            <$> (x .?> "Jobs" .!@ mempty)
            <*> (x .?> "JobsNotFound" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable BatchGetJobs

instance NFData BatchGetJobs

instance ToHeaders BatchGetJobs where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSGlue.BatchGetJobs" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON BatchGetJobs where
  toJSON BatchGetJobs' {..} =
    object (catMaybes [Just ("JobNames" .= _bgjJobNames)])

instance ToPath BatchGetJobs where
  toPath = const "/"

instance ToQuery BatchGetJobs where
  toQuery = const mempty

-- | /See:/ 'batchGetJobsResponse' smart constructor.
data BatchGetJobsResponse = BatchGetJobsResponse'
  { _bgjrsJobs ::
      !(Maybe [Job]),
    _bgjrsJobsNotFound :: !(Maybe [Text]),
    _bgjrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchGetJobsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bgjrsJobs' - A list of job definitions.
--
-- * 'bgjrsJobsNotFound' - A list of names of jobs not found.
--
-- * 'bgjrsResponseStatus' - -- | The response status code.
batchGetJobsResponse ::
  -- | 'bgjrsResponseStatus'
  Int ->
  BatchGetJobsResponse
batchGetJobsResponse pResponseStatus_ =
  BatchGetJobsResponse'
    { _bgjrsJobs = Nothing,
      _bgjrsJobsNotFound = Nothing,
      _bgjrsResponseStatus = pResponseStatus_
    }

-- | A list of job definitions.
bgjrsJobs :: Lens' BatchGetJobsResponse [Job]
bgjrsJobs = lens _bgjrsJobs (\s a -> s {_bgjrsJobs = a}) . _Default . _Coerce

-- | A list of names of jobs not found.
bgjrsJobsNotFound :: Lens' BatchGetJobsResponse [Text]
bgjrsJobsNotFound = lens _bgjrsJobsNotFound (\s a -> s {_bgjrsJobsNotFound = a}) . _Default . _Coerce

-- | -- | The response status code.
bgjrsResponseStatus :: Lens' BatchGetJobsResponse Int
bgjrsResponseStatus = lens _bgjrsResponseStatus (\s a -> s {_bgjrsResponseStatus = a})

instance NFData BatchGetJobsResponse
