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
-- Module      : Network.AWS.Glue.ListJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the names of all job resources in this AWS account, or the resources with the specified tag. This operation allows you to see which resources are available in your account, and their names.
--
--
-- This operation takes the optional @Tags@ field, which you can use as a filter on the response so that tagged resources can be retrieved as a group. If you choose to use tags filtering, only resources with the tag are retrieved.
module Network.AWS.Glue.ListJobs
  ( -- * Creating a Request
    listJobs,
    ListJobs,

    -- * Request Lenses
    ljNextToken,
    ljMaxResults,
    ljTags,

    -- * Destructuring the Response
    listJobsResponse,
    ListJobsResponse,

    -- * Response Lenses
    ljrsNextToken,
    ljrsJobNames,
    ljrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listJobs' smart constructor.
data ListJobs = ListJobs'
  { _ljNextToken :: !(Maybe Text),
    _ljMaxResults :: !(Maybe Nat),
    _ljTags :: !(Maybe (Map Text (Text)))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListJobs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ljNextToken' - A continuation token, if this is a continuation request.
--
-- * 'ljMaxResults' - The maximum size of a list to return.
--
-- * 'ljTags' - Specifies to return only these tagged resources.
listJobs ::
  ListJobs
listJobs =
  ListJobs'
    { _ljNextToken = Nothing,
      _ljMaxResults = Nothing,
      _ljTags = Nothing
    }

-- | A continuation token, if this is a continuation request.
ljNextToken :: Lens' ListJobs (Maybe Text)
ljNextToken = lens _ljNextToken (\s a -> s {_ljNextToken = a})

-- | The maximum size of a list to return.
ljMaxResults :: Lens' ListJobs (Maybe Natural)
ljMaxResults = lens _ljMaxResults (\s a -> s {_ljMaxResults = a}) . mapping _Nat

-- | Specifies to return only these tagged resources.
ljTags :: Lens' ListJobs (HashMap Text (Text))
ljTags = lens _ljTags (\s a -> s {_ljTags = a}) . _Default . _Map

instance AWSRequest ListJobs where
  type Rs ListJobs = ListJobsResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          ListJobsResponse'
            <$> (x .?> "NextToken")
            <*> (x .?> "JobNames" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable ListJobs

instance NFData ListJobs

instance ToHeaders ListJobs where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSGlue.ListJobs" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListJobs where
  toJSON ListJobs' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _ljNextToken,
            ("MaxResults" .=) <$> _ljMaxResults,
            ("Tags" .=) <$> _ljTags
          ]
      )

instance ToPath ListJobs where
  toPath = const "/"

instance ToQuery ListJobs where
  toQuery = const mempty

-- | /See:/ 'listJobsResponse' smart constructor.
data ListJobsResponse = ListJobsResponse'
  { _ljrsNextToken ::
      !(Maybe Text),
    _ljrsJobNames :: !(Maybe [Text]),
    _ljrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListJobsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ljrsNextToken' - A continuation token, if the returned list does not contain the last metric available.
--
-- * 'ljrsJobNames' - The names of all jobs in the account, or the jobs with the specified tags.
--
-- * 'ljrsResponseStatus' - -- | The response status code.
listJobsResponse ::
  -- | 'ljrsResponseStatus'
  Int ->
  ListJobsResponse
listJobsResponse pResponseStatus_ =
  ListJobsResponse'
    { _ljrsNextToken = Nothing,
      _ljrsJobNames = Nothing,
      _ljrsResponseStatus = pResponseStatus_
    }

-- | A continuation token, if the returned list does not contain the last metric available.
ljrsNextToken :: Lens' ListJobsResponse (Maybe Text)
ljrsNextToken = lens _ljrsNextToken (\s a -> s {_ljrsNextToken = a})

-- | The names of all jobs in the account, or the jobs with the specified tags.
ljrsJobNames :: Lens' ListJobsResponse [Text]
ljrsJobNames = lens _ljrsJobNames (\s a -> s {_ljrsJobNames = a}) . _Default . _Coerce

-- | -- | The response status code.
ljrsResponseStatus :: Lens' ListJobsResponse Int
ljrsResponseStatus = lens _ljrsResponseStatus (\s a -> s {_ljrsResponseStatus = a})

instance NFData ListJobsResponse
