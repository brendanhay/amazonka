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
-- Module      : Network.AWS.WorkMail.ListMailboxExportJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the mailbox export jobs started for the specified organization within the last seven days.
module Network.AWS.WorkMail.ListMailboxExportJobs
  ( -- * Creating a Request
    listMailboxExportJobs,
    ListMailboxExportJobs,

    -- * Request Lenses
    lmejNextToken,
    lmejMaxResults,
    lmejOrganizationId,

    -- * Destructuring the Response
    listMailboxExportJobsResponse,
    ListMailboxExportJobsResponse,

    -- * Response Lenses
    lmejrsNextToken,
    lmejrsJobs,
    lmejrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'listMailboxExportJobs' smart constructor.
data ListMailboxExportJobs = ListMailboxExportJobs'
  { _lmejNextToken ::
      !(Maybe Text),
    _lmejMaxResults :: !(Maybe Nat),
    _lmejOrganizationId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListMailboxExportJobs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lmejNextToken' - The token to use to retrieve the next page of results.
--
-- * 'lmejMaxResults' - The maximum number of results to return in a single call.
--
-- * 'lmejOrganizationId' - The organization ID.
listMailboxExportJobs ::
  -- | 'lmejOrganizationId'
  Text ->
  ListMailboxExportJobs
listMailboxExportJobs pOrganizationId_ =
  ListMailboxExportJobs'
    { _lmejNextToken = Nothing,
      _lmejMaxResults = Nothing,
      _lmejOrganizationId = pOrganizationId_
    }

-- | The token to use to retrieve the next page of results.
lmejNextToken :: Lens' ListMailboxExportJobs (Maybe Text)
lmejNextToken = lens _lmejNextToken (\s a -> s {_lmejNextToken = a})

-- | The maximum number of results to return in a single call.
lmejMaxResults :: Lens' ListMailboxExportJobs (Maybe Natural)
lmejMaxResults = lens _lmejMaxResults (\s a -> s {_lmejMaxResults = a}) . mapping _Nat

-- | The organization ID.
lmejOrganizationId :: Lens' ListMailboxExportJobs Text
lmejOrganizationId = lens _lmejOrganizationId (\s a -> s {_lmejOrganizationId = a})

instance AWSRequest ListMailboxExportJobs where
  type Rs ListMailboxExportJobs = ListMailboxExportJobsResponse
  request = postJSON workMail
  response =
    receiveJSON
      ( \s h x ->
          ListMailboxExportJobsResponse'
            <$> (x .?> "NextToken")
            <*> (x .?> "Jobs" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable ListMailboxExportJobs

instance NFData ListMailboxExportJobs

instance ToHeaders ListMailboxExportJobs where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("WorkMailService.ListMailboxExportJobs" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListMailboxExportJobs where
  toJSON ListMailboxExportJobs' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _lmejNextToken,
            ("MaxResults" .=) <$> _lmejMaxResults,
            Just ("OrganizationId" .= _lmejOrganizationId)
          ]
      )

instance ToPath ListMailboxExportJobs where
  toPath = const "/"

instance ToQuery ListMailboxExportJobs where
  toQuery = const mempty

-- | /See:/ 'listMailboxExportJobsResponse' smart constructor.
data ListMailboxExportJobsResponse = ListMailboxExportJobsResponse'
  { _lmejrsNextToken ::
      !(Maybe Text),
    _lmejrsJobs ::
      !(Maybe [MailboxExportJob]),
    _lmejrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListMailboxExportJobsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lmejrsNextToken' - The token to use to retrieve the next page of results.
--
-- * 'lmejrsJobs' - The mailbox export job details.
--
-- * 'lmejrsResponseStatus' - -- | The response status code.
listMailboxExportJobsResponse ::
  -- | 'lmejrsResponseStatus'
  Int ->
  ListMailboxExportJobsResponse
listMailboxExportJobsResponse pResponseStatus_ =
  ListMailboxExportJobsResponse'
    { _lmejrsNextToken = Nothing,
      _lmejrsJobs = Nothing,
      _lmejrsResponseStatus = pResponseStatus_
    }

-- | The token to use to retrieve the next page of results.
lmejrsNextToken :: Lens' ListMailboxExportJobsResponse (Maybe Text)
lmejrsNextToken = lens _lmejrsNextToken (\s a -> s {_lmejrsNextToken = a})

-- | The mailbox export job details.
lmejrsJobs :: Lens' ListMailboxExportJobsResponse [MailboxExportJob]
lmejrsJobs = lens _lmejrsJobs (\s a -> s {_lmejrsJobs = a}) . _Default . _Coerce

-- | -- | The response status code.
lmejrsResponseStatus :: Lens' ListMailboxExportJobsResponse Int
lmejrsResponseStatus = lens _lmejrsResponseStatus (\s a -> s {_lmejrsResponseStatus = a})

instance NFData ListMailboxExportJobsResponse
