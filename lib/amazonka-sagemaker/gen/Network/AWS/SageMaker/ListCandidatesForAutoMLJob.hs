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
-- Module      : Network.AWS.SageMaker.ListCandidatesForAutoMLJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the Candidates created for the job.
--
--
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListCandidatesForAutoMLJob
  ( -- * Creating a Request
    listCandidatesForAutoMLJob,
    ListCandidatesForAutoMLJob,

    -- * Request Lenses
    lcfamljCandidateNameEquals,
    lcfamljNextToken,
    lcfamljSortOrder,
    lcfamljStatusEquals,
    lcfamljMaxResults,
    lcfamljSortBy,
    lcfamljAutoMLJobName,

    -- * Destructuring the Response
    listCandidatesForAutoMLJobResponse,
    ListCandidatesForAutoMLJobResponse,

    -- * Response Lenses
    lcfamljrsNextToken,
    lcfamljrsResponseStatus,
    lcfamljrsCandidates,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'listCandidatesForAutoMLJob' smart constructor.
data ListCandidatesForAutoMLJob = ListCandidatesForAutoMLJob'
  { _lcfamljCandidateNameEquals ::
      !(Maybe Text),
    _lcfamljNextToken :: !(Maybe Text),
    _lcfamljSortOrder ::
      !(Maybe AutoMLSortOrder),
    _lcfamljStatusEquals ::
      !(Maybe CandidateStatus),
    _lcfamljMaxResults :: !(Maybe Nat),
    _lcfamljSortBy ::
      !(Maybe CandidateSortBy),
    _lcfamljAutoMLJobName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListCandidatesForAutoMLJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcfamljCandidateNameEquals' - List the Candidates for the job and filter by candidate name.
--
-- * 'lcfamljNextToken' - If the previous response was truncated, you receive this token. Use it in your next request to receive the next set of results.
--
-- * 'lcfamljSortOrder' - The sort order for the results. The default is Ascending.
--
-- * 'lcfamljStatusEquals' - List the Candidates for the job and filter by status.
--
-- * 'lcfamljMaxResults' - List the job's Candidates up to a specified limit.
--
-- * 'lcfamljSortBy' - The parameter by which to sort the results. The default is Descending.
--
-- * 'lcfamljAutoMLJobName' - List the Candidates created for the job by providing the job's name.
listCandidatesForAutoMLJob ::
  -- | 'lcfamljAutoMLJobName'
  Text ->
  ListCandidatesForAutoMLJob
listCandidatesForAutoMLJob pAutoMLJobName_ =
  ListCandidatesForAutoMLJob'
    { _lcfamljCandidateNameEquals =
        Nothing,
      _lcfamljNextToken = Nothing,
      _lcfamljSortOrder = Nothing,
      _lcfamljStatusEquals = Nothing,
      _lcfamljMaxResults = Nothing,
      _lcfamljSortBy = Nothing,
      _lcfamljAutoMLJobName = pAutoMLJobName_
    }

-- | List the Candidates for the job and filter by candidate name.
lcfamljCandidateNameEquals :: Lens' ListCandidatesForAutoMLJob (Maybe Text)
lcfamljCandidateNameEquals = lens _lcfamljCandidateNameEquals (\s a -> s {_lcfamljCandidateNameEquals = a})

-- | If the previous response was truncated, you receive this token. Use it in your next request to receive the next set of results.
lcfamljNextToken :: Lens' ListCandidatesForAutoMLJob (Maybe Text)
lcfamljNextToken = lens _lcfamljNextToken (\s a -> s {_lcfamljNextToken = a})

-- | The sort order for the results. The default is Ascending.
lcfamljSortOrder :: Lens' ListCandidatesForAutoMLJob (Maybe AutoMLSortOrder)
lcfamljSortOrder = lens _lcfamljSortOrder (\s a -> s {_lcfamljSortOrder = a})

-- | List the Candidates for the job and filter by status.
lcfamljStatusEquals :: Lens' ListCandidatesForAutoMLJob (Maybe CandidateStatus)
lcfamljStatusEquals = lens _lcfamljStatusEquals (\s a -> s {_lcfamljStatusEquals = a})

-- | List the job's Candidates up to a specified limit.
lcfamljMaxResults :: Lens' ListCandidatesForAutoMLJob (Maybe Natural)
lcfamljMaxResults = lens _lcfamljMaxResults (\s a -> s {_lcfamljMaxResults = a}) . mapping _Nat

-- | The parameter by which to sort the results. The default is Descending.
lcfamljSortBy :: Lens' ListCandidatesForAutoMLJob (Maybe CandidateSortBy)
lcfamljSortBy = lens _lcfamljSortBy (\s a -> s {_lcfamljSortBy = a})

-- | List the Candidates created for the job by providing the job's name.
lcfamljAutoMLJobName :: Lens' ListCandidatesForAutoMLJob Text
lcfamljAutoMLJobName = lens _lcfamljAutoMLJobName (\s a -> s {_lcfamljAutoMLJobName = a})

instance AWSPager ListCandidatesForAutoMLJob where
  page rq rs
    | stop (rs ^. lcfamljrsNextToken) = Nothing
    | stop (rs ^. lcfamljrsCandidates) = Nothing
    | otherwise =
      Just $ rq & lcfamljNextToken .~ rs ^. lcfamljrsNextToken

instance AWSRequest ListCandidatesForAutoMLJob where
  type
    Rs ListCandidatesForAutoMLJob =
      ListCandidatesForAutoMLJobResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          ListCandidatesForAutoMLJobResponse'
            <$> (x .?> "NextToken")
            <*> (pure (fromEnum s))
            <*> (x .?> "Candidates" .!@ mempty)
      )

instance Hashable ListCandidatesForAutoMLJob

instance NFData ListCandidatesForAutoMLJob

instance ToHeaders ListCandidatesForAutoMLJob where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("SageMaker.ListCandidatesForAutoMLJob" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListCandidatesForAutoMLJob where
  toJSON ListCandidatesForAutoMLJob' {..} =
    object
      ( catMaybes
          [ ("CandidateNameEquals" .=) <$> _lcfamljCandidateNameEquals,
            ("NextToken" .=) <$> _lcfamljNextToken,
            ("SortOrder" .=) <$> _lcfamljSortOrder,
            ("StatusEquals" .=) <$> _lcfamljStatusEquals,
            ("MaxResults" .=) <$> _lcfamljMaxResults,
            ("SortBy" .=) <$> _lcfamljSortBy,
            Just ("AutoMLJobName" .= _lcfamljAutoMLJobName)
          ]
      )

instance ToPath ListCandidatesForAutoMLJob where
  toPath = const "/"

instance ToQuery ListCandidatesForAutoMLJob where
  toQuery = const mempty

-- | /See:/ 'listCandidatesForAutoMLJobResponse' smart constructor.
data ListCandidatesForAutoMLJobResponse = ListCandidatesForAutoMLJobResponse'
  { _lcfamljrsNextToken ::
      !(Maybe Text),
    _lcfamljrsResponseStatus ::
      !Int,
    _lcfamljrsCandidates ::
      ![AutoMLCandidate]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListCandidatesForAutoMLJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcfamljrsNextToken' - If the previous response was truncated, you receive this token. Use it in your next request to receive the next set of results.
--
-- * 'lcfamljrsResponseStatus' - -- | The response status code.
--
-- * 'lcfamljrsCandidates' - Summaries about the Candidates.
listCandidatesForAutoMLJobResponse ::
  -- | 'lcfamljrsResponseStatus'
  Int ->
  ListCandidatesForAutoMLJobResponse
listCandidatesForAutoMLJobResponse pResponseStatus_ =
  ListCandidatesForAutoMLJobResponse'
    { _lcfamljrsNextToken =
        Nothing,
      _lcfamljrsResponseStatus = pResponseStatus_,
      _lcfamljrsCandidates = mempty
    }

-- | If the previous response was truncated, you receive this token. Use it in your next request to receive the next set of results.
lcfamljrsNextToken :: Lens' ListCandidatesForAutoMLJobResponse (Maybe Text)
lcfamljrsNextToken = lens _lcfamljrsNextToken (\s a -> s {_lcfamljrsNextToken = a})

-- | -- | The response status code.
lcfamljrsResponseStatus :: Lens' ListCandidatesForAutoMLJobResponse Int
lcfamljrsResponseStatus = lens _lcfamljrsResponseStatus (\s a -> s {_lcfamljrsResponseStatus = a})

-- | Summaries about the Candidates.
lcfamljrsCandidates :: Lens' ListCandidatesForAutoMLJobResponse [AutoMLCandidate]
lcfamljrsCandidates = lens _lcfamljrsCandidates (\s a -> s {_lcfamljrsCandidates = a}) . _Coerce

instance NFData ListCandidatesForAutoMLJobResponse
