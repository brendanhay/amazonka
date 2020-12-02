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
-- Module      : Network.AWS.Translate.ListTextTranslationJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the batch translation jobs that you have submitted.
module Network.AWS.Translate.ListTextTranslationJobs
  ( -- * Creating a Request
    listTextTranslationJobs,
    ListTextTranslationJobs,

    -- * Request Lenses
    lttjNextToken,
    lttjFilter,
    lttjMaxResults,

    -- * Destructuring the Response
    listTextTranslationJobsResponse,
    ListTextTranslationJobsResponse,

    -- * Response Lenses
    lttjrsTextTranslationJobPropertiesList,
    lttjrsNextToken,
    lttjrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Translate.Types

-- | /See:/ 'listTextTranslationJobs' smart constructor.
data ListTextTranslationJobs = ListTextTranslationJobs'
  { _lttjNextToken ::
      !(Maybe Text),
    _lttjFilter ::
      !(Maybe TextTranslationJobFilter),
    _lttjMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListTextTranslationJobs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lttjNextToken' - The token to request the next page of results.
--
-- * 'lttjFilter' - The parameters that specify which batch translation jobs to retrieve. Filters include job name, job status, and submission time. You can only set one filter at a time.
--
-- * 'lttjMaxResults' - The maximum number of results to return in each page. The default value is 100.
listTextTranslationJobs ::
  ListTextTranslationJobs
listTextTranslationJobs =
  ListTextTranslationJobs'
    { _lttjNextToken = Nothing,
      _lttjFilter = Nothing,
      _lttjMaxResults = Nothing
    }

-- | The token to request the next page of results.
lttjNextToken :: Lens' ListTextTranslationJobs (Maybe Text)
lttjNextToken = lens _lttjNextToken (\s a -> s {_lttjNextToken = a})

-- | The parameters that specify which batch translation jobs to retrieve. Filters include job name, job status, and submission time. You can only set one filter at a time.
lttjFilter :: Lens' ListTextTranslationJobs (Maybe TextTranslationJobFilter)
lttjFilter = lens _lttjFilter (\s a -> s {_lttjFilter = a})

-- | The maximum number of results to return in each page. The default value is 100.
lttjMaxResults :: Lens' ListTextTranslationJobs (Maybe Natural)
lttjMaxResults = lens _lttjMaxResults (\s a -> s {_lttjMaxResults = a}) . mapping _Nat

instance AWSRequest ListTextTranslationJobs where
  type Rs ListTextTranslationJobs = ListTextTranslationJobsResponse
  request = postJSON translate
  response =
    receiveJSON
      ( \s h x ->
          ListTextTranslationJobsResponse'
            <$> (x .?> "TextTranslationJobPropertiesList" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListTextTranslationJobs

instance NFData ListTextTranslationJobs

instance ToHeaders ListTextTranslationJobs where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSShineFrontendService_20170701.ListTextTranslationJobs" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListTextTranslationJobs where
  toJSON ListTextTranslationJobs' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _lttjNextToken,
            ("Filter" .=) <$> _lttjFilter,
            ("MaxResults" .=) <$> _lttjMaxResults
          ]
      )

instance ToPath ListTextTranslationJobs where
  toPath = const "/"

instance ToQuery ListTextTranslationJobs where
  toQuery = const mempty

-- | /See:/ 'listTextTranslationJobsResponse' smart constructor.
data ListTextTranslationJobsResponse = ListTextTranslationJobsResponse'
  { _lttjrsTextTranslationJobPropertiesList ::
      !( Maybe
           [TextTranslationJobProperties]
       ),
    _lttjrsNextToken ::
      !(Maybe Text),
    _lttjrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListTextTranslationJobsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lttjrsTextTranslationJobPropertiesList' - A list containing the properties of each job that is returned.
--
-- * 'lttjrsNextToken' - The token to use to retreive the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'lttjrsResponseStatus' - -- | The response status code.
listTextTranslationJobsResponse ::
  -- | 'lttjrsResponseStatus'
  Int ->
  ListTextTranslationJobsResponse
listTextTranslationJobsResponse pResponseStatus_ =
  ListTextTranslationJobsResponse'
    { _lttjrsTextTranslationJobPropertiesList =
        Nothing,
      _lttjrsNextToken = Nothing,
      _lttjrsResponseStatus = pResponseStatus_
    }

-- | A list containing the properties of each job that is returned.
lttjrsTextTranslationJobPropertiesList :: Lens' ListTextTranslationJobsResponse [TextTranslationJobProperties]
lttjrsTextTranslationJobPropertiesList = lens _lttjrsTextTranslationJobPropertiesList (\s a -> s {_lttjrsTextTranslationJobPropertiesList = a}) . _Default . _Coerce

-- | The token to use to retreive the next page of results. This value is @null@ when there are no more results to return.
lttjrsNextToken :: Lens' ListTextTranslationJobsResponse (Maybe Text)
lttjrsNextToken = lens _lttjrsNextToken (\s a -> s {_lttjrsNextToken = a})

-- | -- | The response status code.
lttjrsResponseStatus :: Lens' ListTextTranslationJobsResponse Int
lttjrsResponseStatus = lens _lttjrsResponseStatus (\s a -> s {_lttjrsResponseStatus = a})

instance NFData ListTextTranslationJobsResponse
