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
-- Module      : Network.AWS.CodeBuild.BatchGetReports
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of reports.
module Network.AWS.CodeBuild.BatchGetReports
  ( -- * Creating a Request
    batchGetReports,
    BatchGetReports,

    -- * Request Lenses
    bgrReportARNs,

    -- * Destructuring the Response
    batchGetReportsResponse,
    BatchGetReportsResponse,

    -- * Response Lenses
    bgrrsReports,
    bgrrsReportsNotFound,
    bgrrsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'batchGetReports' smart constructor.
newtype BatchGetReports = BatchGetReports'
  { _bgrReportARNs ::
      List1 Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchGetReports' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bgrReportARNs' - An array of ARNs that identify the @Report@ objects to return.
batchGetReports ::
  -- | 'bgrReportARNs'
  NonEmpty Text ->
  BatchGetReports
batchGetReports pReportARNs_ =
  BatchGetReports' {_bgrReportARNs = _List1 # pReportARNs_}

-- | An array of ARNs that identify the @Report@ objects to return.
bgrReportARNs :: Lens' BatchGetReports (NonEmpty Text)
bgrReportARNs = lens _bgrReportARNs (\s a -> s {_bgrReportARNs = a}) . _List1

instance AWSRequest BatchGetReports where
  type Rs BatchGetReports = BatchGetReportsResponse
  request = postJSON codeBuild
  response =
    receiveJSON
      ( \s h x ->
          BatchGetReportsResponse'
            <$> (x .?> "reports")
            <*> (x .?> "reportsNotFound")
            <*> (pure (fromEnum s))
      )

instance Hashable BatchGetReports

instance NFData BatchGetReports

instance ToHeaders BatchGetReports where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("CodeBuild_20161006.BatchGetReports" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON BatchGetReports where
  toJSON BatchGetReports' {..} =
    object (catMaybes [Just ("reportArns" .= _bgrReportARNs)])

instance ToPath BatchGetReports where
  toPath = const "/"

instance ToQuery BatchGetReports where
  toQuery = const mempty

-- | /See:/ 'batchGetReportsResponse' smart constructor.
data BatchGetReportsResponse = BatchGetReportsResponse'
  { _bgrrsReports ::
      !(Maybe (List1 Report)),
    _bgrrsReportsNotFound ::
      !(Maybe (List1 Text)),
    _bgrrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchGetReportsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bgrrsReports' - The array of @Report@ objects returned by @BatchGetReports@ .
--
-- * 'bgrrsReportsNotFound' - An array of ARNs passed to @BatchGetReportGroups@ that are not associated with a @Report@ .
--
-- * 'bgrrsResponseStatus' - -- | The response status code.
batchGetReportsResponse ::
  -- | 'bgrrsResponseStatus'
  Int ->
  BatchGetReportsResponse
batchGetReportsResponse pResponseStatus_ =
  BatchGetReportsResponse'
    { _bgrrsReports = Nothing,
      _bgrrsReportsNotFound = Nothing,
      _bgrrsResponseStatus = pResponseStatus_
    }

-- | The array of @Report@ objects returned by @BatchGetReports@ .
bgrrsReports :: Lens' BatchGetReportsResponse (Maybe (NonEmpty Report))
bgrrsReports = lens _bgrrsReports (\s a -> s {_bgrrsReports = a}) . mapping _List1

-- | An array of ARNs passed to @BatchGetReportGroups@ that are not associated with a @Report@ .
bgrrsReportsNotFound :: Lens' BatchGetReportsResponse (Maybe (NonEmpty Text))
bgrrsReportsNotFound = lens _bgrrsReportsNotFound (\s a -> s {_bgrrsReportsNotFound = a}) . mapping _List1

-- | -- | The response status code.
bgrrsResponseStatus :: Lens' BatchGetReportsResponse Int
bgrrsResponseStatus = lens _bgrrsResponseStatus (\s a -> s {_bgrrsResponseStatus = a})

instance NFData BatchGetReportsResponse
