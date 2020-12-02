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
-- Module      : Network.AWS.DynamoDB.ListExports
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists completed exports within the past 90 days.
module Network.AWS.DynamoDB.ListExports
  ( -- * Creating a Request
    listExports,
    ListExports,

    -- * Request Lenses
    leTableARN,
    leNextToken,
    leMaxResults,

    -- * Destructuring the Response
    listExportsResponse,
    ListExportsResponse,

    -- * Response Lenses
    lersExportSummaries,
    lersNextToken,
    lersResponseStatus,
  )
where

import Network.AWS.DynamoDB.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listExports' smart constructor.
data ListExports = ListExports'
  { _leTableARN :: !(Maybe Text),
    _leNextToken :: !(Maybe Text),
    _leMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListExports' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'leTableARN' - The Amazon Resource Name (ARN) associated with the exported table.
--
-- * 'leNextToken' - An optional string that, if supplied, must be copied from the output of a previous call to @ListExports@ . When provided in this manner, the API fetches the next page of results.
--
-- * 'leMaxResults' - Maximum number of results to return per page.
listExports ::
  ListExports
listExports =
  ListExports'
    { _leTableARN = Nothing,
      _leNextToken = Nothing,
      _leMaxResults = Nothing
    }

-- | The Amazon Resource Name (ARN) associated with the exported table.
leTableARN :: Lens' ListExports (Maybe Text)
leTableARN = lens _leTableARN (\s a -> s {_leTableARN = a})

-- | An optional string that, if supplied, must be copied from the output of a previous call to @ListExports@ . When provided in this manner, the API fetches the next page of results.
leNextToken :: Lens' ListExports (Maybe Text)
leNextToken = lens _leNextToken (\s a -> s {_leNextToken = a})

-- | Maximum number of results to return per page.
leMaxResults :: Lens' ListExports (Maybe Natural)
leMaxResults = lens _leMaxResults (\s a -> s {_leMaxResults = a}) . mapping _Nat

instance AWSRequest ListExports where
  type Rs ListExports = ListExportsResponse
  request = postJSON dynamoDB
  response =
    receiveJSON
      ( \s h x ->
          ListExportsResponse'
            <$> (x .?> "ExportSummaries" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListExports

instance NFData ListExports

instance ToHeaders ListExports where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("DynamoDB_20120810.ListExports" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.0" :: ByteString)
          ]
      )

instance ToJSON ListExports where
  toJSON ListExports' {..} =
    object
      ( catMaybes
          [ ("TableArn" .=) <$> _leTableARN,
            ("NextToken" .=) <$> _leNextToken,
            ("MaxResults" .=) <$> _leMaxResults
          ]
      )

instance ToPath ListExports where
  toPath = const "/"

instance ToQuery ListExports where
  toQuery = const mempty

-- | /See:/ 'listExportsResponse' smart constructor.
data ListExportsResponse = ListExportsResponse'
  { _lersExportSummaries ::
      !(Maybe [ExportSummary]),
    _lersNextToken :: !(Maybe Text),
    _lersResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListExportsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lersExportSummaries' - A list of @ExportSummary@ objects.
--
-- * 'lersNextToken' - If this value is returned, there are additional results to be displayed. To retrieve them, call @ListExports@ again, with @NextToken@ set to this value.
--
-- * 'lersResponseStatus' - -- | The response status code.
listExportsResponse ::
  -- | 'lersResponseStatus'
  Int ->
  ListExportsResponse
listExportsResponse pResponseStatus_ =
  ListExportsResponse'
    { _lersExportSummaries = Nothing,
      _lersNextToken = Nothing,
      _lersResponseStatus = pResponseStatus_
    }

-- | A list of @ExportSummary@ objects.
lersExportSummaries :: Lens' ListExportsResponse [ExportSummary]
lersExportSummaries = lens _lersExportSummaries (\s a -> s {_lersExportSummaries = a}) . _Default . _Coerce

-- | If this value is returned, there are additional results to be displayed. To retrieve them, call @ListExports@ again, with @NextToken@ set to this value.
lersNextToken :: Lens' ListExportsResponse (Maybe Text)
lersNextToken = lens _lersNextToken (\s a -> s {_lersNextToken = a})

-- | -- | The response status code.
lersResponseStatus :: Lens' ListExportsResponse Int
lersResponseStatus = lens _lersResponseStatus (\s a -> s {_lersResponseStatus = a})

instance NFData ListExportsResponse
