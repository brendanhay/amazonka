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
-- Module      : Network.AWS.Greengrass.ListBulkDeploymentDetailedReports
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a paginated list of the deployments that have been started in a bulk deployment operation, and their current deployment status.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListBulkDeploymentDetailedReports
  ( -- * Creating a Request
    listBulkDeploymentDetailedReports,
    ListBulkDeploymentDetailedReports,

    -- * Request Lenses
    lbddrNextToken,
    lbddrMaxResults,
    lbddrBulkDeploymentId,

    -- * Destructuring the Response
    listBulkDeploymentDetailedReportsResponse,
    ListBulkDeploymentDetailedReportsResponse,

    -- * Response Lenses
    lbddrrsNextToken,
    lbddrrsDeployments,
    lbddrrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listBulkDeploymentDetailedReports' smart constructor.
data ListBulkDeploymentDetailedReports = ListBulkDeploymentDetailedReports'
  { _lbddrNextToken ::
      !(Maybe Text),
    _lbddrMaxResults ::
      !(Maybe Text),
    _lbddrBulkDeploymentId ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListBulkDeploymentDetailedReports' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbddrNextToken' - The token for the next set of results, or ''null'' if there are no additional results.
--
-- * 'lbddrMaxResults' - The maximum number of results to be returned per request.
--
-- * 'lbddrBulkDeploymentId' - The ID of the bulk deployment.
listBulkDeploymentDetailedReports ::
  -- | 'lbddrBulkDeploymentId'
  Text ->
  ListBulkDeploymentDetailedReports
listBulkDeploymentDetailedReports pBulkDeploymentId_ =
  ListBulkDeploymentDetailedReports'
    { _lbddrNextToken = Nothing,
      _lbddrMaxResults = Nothing,
      _lbddrBulkDeploymentId = pBulkDeploymentId_
    }

-- | The token for the next set of results, or ''null'' if there are no additional results.
lbddrNextToken :: Lens' ListBulkDeploymentDetailedReports (Maybe Text)
lbddrNextToken = lens _lbddrNextToken (\s a -> s {_lbddrNextToken = a})

-- | The maximum number of results to be returned per request.
lbddrMaxResults :: Lens' ListBulkDeploymentDetailedReports (Maybe Text)
lbddrMaxResults = lens _lbddrMaxResults (\s a -> s {_lbddrMaxResults = a})

-- | The ID of the bulk deployment.
lbddrBulkDeploymentId :: Lens' ListBulkDeploymentDetailedReports Text
lbddrBulkDeploymentId = lens _lbddrBulkDeploymentId (\s a -> s {_lbddrBulkDeploymentId = a})

instance AWSPager ListBulkDeploymentDetailedReports where
  page rq rs
    | stop (rs ^. lbddrrsNextToken) = Nothing
    | stop (rs ^. lbddrrsDeployments) = Nothing
    | otherwise = Just $ rq & lbddrNextToken .~ rs ^. lbddrrsNextToken

instance AWSRequest ListBulkDeploymentDetailedReports where
  type
    Rs ListBulkDeploymentDetailedReports =
      ListBulkDeploymentDetailedReportsResponse
  request = get greengrass
  response =
    receiveJSON
      ( \s h x ->
          ListBulkDeploymentDetailedReportsResponse'
            <$> (x .?> "NextToken")
            <*> (x .?> "Deployments" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable ListBulkDeploymentDetailedReports

instance NFData ListBulkDeploymentDetailedReports

instance ToHeaders ListBulkDeploymentDetailedReports where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath ListBulkDeploymentDetailedReports where
  toPath ListBulkDeploymentDetailedReports' {..} =
    mconcat
      [ "/greengrass/bulk/deployments/",
        toBS _lbddrBulkDeploymentId,
        "/detailed-reports"
      ]

instance ToQuery ListBulkDeploymentDetailedReports where
  toQuery ListBulkDeploymentDetailedReports' {..} =
    mconcat
      ["NextToken" =: _lbddrNextToken, "MaxResults" =: _lbddrMaxResults]

-- | /See:/ 'listBulkDeploymentDetailedReportsResponse' smart constructor.
data ListBulkDeploymentDetailedReportsResponse = ListBulkDeploymentDetailedReportsResponse'
  { _lbddrrsNextToken ::
      !( Maybe
           Text
       ),
    _lbddrrsDeployments ::
      !( Maybe
           [BulkDeploymentResult]
       ),
    _lbddrrsResponseStatus ::
      !Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'ListBulkDeploymentDetailedReportsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbddrrsNextToken' - The token for the next set of results, or ''null'' if there are no additional results.
--
-- * 'lbddrrsDeployments' - A list of the individual group deployments in the bulk deployment operation.
--
-- * 'lbddrrsResponseStatus' - -- | The response status code.
listBulkDeploymentDetailedReportsResponse ::
  -- | 'lbddrrsResponseStatus'
  Int ->
  ListBulkDeploymentDetailedReportsResponse
listBulkDeploymentDetailedReportsResponse pResponseStatus_ =
  ListBulkDeploymentDetailedReportsResponse'
    { _lbddrrsNextToken =
        Nothing,
      _lbddrrsDeployments = Nothing,
      _lbddrrsResponseStatus = pResponseStatus_
    }

-- | The token for the next set of results, or ''null'' if there are no additional results.
lbddrrsNextToken :: Lens' ListBulkDeploymentDetailedReportsResponse (Maybe Text)
lbddrrsNextToken = lens _lbddrrsNextToken (\s a -> s {_lbddrrsNextToken = a})

-- | A list of the individual group deployments in the bulk deployment operation.
lbddrrsDeployments :: Lens' ListBulkDeploymentDetailedReportsResponse [BulkDeploymentResult]
lbddrrsDeployments = lens _lbddrrsDeployments (\s a -> s {_lbddrrsDeployments = a}) . _Default . _Coerce

-- | -- | The response status code.
lbddrrsResponseStatus :: Lens' ListBulkDeploymentDetailedReportsResponse Int
lbddrrsResponseStatus = lens _lbddrrsResponseStatus (\s a -> s {_lbddrrsResponseStatus = a})

instance NFData ListBulkDeploymentDetailedReportsResponse
