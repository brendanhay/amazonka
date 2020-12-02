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
-- Module      : Network.AWS.Pinpoint.GetExportJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of all the export jobs for an application.
module Network.AWS.Pinpoint.GetExportJobs
  ( -- * Creating a Request
    getExportJobs,
    GetExportJobs,

    -- * Request Lenses
    gejsToken,
    gejsPageSize,
    gejsApplicationId,

    -- * Destructuring the Response
    getExportJobsResponse,
    GetExportJobsResponse,

    -- * Response Lenses
    gejrsResponseStatus,
    gejrsExportJobsResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getExportJobs' smart constructor.
data GetExportJobs = GetExportJobs'
  { _gejsToken :: !(Maybe Text),
    _gejsPageSize :: !(Maybe Text),
    _gejsApplicationId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetExportJobs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gejsToken' - The NextToken string that specifies which page of results to return in a paginated response.
--
-- * 'gejsPageSize' - The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- * 'gejsApplicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
getExportJobs ::
  -- | 'gejsApplicationId'
  Text ->
  GetExportJobs
getExportJobs pApplicationId_ =
  GetExportJobs'
    { _gejsToken = Nothing,
      _gejsPageSize = Nothing,
      _gejsApplicationId = pApplicationId_
    }

-- | The NextToken string that specifies which page of results to return in a paginated response.
gejsToken :: Lens' GetExportJobs (Maybe Text)
gejsToken = lens _gejsToken (\s a -> s {_gejsToken = a})

-- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
gejsPageSize :: Lens' GetExportJobs (Maybe Text)
gejsPageSize = lens _gejsPageSize (\s a -> s {_gejsPageSize = a})

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
gejsApplicationId :: Lens' GetExportJobs Text
gejsApplicationId = lens _gejsApplicationId (\s a -> s {_gejsApplicationId = a})

instance AWSRequest GetExportJobs where
  type Rs GetExportJobs = GetExportJobsResponse
  request = get pinpoint
  response =
    receiveJSON
      ( \s h x ->
          GetExportJobsResponse'
            <$> (pure (fromEnum s)) <*> (eitherParseJSON x)
      )

instance Hashable GetExportJobs

instance NFData GetExportJobs

instance ToHeaders GetExportJobs where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath GetExportJobs where
  toPath GetExportJobs' {..} =
    mconcat ["/v1/apps/", toBS _gejsApplicationId, "/jobs/export"]

instance ToQuery GetExportJobs where
  toQuery GetExportJobs' {..} =
    mconcat ["token" =: _gejsToken, "page-size" =: _gejsPageSize]

-- | /See:/ 'getExportJobsResponse' smart constructor.
data GetExportJobsResponse = GetExportJobsResponse'
  { _gejrsResponseStatus ::
      !Int,
    _gejrsExportJobsResponse :: !ExportJobsResponse
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetExportJobsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gejrsResponseStatus' - -- | The response status code.
--
-- * 'gejrsExportJobsResponse' - Undocumented member.
getExportJobsResponse ::
  -- | 'gejrsResponseStatus'
  Int ->
  -- | 'gejrsExportJobsResponse'
  ExportJobsResponse ->
  GetExportJobsResponse
getExportJobsResponse pResponseStatus_ pExportJobsResponse_ =
  GetExportJobsResponse'
    { _gejrsResponseStatus = pResponseStatus_,
      _gejrsExportJobsResponse = pExportJobsResponse_
    }

-- | -- | The response status code.
gejrsResponseStatus :: Lens' GetExportJobsResponse Int
gejrsResponseStatus = lens _gejrsResponseStatus (\s a -> s {_gejrsResponseStatus = a})

-- | Undocumented member.
gejrsExportJobsResponse :: Lens' GetExportJobsResponse ExportJobsResponse
gejrsExportJobsResponse = lens _gejrsExportJobsResponse (\s a -> s {_gejrsExportJobsResponse = a})

instance NFData GetExportJobsResponse
