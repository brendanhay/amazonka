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
-- Module      : Network.AWS.Pinpoint.GetImportJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of all the import jobs for an application.
module Network.AWS.Pinpoint.GetImportJobs
  ( -- * Creating a Request
    getImportJobs,
    GetImportJobs,

    -- * Request Lenses
    gijsToken,
    gijsPageSize,
    gijsApplicationId,

    -- * Destructuring the Response
    getImportJobsResponse,
    GetImportJobsResponse,

    -- * Response Lenses
    gijsrsResponseStatus,
    gijsrsImportJobsResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getImportJobs' smart constructor.
data GetImportJobs = GetImportJobs'
  { _gijsToken :: !(Maybe Text),
    _gijsPageSize :: !(Maybe Text),
    _gijsApplicationId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetImportJobs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gijsToken' - The NextToken string that specifies which page of results to return in a paginated response.
--
-- * 'gijsPageSize' - The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- * 'gijsApplicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
getImportJobs ::
  -- | 'gijsApplicationId'
  Text ->
  GetImportJobs
getImportJobs pApplicationId_ =
  GetImportJobs'
    { _gijsToken = Nothing,
      _gijsPageSize = Nothing,
      _gijsApplicationId = pApplicationId_
    }

-- | The NextToken string that specifies which page of results to return in a paginated response.
gijsToken :: Lens' GetImportJobs (Maybe Text)
gijsToken = lens _gijsToken (\s a -> s {_gijsToken = a})

-- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
gijsPageSize :: Lens' GetImportJobs (Maybe Text)
gijsPageSize = lens _gijsPageSize (\s a -> s {_gijsPageSize = a})

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
gijsApplicationId :: Lens' GetImportJobs Text
gijsApplicationId = lens _gijsApplicationId (\s a -> s {_gijsApplicationId = a})

instance AWSRequest GetImportJobs where
  type Rs GetImportJobs = GetImportJobsResponse
  request = get pinpoint
  response =
    receiveJSON
      ( \s h x ->
          GetImportJobsResponse'
            <$> (pure (fromEnum s)) <*> (eitherParseJSON x)
      )

instance Hashable GetImportJobs

instance NFData GetImportJobs

instance ToHeaders GetImportJobs where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath GetImportJobs where
  toPath GetImportJobs' {..} =
    mconcat ["/v1/apps/", toBS _gijsApplicationId, "/jobs/import"]

instance ToQuery GetImportJobs where
  toQuery GetImportJobs' {..} =
    mconcat ["token" =: _gijsToken, "page-size" =: _gijsPageSize]

-- | /See:/ 'getImportJobsResponse' smart constructor.
data GetImportJobsResponse = GetImportJobsResponse'
  { _gijsrsResponseStatus ::
      !Int,
    _gijsrsImportJobsResponse ::
      !ImportJobsResponse
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetImportJobsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gijsrsResponseStatus' - -- | The response status code.
--
-- * 'gijsrsImportJobsResponse' - Undocumented member.
getImportJobsResponse ::
  -- | 'gijsrsResponseStatus'
  Int ->
  -- | 'gijsrsImportJobsResponse'
  ImportJobsResponse ->
  GetImportJobsResponse
getImportJobsResponse pResponseStatus_ pImportJobsResponse_ =
  GetImportJobsResponse'
    { _gijsrsResponseStatus = pResponseStatus_,
      _gijsrsImportJobsResponse = pImportJobsResponse_
    }

-- | -- | The response status code.
gijsrsResponseStatus :: Lens' GetImportJobsResponse Int
gijsrsResponseStatus = lens _gijsrsResponseStatus (\s a -> s {_gijsrsResponseStatus = a})

-- | Undocumented member.
gijsrsImportJobsResponse :: Lens' GetImportJobsResponse ImportJobsResponse
gijsrsImportJobsResponse = lens _gijsrsImportJobsResponse (\s a -> s {_gijsrsImportJobsResponse = a})

instance NFData GetImportJobsResponse
