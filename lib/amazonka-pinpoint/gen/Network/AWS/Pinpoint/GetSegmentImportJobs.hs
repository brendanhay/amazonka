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
-- Module      : Network.AWS.Pinpoint.GetSegmentImportJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of the import jobs for a segment.
module Network.AWS.Pinpoint.GetSegmentImportJobs
  ( -- * Creating a Request
    getSegmentImportJobs,
    GetSegmentImportJobs,

    -- * Request Lenses
    gsijToken,
    gsijPageSize,
    gsijSegmentId,
    gsijApplicationId,

    -- * Destructuring the Response
    getSegmentImportJobsResponse,
    GetSegmentImportJobsResponse,

    -- * Response Lenses
    gsijrsResponseStatus,
    gsijrsImportJobsResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getSegmentImportJobs' smart constructor.
data GetSegmentImportJobs = GetSegmentImportJobs'
  { _gsijToken ::
      !(Maybe Text),
    _gsijPageSize :: !(Maybe Text),
    _gsijSegmentId :: !Text,
    _gsijApplicationId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetSegmentImportJobs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsijToken' - The NextToken string that specifies which page of results to return in a paginated response.
--
-- * 'gsijPageSize' - The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- * 'gsijSegmentId' - The unique identifier for the segment.
--
-- * 'gsijApplicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
getSegmentImportJobs ::
  -- | 'gsijSegmentId'
  Text ->
  -- | 'gsijApplicationId'
  Text ->
  GetSegmentImportJobs
getSegmentImportJobs pSegmentId_ pApplicationId_ =
  GetSegmentImportJobs'
    { _gsijToken = Nothing,
      _gsijPageSize = Nothing,
      _gsijSegmentId = pSegmentId_,
      _gsijApplicationId = pApplicationId_
    }

-- | The NextToken string that specifies which page of results to return in a paginated response.
gsijToken :: Lens' GetSegmentImportJobs (Maybe Text)
gsijToken = lens _gsijToken (\s a -> s {_gsijToken = a})

-- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
gsijPageSize :: Lens' GetSegmentImportJobs (Maybe Text)
gsijPageSize = lens _gsijPageSize (\s a -> s {_gsijPageSize = a})

-- | The unique identifier for the segment.
gsijSegmentId :: Lens' GetSegmentImportJobs Text
gsijSegmentId = lens _gsijSegmentId (\s a -> s {_gsijSegmentId = a})

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
gsijApplicationId :: Lens' GetSegmentImportJobs Text
gsijApplicationId = lens _gsijApplicationId (\s a -> s {_gsijApplicationId = a})

instance AWSRequest GetSegmentImportJobs where
  type Rs GetSegmentImportJobs = GetSegmentImportJobsResponse
  request = get pinpoint
  response =
    receiveJSON
      ( \s h x ->
          GetSegmentImportJobsResponse'
            <$> (pure (fromEnum s)) <*> (eitherParseJSON x)
      )

instance Hashable GetSegmentImportJobs

instance NFData GetSegmentImportJobs

instance ToHeaders GetSegmentImportJobs where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath GetSegmentImportJobs where
  toPath GetSegmentImportJobs' {..} =
    mconcat
      [ "/v1/apps/",
        toBS _gsijApplicationId,
        "/segments/",
        toBS _gsijSegmentId,
        "/jobs/import"
      ]

instance ToQuery GetSegmentImportJobs where
  toQuery GetSegmentImportJobs' {..} =
    mconcat ["token" =: _gsijToken, "page-size" =: _gsijPageSize]

-- | /See:/ 'getSegmentImportJobsResponse' smart constructor.
data GetSegmentImportJobsResponse = GetSegmentImportJobsResponse'
  { _gsijrsResponseStatus ::
      !Int,
    _gsijrsImportJobsResponse ::
      !ImportJobsResponse
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetSegmentImportJobsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsijrsResponseStatus' - -- | The response status code.
--
-- * 'gsijrsImportJobsResponse' - Undocumented member.
getSegmentImportJobsResponse ::
  -- | 'gsijrsResponseStatus'
  Int ->
  -- | 'gsijrsImportJobsResponse'
  ImportJobsResponse ->
  GetSegmentImportJobsResponse
getSegmentImportJobsResponse pResponseStatus_ pImportJobsResponse_ =
  GetSegmentImportJobsResponse'
    { _gsijrsResponseStatus =
        pResponseStatus_,
      _gsijrsImportJobsResponse = pImportJobsResponse_
    }

-- | -- | The response status code.
gsijrsResponseStatus :: Lens' GetSegmentImportJobsResponse Int
gsijrsResponseStatus = lens _gsijrsResponseStatus (\s a -> s {_gsijrsResponseStatus = a})

-- | Undocumented member.
gsijrsImportJobsResponse :: Lens' GetSegmentImportJobsResponse ImportJobsResponse
gsijrsImportJobsResponse = lens _gsijrsImportJobsResponse (\s a -> s {_gsijrsImportJobsResponse = a})

instance NFData GetSegmentImportJobsResponse
