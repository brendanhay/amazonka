{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetSegmentExportJobs
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of export jobs for a specific segment.
module Network.AWS.Pinpoint.GetSegmentExportJobs
    (
    -- * Creating a Request
      getSegmentExportJobs
    , GetSegmentExportJobs
    -- * Request Lenses
    , gsejToken
    , gsejPageSize
    , gsejSegmentId
    , gsejApplicationId

    -- * Destructuring the Response
    , getSegmentExportJobsResponse
    , GetSegmentExportJobsResponse
    -- * Response Lenses
    , gsejrsResponseStatus
    , gsejrsExportJobsResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Pinpoint.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getSegmentExportJobs' smart constructor.
data GetSegmentExportJobs = GetSegmentExportJobs'
  { _gsejToken         :: !(Maybe Text)
  , _gsejPageSize      :: !(Maybe Text)
  , _gsejSegmentId     :: !Text
  , _gsejApplicationId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetSegmentExportJobs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsejToken' - The NextToken string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- * 'gsejPageSize' - The number of entries you want on each page in the response.
--
-- * 'gsejSegmentId' - Undocumented member.
--
-- * 'gsejApplicationId' - Undocumented member.
getSegmentExportJobs
    :: Text -- ^ 'gsejSegmentId'
    -> Text -- ^ 'gsejApplicationId'
    -> GetSegmentExportJobs
getSegmentExportJobs pSegmentId_ pApplicationId_ =
  GetSegmentExportJobs'
    { _gsejToken = Nothing
    , _gsejPageSize = Nothing
    , _gsejSegmentId = pSegmentId_
    , _gsejApplicationId = pApplicationId_
    }


-- | The NextToken string returned on a previous page that you use to get the next page of results in a paginated response.
gsejToken :: Lens' GetSegmentExportJobs (Maybe Text)
gsejToken = lens _gsejToken (\ s a -> s{_gsejToken = a})

-- | The number of entries you want on each page in the response.
gsejPageSize :: Lens' GetSegmentExportJobs (Maybe Text)
gsejPageSize = lens _gsejPageSize (\ s a -> s{_gsejPageSize = a})

-- | Undocumented member.
gsejSegmentId :: Lens' GetSegmentExportJobs Text
gsejSegmentId = lens _gsejSegmentId (\ s a -> s{_gsejSegmentId = a})

-- | Undocumented member.
gsejApplicationId :: Lens' GetSegmentExportJobs Text
gsejApplicationId = lens _gsejApplicationId (\ s a -> s{_gsejApplicationId = a})

instance AWSRequest GetSegmentExportJobs where
        type Rs GetSegmentExportJobs =
             GetSegmentExportJobsResponse
        request = get pinpoint
        response
          = receiveJSON
              (\ s h x ->
                 GetSegmentExportJobsResponse' <$>
                   (pure (fromEnum s)) <*> (eitherParseJSON x))

instance Hashable GetSegmentExportJobs where

instance NFData GetSegmentExportJobs where

instance ToHeaders GetSegmentExportJobs where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetSegmentExportJobs where
        toPath GetSegmentExportJobs'{..}
          = mconcat
              ["/v1/apps/", toBS _gsejApplicationId, "/segments/",
               toBS _gsejSegmentId, "/jobs/export"]

instance ToQuery GetSegmentExportJobs where
        toQuery GetSegmentExportJobs'{..}
          = mconcat
              ["token" =: _gsejToken, "page-size" =: _gsejPageSize]

-- | /See:/ 'getSegmentExportJobsResponse' smart constructor.
data GetSegmentExportJobsResponse = GetSegmentExportJobsResponse'
  { _gsejrsResponseStatus     :: !Int
  , _gsejrsExportJobsResponse :: !ExportJobsResponse
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetSegmentExportJobsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsejrsResponseStatus' - -- | The response status code.
--
-- * 'gsejrsExportJobsResponse' - Undocumented member.
getSegmentExportJobsResponse
    :: Int -- ^ 'gsejrsResponseStatus'
    -> ExportJobsResponse -- ^ 'gsejrsExportJobsResponse'
    -> GetSegmentExportJobsResponse
getSegmentExportJobsResponse pResponseStatus_ pExportJobsResponse_ =
  GetSegmentExportJobsResponse'
    { _gsejrsResponseStatus = pResponseStatus_
    , _gsejrsExportJobsResponse = pExportJobsResponse_
    }


-- | -- | The response status code.
gsejrsResponseStatus :: Lens' GetSegmentExportJobsResponse Int
gsejrsResponseStatus = lens _gsejrsResponseStatus (\ s a -> s{_gsejrsResponseStatus = a})

-- | Undocumented member.
gsejrsExportJobsResponse :: Lens' GetSegmentExportJobsResponse ExportJobsResponse
gsejrsExportJobsResponse = lens _gsejrsExportJobsResponse (\ s a -> s{_gsejrsExportJobsResponse = a})

instance NFData GetSegmentExportJobsResponse where
