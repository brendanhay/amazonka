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
-- Module      : Network.AWS.Pinpoint.GetImportJobs
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about your import jobs.
module Network.AWS.Pinpoint.GetImportJobs
    (
    -- * Creating a Request
      getImportJobs
    , GetImportJobs
    -- * Request Lenses
    , gijsToken
    , gijsPageSize
    , gijsApplicationId

    -- * Destructuring the Response
    , getImportJobsResponse
    , GetImportJobsResponse
    -- * Response Lenses
    , gijsrsResponseStatus
    , gijsrsImportJobsResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Pinpoint.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getImportJobs' smart constructor.
data GetImportJobs = GetImportJobs'
  { _gijsToken         :: !(Maybe Text)
  , _gijsPageSize      :: !(Maybe Text)
  , _gijsApplicationId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetImportJobs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gijsToken' - The NextToken string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- * 'gijsPageSize' - The number of entries you want on each page in the response.
--
-- * 'gijsApplicationId' - Undocumented member.
getImportJobs
    :: Text -- ^ 'gijsApplicationId'
    -> GetImportJobs
getImportJobs pApplicationId_ =
  GetImportJobs'
    { _gijsToken = Nothing
    , _gijsPageSize = Nothing
    , _gijsApplicationId = pApplicationId_
    }


-- | The NextToken string returned on a previous page that you use to get the next page of results in a paginated response.
gijsToken :: Lens' GetImportJobs (Maybe Text)
gijsToken = lens _gijsToken (\ s a -> s{_gijsToken = a})

-- | The number of entries you want on each page in the response.
gijsPageSize :: Lens' GetImportJobs (Maybe Text)
gijsPageSize = lens _gijsPageSize (\ s a -> s{_gijsPageSize = a})

-- | Undocumented member.
gijsApplicationId :: Lens' GetImportJobs Text
gijsApplicationId = lens _gijsApplicationId (\ s a -> s{_gijsApplicationId = a})

instance AWSRequest GetImportJobs where
        type Rs GetImportJobs = GetImportJobsResponse
        request = get pinpoint
        response
          = receiveJSON
              (\ s h x ->
                 GetImportJobsResponse' <$>
                   (pure (fromEnum s)) <*> (eitherParseJSON x))

instance Hashable GetImportJobs where

instance NFData GetImportJobs where

instance ToHeaders GetImportJobs where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetImportJobs where
        toPath GetImportJobs'{..}
          = mconcat
              ["/v1/apps/", toBS _gijsApplicationId,
               "/jobs/import"]

instance ToQuery GetImportJobs where
        toQuery GetImportJobs'{..}
          = mconcat
              ["token" =: _gijsToken, "page-size" =: _gijsPageSize]

-- | /See:/ 'getImportJobsResponse' smart constructor.
data GetImportJobsResponse = GetImportJobsResponse'
  { _gijsrsResponseStatus     :: !Int
  , _gijsrsImportJobsResponse :: !ImportJobsResponse
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetImportJobsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gijsrsResponseStatus' - -- | The response status code.
--
-- * 'gijsrsImportJobsResponse' - Undocumented member.
getImportJobsResponse
    :: Int -- ^ 'gijsrsResponseStatus'
    -> ImportJobsResponse -- ^ 'gijsrsImportJobsResponse'
    -> GetImportJobsResponse
getImportJobsResponse pResponseStatus_ pImportJobsResponse_ =
  GetImportJobsResponse'
    { _gijsrsResponseStatus = pResponseStatus_
    , _gijsrsImportJobsResponse = pImportJobsResponse_
    }


-- | -- | The response status code.
gijsrsResponseStatus :: Lens' GetImportJobsResponse Int
gijsrsResponseStatus = lens _gijsrsResponseStatus (\ s a -> s{_gijsrsResponseStatus = a})

-- | Undocumented member.
gijsrsImportJobsResponse :: Lens' GetImportJobsResponse ImportJobsResponse
gijsrsImportJobsResponse = lens _gijsrsImportJobsResponse (\ s a -> s{_gijsrsImportJobsResponse = a})

instance NFData GetImportJobsResponse where
