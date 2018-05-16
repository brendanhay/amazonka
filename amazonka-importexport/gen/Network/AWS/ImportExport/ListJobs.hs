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
-- Module      : Network.AWS.ImportExport.ListJobs
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns the jobs associated with the requester. AWS Import/Export lists the jobs in reverse chronological order based on the date of creation. For example if Job Test1 was created 2009Dec30 and Test2 was created 2010Feb05, the ListJobs operation would return Test2 followed by Test1.
--
-- This operation returns paginated results.
module Network.AWS.ImportExport.ListJobs
    (
    -- * Creating a Request
      listJobs
    , ListJobs
    -- * Request Lenses
    , ljAPIVersion
    , ljMarker
    , ljMaxJobs

    -- * Destructuring the Response
    , listJobsResponse
    , ListJobsResponse
    -- * Response Lenses
    , ljrsJobs
    , ljrsIsTruncated
    , ljrsResponseStatus
    ) where

import Network.AWS.ImportExport.Types
import Network.AWS.ImportExport.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Input structure for the ListJobs operation.
--
-- /See:/ 'listJobs' smart constructor.
data ListJobs = ListJobs'
  { _ljAPIVersion :: !(Maybe Text)
  , _ljMarker     :: !(Maybe Text)
  , _ljMaxJobs    :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListJobs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ljAPIVersion' - Undocumented member.
--
-- * 'ljMarker' - Undocumented member.
--
-- * 'ljMaxJobs' - Undocumented member.
listJobs
    :: ListJobs
listJobs =
  ListJobs' {_ljAPIVersion = Nothing, _ljMarker = Nothing, _ljMaxJobs = Nothing}


-- | Undocumented member.
ljAPIVersion :: Lens' ListJobs (Maybe Text)
ljAPIVersion = lens _ljAPIVersion (\ s a -> s{_ljAPIVersion = a})

-- | Undocumented member.
ljMarker :: Lens' ListJobs (Maybe Text)
ljMarker = lens _ljMarker (\ s a -> s{_ljMarker = a})

-- | Undocumented member.
ljMaxJobs :: Lens' ListJobs (Maybe Int)
ljMaxJobs = lens _ljMaxJobs (\ s a -> s{_ljMaxJobs = a})

instance AWSPager ListJobs where
        page rq rs
          | stop (rs ^. ljrsIsTruncated) = Nothing
          | isNothing (rs ^? ljrsJobs . _last . jobJobId) =
            Nothing
          | otherwise =
            Just $ rq &
              ljMarker .~ rs ^? ljrsJobs . _last . jobJobId

instance AWSRequest ListJobs where
        type Rs ListJobs = ListJobsResponse
        request = postQuery importExport
        response
          = receiveXMLWrapper "ListJobsResult"
              (\ s h x ->
                 ListJobsResponse' <$>
                   (x .@? "Jobs" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (x .@? "IsTruncated")
                     <*> (pure (fromEnum s)))

instance Hashable ListJobs where

instance NFData ListJobs where

instance ToHeaders ListJobs where
        toHeaders = const mempty

instance ToPath ListJobs where
        toPath = const "/"

instance ToQuery ListJobs where
        toQuery ListJobs'{..}
          = mconcat
              ["Operation=ListJobs",
               "Action" =: ("ListJobs" :: ByteString),
               "Version" =: ("2010-06-01" :: ByteString),
               "APIVersion" =: _ljAPIVersion, "Marker" =: _ljMarker,
               "MaxJobs" =: _ljMaxJobs]

-- | Output structure for the ListJobs operation.
--
-- /See:/ 'listJobsResponse' smart constructor.
data ListJobsResponse = ListJobsResponse'
  { _ljrsJobs           :: !(Maybe [Job])
  , _ljrsIsTruncated    :: !(Maybe Bool)
  , _ljrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListJobsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ljrsJobs' - Undocumented member.
--
-- * 'ljrsIsTruncated' - Undocumented member.
--
-- * 'ljrsResponseStatus' - -- | The response status code.
listJobsResponse
    :: Int -- ^ 'ljrsResponseStatus'
    -> ListJobsResponse
listJobsResponse pResponseStatus_ =
  ListJobsResponse'
    { _ljrsJobs = Nothing
    , _ljrsIsTruncated = Nothing
    , _ljrsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
ljrsJobs :: Lens' ListJobsResponse [Job]
ljrsJobs = lens _ljrsJobs (\ s a -> s{_ljrsJobs = a}) . _Default . _Coerce

-- | Undocumented member.
ljrsIsTruncated :: Lens' ListJobsResponse (Maybe Bool)
ljrsIsTruncated = lens _ljrsIsTruncated (\ s a -> s{_ljrsIsTruncated = a})

-- | -- | The response status code.
ljrsResponseStatus :: Lens' ListJobsResponse Int
ljrsResponseStatus = lens _ljrsResponseStatus (\ s a -> s{_ljrsResponseStatus = a})

instance NFData ListJobsResponse where
