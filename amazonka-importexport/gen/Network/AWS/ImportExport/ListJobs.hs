{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.ImportExport.ListJobs
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | This operation returns the jobs associated with the requester. AWS
-- Import\/Export lists the jobs in reverse chronological order based on
-- the date of creation. For example if Job Test1 was created 2009Dec30 and
-- Test2 was created 2010Feb05, the ListJobs operation would return Test2
-- followed by Test1.
--
-- <http://docs.aws.amazon.com/AWSImportExport/latest/DG/WebListJobs.html>
module Network.AWS.ImportExport.ListJobs
    (
    -- * Request
      ListJobs
    -- ** Request constructor
    , listJobs
    -- ** Request lenses
    , ljAPIVersion
    , ljMarker
    , ljMaxJobs

    -- * Response
    , ListJobsResponse
    -- ** Response constructor
    , listJobsResponse
    -- ** Response lenses
    , ljrJobs
    , ljrIsTruncated
    , ljrStatus
    ) where

import           Network.AWS.ImportExport.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Input structure for the ListJobs operation.
--
-- /See:/ 'listJobs' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ljAPIVersion'
--
-- * 'ljMarker'
--
-- * 'ljMaxJobs'
data ListJobs = ListJobs'
    { _ljAPIVersion :: !(Maybe Text)
    , _ljMarker     :: !(Maybe Text)
    , _ljMaxJobs    :: !(Maybe Int)
    } deriving (Eq,Read,Show)

-- | 'ListJobs' smart constructor.
listJobs :: ListJobs
listJobs =
    ListJobs'
    { _ljAPIVersion = Nothing
    , _ljMarker = Nothing
    , _ljMaxJobs = Nothing
    }

-- | FIXME: Undocumented member.
ljAPIVersion :: Lens' ListJobs (Maybe Text)
ljAPIVersion = lens _ljAPIVersion (\ s a -> s{_ljAPIVersion = a});

-- | FIXME: Undocumented member.
ljMarker :: Lens' ListJobs (Maybe Text)
ljMarker = lens _ljMarker (\ s a -> s{_ljMarker = a});

-- | FIXME: Undocumented member.
ljMaxJobs :: Lens' ListJobs (Maybe Int)
ljMaxJobs = lens _ljMaxJobs (\ s a -> s{_ljMaxJobs = a});

instance AWSPager ListJobs where
        page rq rs
          | stop (rs ^. ljrIsTruncated) = Nothing
          | isNothing (rs ^? ljrJobs . _last . jobJobId) =
            Nothing
          | otherwise =
            Just $ rq &
              ljMarker .~ rs ^? ljrJobs . _last . jobJobId

instance AWSRequest ListJobs where
        type Sv ListJobs = ImportExport
        type Rs ListJobs = ListJobsResponse
        request = post
        response
          = receiveXMLWrapper "ListJobsResult"
              (\ s h x ->
                 ListJobsResponse' <$>
                   (x .@? "Jobs" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (x .@? "IsTruncated")
                     <*> (pure (fromEnum s)))

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
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ljrJobs'
--
-- * 'ljrIsTruncated'
--
-- * 'ljrStatus'
data ListJobsResponse = ListJobsResponse'
    { _ljrJobs        :: !(Maybe [Job])
    , _ljrIsTruncated :: !(Maybe Bool)
    , _ljrStatus      :: !Int
    } deriving (Eq,Read,Show)

-- | 'ListJobsResponse' smart constructor.
listJobsResponse :: Int -> ListJobsResponse
listJobsResponse pStatus =
    ListJobsResponse'
    { _ljrJobs = Nothing
    , _ljrIsTruncated = Nothing
    , _ljrStatus = pStatus
    }

-- | FIXME: Undocumented member.
ljrJobs :: Lens' ListJobsResponse [Job]
ljrJobs = lens _ljrJobs (\ s a -> s{_ljrJobs = a}) . _Default;

-- | FIXME: Undocumented member.
ljrIsTruncated :: Lens' ListJobsResponse (Maybe Bool)
ljrIsTruncated = lens _ljrIsTruncated (\ s a -> s{_ljrIsTruncated = a});

-- | FIXME: Undocumented member.
ljrStatus :: Lens' ListJobsResponse Int
ljrStatus = lens _ljrStatus (\ s a -> s{_ljrStatus = a});
