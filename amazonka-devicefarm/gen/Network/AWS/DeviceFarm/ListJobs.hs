{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.ListJobs
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Gets information about jobs.
--
-- <http://docs.aws.amazon.com/devicefarm/latest/APIReference/API_ListJobs.html>
module Network.AWS.DeviceFarm.ListJobs
    (
    -- * Request
      ListJobs
    -- ** Request constructor
    , listJobs
    -- ** Request lenses
    , ljNextToken
    , ljArn

    -- * Response
    , ListJobsResponse
    -- ** Response constructor
    , listJobsResponse
    -- ** Response lenses
    , ljrJobs
    , ljrNextToken
    , ljrStatus
    ) where

import           Network.AWS.DeviceFarm.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents a request to the list jobs operation.
--
-- /See:/ 'listJobs' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ljNextToken'
--
-- * 'ljArn'
data ListJobs = ListJobs'
    { _ljNextToken :: !(Maybe Text)
    , _ljArn       :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListJobs' smart constructor.
listJobs :: Text -> ListJobs
listJobs pArn =
    ListJobs'
    { _ljNextToken = Nothing
    , _ljArn = pArn
    }

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
ljNextToken :: Lens' ListJobs (Maybe Text)
ljNextToken = lens _ljNextToken (\ s a -> s{_ljNextToken = a});

-- | The jobs\' ARNs.
ljArn :: Lens' ListJobs Text
ljArn = lens _ljArn (\ s a -> s{_ljArn = a});

instance AWSRequest ListJobs where
        type Sv ListJobs = DeviceFarm
        type Rs ListJobs = ListJobsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListJobsResponse' <$>
                   (x .?> "jobs" .!@ mempty) <*> (x .?> "nextToken") <*>
                     (pure (fromEnum s)))

instance ToHeaders ListJobs where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.ListJobs" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListJobs where
        toJSON ListJobs'{..}
          = object
              ["nextToken" .= _ljNextToken, "arn" .= _ljArn]

instance ToPath ListJobs where
        toPath = const "/"

instance ToQuery ListJobs where
        toQuery = const mempty

-- | Represents the result of a list jobs request.
--
-- /See:/ 'listJobsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ljrJobs'
--
-- * 'ljrNextToken'
--
-- * 'ljrStatus'
data ListJobsResponse = ListJobsResponse'
    { _ljrJobs      :: !(Maybe [Job])
    , _ljrNextToken :: !(Maybe Text)
    , _ljrStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListJobsResponse' smart constructor.
listJobsResponse :: Int -> ListJobsResponse
listJobsResponse pStatus =
    ListJobsResponse'
    { _ljrJobs = Nothing
    , _ljrNextToken = Nothing
    , _ljrStatus = pStatus
    }

-- | Information about the jobs.
ljrJobs :: Lens' ListJobsResponse [Job]
ljrJobs = lens _ljrJobs (\ s a -> s{_ljrJobs = a}) . _Default;

-- | If the number of items that are returned is significantly large, this is
-- an identifier that is also returned, which can be used in a subsequent
-- call to this operation to return the next set of items in the list.
ljrNextToken :: Lens' ListJobsResponse (Maybe Text)
ljrNextToken = lens _ljrNextToken (\ s a -> s{_ljrNextToken = a});

-- | FIXME: Undocumented member.
ljrStatus :: Lens' ListJobsResponse Int
ljrStatus = lens _ljrStatus (\ s a -> s{_ljrStatus = a});
