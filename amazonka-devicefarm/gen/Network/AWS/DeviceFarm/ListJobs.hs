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
-- Module      : Network.AWS.DeviceFarm.ListJobs
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about jobs.
--
-- /See:/ <http://docs.aws.amazon.com/devicefarm/latest/APIReference/API_ListJobs.html AWS API Reference> for ListJobs.
module Network.AWS.DeviceFarm.ListJobs
    (
    -- * Creating a Request
      ListJobs
    , listJobs
    -- * Request Lenses
    , ljNextToken
    , ljArn

    -- * Destructuring the Response
    , ListJobsResponse
    , listJobsResponse
    -- * Response Lenses
    , ljrsJobs
    , ljrsNextToken
    , ljrsStatus
    ) where

import Network.AWS.DeviceFarm.Types
import Network.AWS.DeviceFarm.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

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
    , _ljArn :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListJobs' smart constructor.
listJobs :: Text -> ListJobs
listJobs pArn_ = 
    ListJobs'
    { _ljNextToken = Nothing
    , _ljArn = pArn_
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
-- * 'ljrsJobs'
--
-- * 'ljrsNextToken'
--
-- * 'ljrsStatus'
data ListJobsResponse = ListJobsResponse'
    { _ljrsJobs :: !(Maybe [Job])
    , _ljrsNextToken :: !(Maybe Text)
    , _ljrsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListJobsResponse' smart constructor.
listJobsResponse :: Int -> ListJobsResponse
listJobsResponse pStatus_ = 
    ListJobsResponse'
    { _ljrsJobs = Nothing
    , _ljrsNextToken = Nothing
    , _ljrsStatus = pStatus_
    }

-- | Information about the jobs.
ljrsJobs :: Lens' ListJobsResponse [Job]
ljrsJobs = lens _ljrsJobs (\ s a -> s{_ljrsJobs = a}) . _Default . _Coerce;

-- | If the number of items that are returned is significantly large, this is
-- an identifier that is also returned, which can be used in a subsequent
-- call to this operation to return the next set of items in the list.
ljrsNextToken :: Lens' ListJobsResponse (Maybe Text)
ljrsNextToken = lens _ljrsNextToken (\ s a -> s{_ljrsNextToken = a});

-- | Undocumented member.
ljrsStatus :: Lens' ListJobsResponse Int
ljrsStatus = lens _ljrsStatus (\ s a -> s{_ljrsStatus = a});
