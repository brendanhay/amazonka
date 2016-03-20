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
-- Module      : Network.AWS.Glacier.ListJobs
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation lists jobs for a vault, including jobs that are
-- in-progress and jobs that have recently finished.
--
-- Amazon Glacier retains recently completed jobs for a period before
-- deleting them; however, it eventually removes completed jobs. The output
-- of completed jobs can be retrieved. Retaining completed jobs for a
-- period of time after they have completed enables you to get a job output
-- in the event you miss the job completion notification or your first
-- attempt to download it fails. For example, suppose you start an archive
-- retrieval job to download an archive. After the job completes, you start
-- to download the archive but encounter a network error. In this scenario,
-- you can retry and download the archive while the job exists.
--
-- To retrieve an archive or retrieve a vault inventory from Amazon
-- Glacier, you first initiate a job, and after the job completes, you
-- download the data. For an archive retrieval, the output is the archive
-- data, and for an inventory retrieval, it is the inventory list. The List
-- Job operation returns a list of these jobs sorted by job initiation
-- time.
--
-- This List Jobs operation supports pagination. By default, this operation
-- returns up to 1,000 jobs in the response. You should always check the
-- response for a 'marker' at which to continue the list; if there are no
-- more items the 'marker' is 'null'. To return a list of jobs that begins
-- at a specific job, set the 'marker' request parameter to the value you
-- obtained from a previous List Jobs request. You can also limit the
-- number of jobs returned in the response by specifying the 'limit'
-- parameter in the request.
--
-- Additionally, you can filter the jobs list returned by specifying an
-- optional 'statuscode' (InProgress, Succeeded, or Failed) and 'completed'
-- (true, false) parameter. The 'statuscode' allows you to specify that
-- only jobs that match a specified status are returned. The 'completed'
-- parameter allows you to specify that only jobs in a specific completion
-- state are returned.
--
-- An AWS account has full permission to perform all operations (actions).
-- However, AWS Identity and Access Management (IAM) users don\'t have any
-- permissions by default. You must grant them explicit permission to
-- perform specific actions. For more information, see
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)>.
--
-- For the underlying REST API, go to
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-jobs-get.html List Jobs>
--
-- This operation returns paginated results.
module Network.AWS.Glacier.ListJobs
    (
    -- * Creating a Request
      listJobs
    , ListJobs
    -- * Request Lenses
    , ljMarker
    , ljCompleted
    , ljLimit
    , ljStatuscode
    , ljAccountId
    , ljVaultName

    -- * Destructuring the Response
    , listJobsResponse
    , ListJobsResponse
    -- * Response Lenses
    , ljrsMarker
    , ljrsJobList
    , ljrsResponseStatus
    ) where

import           Network.AWS.Glacier.Types
import           Network.AWS.Glacier.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Provides options for retrieving a job list for an Amazon Glacier vault.
--
-- /See:/ 'listJobs' smart constructor.
data ListJobs = ListJobs'
    { _ljMarker     :: !(Maybe Text)
    , _ljCompleted  :: !(Maybe Text)
    , _ljLimit      :: !(Maybe Text)
    , _ljStatuscode :: !(Maybe Text)
    , _ljAccountId  :: !Text
    , _ljVaultName  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListJobs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ljMarker'
--
-- * 'ljCompleted'
--
-- * 'ljLimit'
--
-- * 'ljStatuscode'
--
-- * 'ljAccountId'
--
-- * 'ljVaultName'
listJobs
    :: Text -- ^ 'ljAccountId'
    -> Text -- ^ 'ljVaultName'
    -> ListJobs
listJobs pAccountId_ pVaultName_ =
    ListJobs'
    { _ljMarker = Nothing
    , _ljCompleted = Nothing
    , _ljLimit = Nothing
    , _ljStatuscode = Nothing
    , _ljAccountId = pAccountId_
    , _ljVaultName = pVaultName_
    }

-- | An opaque string used for pagination. This value specifies the job at
-- which the listing of jobs should begin. Get the marker value from a
-- previous List Jobs response. You need only include the marker if you are
-- continuing the pagination of results started in a previous List Jobs
-- request.
ljMarker :: Lens' ListJobs (Maybe Text)
ljMarker = lens _ljMarker (\ s a -> s{_ljMarker = a});

-- | Specifies the state of the jobs to return. You can specify 'true' or
-- 'false'.
ljCompleted :: Lens' ListJobs (Maybe Text)
ljCompleted = lens _ljCompleted (\ s a -> s{_ljCompleted = a});

-- | Specifies that the response be limited to the specified number of items
-- or fewer. If not specified, the List Jobs operation returns up to 1,000
-- jobs.
ljLimit :: Lens' ListJobs (Maybe Text)
ljLimit = lens _ljLimit (\ s a -> s{_ljLimit = a});

-- | Specifies the type of job status to return. You can specify the
-- following values: \"InProgress\", \"Succeeded\", or \"Failed\".
ljStatuscode :: Lens' ListJobs (Maybe Text)
ljStatuscode = lens _ljStatuscode (\ s a -> s{_ljStatuscode = a});

-- | The 'AccountId' value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- apos'-'apos (hyphen), in which case Amazon Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (apos-apos) in the ID.
ljAccountId :: Lens' ListJobs Text
ljAccountId = lens _ljAccountId (\ s a -> s{_ljAccountId = a});

-- | The name of the vault.
ljVaultName :: Lens' ListJobs Text
ljVaultName = lens _ljVaultName (\ s a -> s{_ljVaultName = a});

instance AWSPager ListJobs where
        page rq rs
          | stop (rs ^. ljrsMarker) = Nothing
          | stop (rs ^. ljrsJobList) = Nothing
          | otherwise =
            Just $ rq & ljMarker .~ rs ^. ljrsMarker

instance AWSRequest ListJobs where
        type Rs ListJobs = ListJobsResponse
        request = get glacier
        response
          = receiveJSON
              (\ s h x ->
                 ListJobsResponse' <$>
                   (x .?> "Marker") <*> (x .?> "JobList" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable ListJobs

instance ToHeaders ListJobs where
        toHeaders = const mempty

instance ToPath ListJobs where
        toPath ListJobs'{..}
          = mconcat
              ["/", toBS _ljAccountId, "/vaults/",
               toBS _ljVaultName, "/jobs"]

instance ToQuery ListJobs where
        toQuery ListJobs'{..}
          = mconcat
              ["marker" =: _ljMarker, "completed" =: _ljCompleted,
               "limit" =: _ljLimit, "statuscode" =: _ljStatuscode]

-- | Contains the Amazon Glacier response to your request.
--
-- /See:/ 'listJobsResponse' smart constructor.
data ListJobsResponse = ListJobsResponse'
    { _ljrsMarker         :: !(Maybe Text)
    , _ljrsJobList        :: !(Maybe [GlacierJobDescription])
    , _ljrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListJobsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ljrsMarker'
--
-- * 'ljrsJobList'
--
-- * 'ljrsResponseStatus'
listJobsResponse
    :: Int -- ^ 'ljrsResponseStatus'
    -> ListJobsResponse
listJobsResponse pResponseStatus_ =
    ListJobsResponse'
    { _ljrsMarker = Nothing
    , _ljrsJobList = Nothing
    , _ljrsResponseStatus = pResponseStatus_
    }

-- | An opaque string that represents where to continue pagination of the
-- results. You use this value in a new List Jobs request to obtain more
-- jobs in the list. If there are no more jobs, this value is 'null'.
ljrsMarker :: Lens' ListJobsResponse (Maybe Text)
ljrsMarker = lens _ljrsMarker (\ s a -> s{_ljrsMarker = a});

-- | A list of job objects. Each job object contains metadata describing the
-- job.
ljrsJobList :: Lens' ListJobsResponse [GlacierJobDescription]
ljrsJobList = lens _ljrsJobList (\ s a -> s{_ljrsJobList = a}) . _Default . _Coerce;

-- | The response status code.
ljrsResponseStatus :: Lens' ListJobsResponse Int
ljrsResponseStatus = lens _ljrsResponseStatus (\ s a -> s{_ljrsResponseStatus = a});
