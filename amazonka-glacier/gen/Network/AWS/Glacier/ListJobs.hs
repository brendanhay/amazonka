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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation lists jobs for a vault, including jobs that are in-progress and jobs that have recently finished. The List Job operation returns a list of these jobs sorted by job initiation time.
--
--
-- The List Jobs operation supports pagination. You should always check the response @Marker@ field. If there are no more jobs to list, the @Marker@ field is set to @null@ . If there are more jobs to list, the @Marker@ field is set to a non-null value, which you can use to continue the pagination of the list. To return a list of jobs that begins at a specific job, set the marker request parameter to the @Marker@ value for that job that you obtained from a previous List Jobs request.
--
-- You can set a maximum limit for the number of jobs returned in the response by specifying the @limit@ parameter in the request. The default limit is 1000. The number of jobs returned might be fewer than the limit, but the number of returned jobs never exceeds the limit.
--
-- Additionally, you can filter the jobs list returned by specifying the optional @statuscode@ parameter or @completed@ parameter, or both. Using the @statuscode@ parameter, you can specify to return only jobs that match either the @InProgress@ , @Succeeded@ , or @Failed@ status. Using the @completed@ parameter, you can specify to return only jobs that were completed (@true@ ) or jobs that were not completed (@false@ ).
--
-- For more information about using this operation, see the documentation for the underlying REST API <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-jobs-get.html List Jobs> .
--
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

import Network.AWS.Glacier.Types
import Network.AWS.Glacier.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Provides options for retrieving a job list for an Amazon Glacier vault.
--
--
--
-- /See:/ 'listJobs' smart constructor.
data ListJobs = ListJobs'
  { _ljMarker     :: !(Maybe Text)
  , _ljCompleted  :: !(Maybe Text)
  , _ljLimit      :: !(Maybe Text)
  , _ljStatuscode :: !(Maybe Text)
  , _ljAccountId  :: !Text
  , _ljVaultName  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListJobs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ljMarker' - An opaque string used for pagination. This value specifies the job at which the listing of jobs should begin. Get the marker value from a previous List Jobs response. You only need to include the marker if you are continuing the pagination of results started in a previous List Jobs request.
--
-- * 'ljCompleted' - The state of the jobs to return. You can specify @true@ or @false@ .
--
-- * 'ljLimit' - The maximum number of jobs to be returned. The default limit is 1000. The number of jobs returned might be fewer than the specified limit, but the number of returned jobs never exceeds the limit.
--
-- * 'ljStatuscode' - The type of job status to return. You can specify the following values: @InProgress@ , @Succeeded@ , or @Failed@ .
--
-- * 'ljAccountId' - The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
--
-- * 'ljVaultName' - The name of the vault.
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


-- | An opaque string used for pagination. This value specifies the job at which the listing of jobs should begin. Get the marker value from a previous List Jobs response. You only need to include the marker if you are continuing the pagination of results started in a previous List Jobs request.
ljMarker :: Lens' ListJobs (Maybe Text)
ljMarker = lens _ljMarker (\ s a -> s{_ljMarker = a})

-- | The state of the jobs to return. You can specify @true@ or @false@ .
ljCompleted :: Lens' ListJobs (Maybe Text)
ljCompleted = lens _ljCompleted (\ s a -> s{_ljCompleted = a})

-- | The maximum number of jobs to be returned. The default limit is 1000. The number of jobs returned might be fewer than the specified limit, but the number of returned jobs never exceeds the limit.
ljLimit :: Lens' ListJobs (Maybe Text)
ljLimit = lens _ljLimit (\ s a -> s{_ljLimit = a})

-- | The type of job status to return. You can specify the following values: @InProgress@ , @Succeeded@ , or @Failed@ .
ljStatuscode :: Lens' ListJobs (Maybe Text)
ljStatuscode = lens _ljStatuscode (\ s a -> s{_ljStatuscode = a})

-- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
ljAccountId :: Lens' ListJobs Text
ljAccountId = lens _ljAccountId (\ s a -> s{_ljAccountId = a})

-- | The name of the vault.
ljVaultName :: Lens' ListJobs Text
ljVaultName = lens _ljVaultName (\ s a -> s{_ljVaultName = a})

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

instance Hashable ListJobs where

instance NFData ListJobs where

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
--
--
-- /See:/ 'listJobsResponse' smart constructor.
data ListJobsResponse = ListJobsResponse'
  { _ljrsMarker         :: !(Maybe Text)
  , _ljrsJobList        :: !(Maybe [GlacierJobDescription])
  , _ljrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListJobsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ljrsMarker' - An opaque string used for pagination that specifies the job at which the listing of jobs should begin. You get the @marker@ value from a previous List Jobs response. You only need to include the marker if you are continuing the pagination of the results started in a previous List Jobs request.
--
-- * 'ljrsJobList' - A list of job objects. Each job object contains metadata describing the job.
--
-- * 'ljrsResponseStatus' - -- | The response status code.
listJobsResponse
    :: Int -- ^ 'ljrsResponseStatus'
    -> ListJobsResponse
listJobsResponse pResponseStatus_ =
  ListJobsResponse'
    { _ljrsMarker = Nothing
    , _ljrsJobList = Nothing
    , _ljrsResponseStatus = pResponseStatus_
    }


-- | An opaque string used for pagination that specifies the job at which the listing of jobs should begin. You get the @marker@ value from a previous List Jobs response. You only need to include the marker if you are continuing the pagination of the results started in a previous List Jobs request.
ljrsMarker :: Lens' ListJobsResponse (Maybe Text)
ljrsMarker = lens _ljrsMarker (\ s a -> s{_ljrsMarker = a})

-- | A list of job objects. Each job object contains metadata describing the job.
ljrsJobList :: Lens' ListJobsResponse [GlacierJobDescription]
ljrsJobList = lens _ljrsJobList (\ s a -> s{_ljrsJobList = a}) . _Default . _Coerce

-- | -- | The response status code.
ljrsResponseStatus :: Lens' ListJobsResponse Int
ljrsResponseStatus = lens _ljrsResponseStatus (\ s a -> s{_ljrsResponseStatus = a})

instance NFData ListJobsResponse where
