{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.ListJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation lists jobs for a vault, including jobs that are in-progress and jobs that have recently finished. The List Job operation returns a list of these jobs sorted by job initiation time.
--
-- The List Jobs operation supports pagination. You should always check the response @Marker@ field. If there are no more jobs to list, the @Marker@ field is set to @null@ . If there are more jobs to list, the @Marker@ field is set to a non-null value, which you can use to continue the pagination of the list. To return a list of jobs that begins at a specific job, set the marker request parameter to the @Marker@ value for that job that you obtained from a previous List Jobs request.
-- You can set a maximum limit for the number of jobs returned in the response by specifying the @limit@ parameter in the request. The default limit is 50. The number of jobs returned might be fewer than the limit, but the number of returned jobs never exceeds the limit.
-- Additionally, you can filter the jobs list returned by specifying the optional @statuscode@ parameter or @completed@ parameter, or both. Using the @statuscode@ parameter, you can specify to return only jobs that match either the @InProgress@ , @Succeeded@ , or @Failed@ status. Using the @completed@ parameter, you can specify to return only jobs that were completed (@true@ ) or jobs that were not completed (@false@ ).
-- For more information about using this operation, see the documentation for the underlying REST API <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-jobs-get.html List Jobs> .
--
-- This operation returns paginated results.
module Network.AWS.Glacier.ListJobs
  ( -- * Creating a request
    ListJobs (..),
    mkListJobs,

    -- ** Request lenses
    ljMarker,
    ljCompleted,
    ljLimit,
    ljStatuscode,
    ljAccountId,
    ljVaultName,

    -- * Destructuring the response
    ListJobsResponse (..),
    mkListJobsResponse,

    -- ** Response lenses
    ljrsMarker,
    ljrsJobList,
    ljrsResponseStatus,
  )
where

import Network.AWS.Glacier.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Provides options for retrieving a job list for an Amazon S3 Glacier vault.
--
-- /See:/ 'mkListJobs' smart constructor.
data ListJobs = ListJobs'
  { marker :: Lude.Maybe Lude.Text,
    completed :: Lude.Maybe Lude.Text,
    limit :: Lude.Maybe Lude.Text,
    statuscode :: Lude.Maybe Lude.Text,
    accountId :: Lude.Text,
    vaultName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListJobs' with the minimum fields required to make a request.
--
-- * 'accountId' - The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
-- * 'completed' - The state of the jobs to return. You can specify @true@ or @false@ .
-- * 'limit' - The maximum number of jobs to be returned. The default limit is 50. The number of jobs returned might be fewer than the specified limit, but the number of returned jobs never exceeds the limit.
-- * 'marker' - An opaque string used for pagination. This value specifies the job at which the listing of jobs should begin. Get the marker value from a previous List Jobs response. You only need to include the marker if you are continuing the pagination of results started in a previous List Jobs request.
-- * 'statuscode' - The type of job status to return. You can specify the following values: @InProgress@ , @Succeeded@ , or @Failed@ .
-- * 'vaultName' - The name of the vault.
mkListJobs ::
  -- | 'accountId'
  Lude.Text ->
  -- | 'vaultName'
  Lude.Text ->
  ListJobs
mkListJobs pAccountId_ pVaultName_ =
  ListJobs'
    { marker = Lude.Nothing,
      completed = Lude.Nothing,
      limit = Lude.Nothing,
      statuscode = Lude.Nothing,
      accountId = pAccountId_,
      vaultName = pVaultName_
    }

-- | An opaque string used for pagination. This value specifies the job at which the listing of jobs should begin. Get the marker value from a previous List Jobs response. You only need to include the marker if you are continuing the pagination of results started in a previous List Jobs request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljMarker :: Lens.Lens' ListJobs (Lude.Maybe Lude.Text)
ljMarker = Lens.lens (marker :: ListJobs -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListJobs)
{-# DEPRECATED ljMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The state of the jobs to return. You can specify @true@ or @false@ .
--
-- /Note:/ Consider using 'completed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljCompleted :: Lens.Lens' ListJobs (Lude.Maybe Lude.Text)
ljCompleted = Lens.lens (completed :: ListJobs -> Lude.Maybe Lude.Text) (\s a -> s {completed = a} :: ListJobs)
{-# DEPRECATED ljCompleted "Use generic-lens or generic-optics with 'completed' instead." #-}

-- | The maximum number of jobs to be returned. The default limit is 50. The number of jobs returned might be fewer than the specified limit, but the number of returned jobs never exceeds the limit.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljLimit :: Lens.Lens' ListJobs (Lude.Maybe Lude.Text)
ljLimit = Lens.lens (limit :: ListJobs -> Lude.Maybe Lude.Text) (\s a -> s {limit = a} :: ListJobs)
{-# DEPRECATED ljLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The type of job status to return. You can specify the following values: @InProgress@ , @Succeeded@ , or @Failed@ .
--
-- /Note:/ Consider using 'statuscode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljStatuscode :: Lens.Lens' ListJobs (Lude.Maybe Lude.Text)
ljStatuscode = Lens.lens (statuscode :: ListJobs -> Lude.Maybe Lude.Text) (\s a -> s {statuscode = a} :: ListJobs)
{-# DEPRECATED ljStatuscode "Use generic-lens or generic-optics with 'statuscode' instead." #-}

-- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljAccountId :: Lens.Lens' ListJobs Lude.Text
ljAccountId = Lens.lens (accountId :: ListJobs -> Lude.Text) (\s a -> s {accountId = a} :: ListJobs)
{-# DEPRECATED ljAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The name of the vault.
--
-- /Note:/ Consider using 'vaultName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljVaultName :: Lens.Lens' ListJobs Lude.Text
ljVaultName = Lens.lens (vaultName :: ListJobs -> Lude.Text) (\s a -> s {vaultName = a} :: ListJobs)
{-# DEPRECATED ljVaultName "Use generic-lens or generic-optics with 'vaultName' instead." #-}

instance Page.AWSPager ListJobs where
  page rq rs
    | Page.stop (rs Lens.^. ljrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. ljrsJobList) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& ljMarker Lens..~ rs Lens.^. ljrsMarker

instance Lude.AWSRequest ListJobs where
  type Rs ListJobs = ListJobsResponse
  request = Req.get glacierService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListJobsResponse'
            Lude.<$> (x Lude..?> "Marker")
            Lude.<*> (x Lude..?> "JobList" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListJobs where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListJobs where
  toPath ListJobs' {..} =
    Lude.mconcat
      [ "/",
        Lude.toBS accountId,
        "/vaults/",
        Lude.toBS vaultName,
        "/jobs"
      ]

instance Lude.ToQuery ListJobs where
  toQuery ListJobs' {..} =
    Lude.mconcat
      [ "marker" Lude.=: marker,
        "completed" Lude.=: completed,
        "limit" Lude.=: limit,
        "statuscode" Lude.=: statuscode
      ]

-- | Contains the Amazon S3 Glacier response to your request.
--
-- /See:/ 'mkListJobsResponse' smart constructor.
data ListJobsResponse = ListJobsResponse'
  { marker ::
      Lude.Maybe Lude.Text,
    jobList :: Lude.Maybe [GlacierJobDescription],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListJobsResponse' with the minimum fields required to make a request.
--
-- * 'jobList' - A list of job objects. Each job object contains metadata describing the job.
-- * 'marker' - An opaque string used for pagination that specifies the job at which the listing of jobs should begin. You get the @marker@ value from a previous List Jobs response. You only need to include the marker if you are continuing the pagination of the results started in a previous List Jobs request.
-- * 'responseStatus' - The response status code.
mkListJobsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListJobsResponse
mkListJobsResponse pResponseStatus_ =
  ListJobsResponse'
    { marker = Lude.Nothing,
      jobList = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An opaque string used for pagination that specifies the job at which the listing of jobs should begin. You get the @marker@ value from a previous List Jobs response. You only need to include the marker if you are continuing the pagination of the results started in a previous List Jobs request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljrsMarker :: Lens.Lens' ListJobsResponse (Lude.Maybe Lude.Text)
ljrsMarker = Lens.lens (marker :: ListJobsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListJobsResponse)
{-# DEPRECATED ljrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A list of job objects. Each job object contains metadata describing the job.
--
-- /Note:/ Consider using 'jobList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljrsJobList :: Lens.Lens' ListJobsResponse (Lude.Maybe [GlacierJobDescription])
ljrsJobList = Lens.lens (jobList :: ListJobsResponse -> Lude.Maybe [GlacierJobDescription]) (\s a -> s {jobList = a} :: ListJobsResponse)
{-# DEPRECATED ljrsJobList "Use generic-lens or generic-optics with 'jobList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljrsResponseStatus :: Lens.Lens' ListJobsResponse Lude.Int
ljrsResponseStatus = Lens.lens (responseStatus :: ListJobsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListJobsResponse)
{-# DEPRECATED ljrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
