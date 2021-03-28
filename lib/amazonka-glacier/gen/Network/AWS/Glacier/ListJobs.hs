{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ListJobs (..)
    , mkListJobs
    -- ** Request lenses
    , ljAccountId
    , ljVaultName
    , ljCompleted
    , ljLimit
    , ljMarker
    , ljStatuscode

    -- * Destructuring the response
    , ListJobsResponse (..)
    , mkListJobsResponse
    -- ** Response lenses
    , ljrrsJobList
    , ljrrsMarker
    , ljrrsResponseStatus
    ) where

import qualified Network.AWS.Glacier.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Provides options for retrieving a job list for an Amazon S3 Glacier vault.
--
-- /See:/ 'mkListJobs' smart constructor.
data ListJobs = ListJobs'
  { accountId :: Core.Text
    -- ^ The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID. 
  , vaultName :: Core.Text
    -- ^ The name of the vault.
  , completed :: Core.Maybe Core.Text
    -- ^ The state of the jobs to return. You can specify @true@ or @false@ .
  , limit :: Core.Maybe Core.Text
    -- ^ The maximum number of jobs to be returned. The default limit is 50. The number of jobs returned might be fewer than the specified limit, but the number of returned jobs never exceeds the limit.
  , marker :: Core.Maybe Core.Text
    -- ^ An opaque string used for pagination. This value specifies the job at which the listing of jobs should begin. Get the marker value from a previous List Jobs response. You only need to include the marker if you are continuing the pagination of results started in a previous List Jobs request.
  , statuscode :: Core.Maybe Core.Text
    -- ^ The type of job status to return. You can specify the following values: @InProgress@ , @Succeeded@ , or @Failed@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListJobs' value with any optional fields omitted.
mkListJobs
    :: Core.Text -- ^ 'accountId'
    -> Core.Text -- ^ 'vaultName'
    -> ListJobs
mkListJobs accountId vaultName
  = ListJobs'{accountId, vaultName, completed = Core.Nothing,
              limit = Core.Nothing, marker = Core.Nothing,
              statuscode = Core.Nothing}

-- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID. 
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljAccountId :: Lens.Lens' ListJobs Core.Text
ljAccountId = Lens.field @"accountId"
{-# INLINEABLE ljAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

-- | The name of the vault.
--
-- /Note:/ Consider using 'vaultName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljVaultName :: Lens.Lens' ListJobs Core.Text
ljVaultName = Lens.field @"vaultName"
{-# INLINEABLE ljVaultName #-}
{-# DEPRECATED vaultName "Use generic-lens or generic-optics with 'vaultName' instead"  #-}

-- | The state of the jobs to return. You can specify @true@ or @false@ .
--
-- /Note:/ Consider using 'completed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljCompleted :: Lens.Lens' ListJobs (Core.Maybe Core.Text)
ljCompleted = Lens.field @"completed"
{-# INLINEABLE ljCompleted #-}
{-# DEPRECATED completed "Use generic-lens or generic-optics with 'completed' instead"  #-}

-- | The maximum number of jobs to be returned. The default limit is 50. The number of jobs returned might be fewer than the specified limit, but the number of returned jobs never exceeds the limit.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljLimit :: Lens.Lens' ListJobs (Core.Maybe Core.Text)
ljLimit = Lens.field @"limit"
{-# INLINEABLE ljLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | An opaque string used for pagination. This value specifies the job at which the listing of jobs should begin. Get the marker value from a previous List Jobs response. You only need to include the marker if you are continuing the pagination of results started in a previous List Jobs request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljMarker :: Lens.Lens' ListJobs (Core.Maybe Core.Text)
ljMarker = Lens.field @"marker"
{-# INLINEABLE ljMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The type of job status to return. You can specify the following values: @InProgress@ , @Succeeded@ , or @Failed@ .
--
-- /Note:/ Consider using 'statuscode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljStatuscode :: Lens.Lens' ListJobs (Core.Maybe Core.Text)
ljStatuscode = Lens.field @"statuscode"
{-# INLINEABLE ljStatuscode #-}
{-# DEPRECATED statuscode "Use generic-lens or generic-optics with 'statuscode' instead"  #-}

instance Core.ToQuery ListJobs where
        toQuery ListJobs{..}
          = Core.maybe Core.mempty (Core.toQueryPair "completed") completed
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "limit") limit
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "statuscode") statuscode

instance Core.ToHeaders ListJobs where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListJobs where
        type Rs ListJobs = ListJobsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/" Core.<> Core.toText accountId Core.<> "/vaults/" Core.<>
                             Core.toText vaultName
                             Core.<> "/jobs",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListJobsResponse' Core.<$>
                   (x Core..:? "JobList") Core.<*> x Core..:? "Marker" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListJobs where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"jobList" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | Contains the Amazon S3 Glacier response to your request.
--
-- /See:/ 'mkListJobsResponse' smart constructor.
data ListJobsResponse = ListJobsResponse'
  { jobList :: Core.Maybe [Types.GlacierJobDescription]
    -- ^ A list of job objects. Each job object contains metadata describing the job.
  , marker :: Core.Maybe Core.Text
    -- ^ An opaque string used for pagination that specifies the job at which the listing of jobs should begin. You get the @marker@ value from a previous List Jobs response. You only need to include the marker if you are continuing the pagination of the results started in a previous List Jobs request. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListJobsResponse' value with any optional fields omitted.
mkListJobsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListJobsResponse
mkListJobsResponse responseStatus
  = ListJobsResponse'{jobList = Core.Nothing, marker = Core.Nothing,
                      responseStatus}

-- | A list of job objects. Each job object contains metadata describing the job.
--
-- /Note:/ Consider using 'jobList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljrrsJobList :: Lens.Lens' ListJobsResponse (Core.Maybe [Types.GlacierJobDescription])
ljrrsJobList = Lens.field @"jobList"
{-# INLINEABLE ljrrsJobList #-}
{-# DEPRECATED jobList "Use generic-lens or generic-optics with 'jobList' instead"  #-}

-- | An opaque string used for pagination that specifies the job at which the listing of jobs should begin. You get the @marker@ value from a previous List Jobs response. You only need to include the marker if you are continuing the pagination of the results started in a previous List Jobs request. 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljrrsMarker :: Lens.Lens' ListJobsResponse (Core.Maybe Core.Text)
ljrrsMarker = Lens.field @"marker"
{-# INLINEABLE ljrrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljrrsResponseStatus :: Lens.Lens' ListJobsResponse Core.Int
ljrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ljrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
