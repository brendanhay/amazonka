{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.DeleteApplicationReferenceDataSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a reference data source configuration from the specified application configuration.
--
-- If the application is running, Amazon Kinesis Analytics immediately removes the in-application table that you created using the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_AddApplicationReferenceDataSource.html AddApplicationReferenceDataSource> operation. 
-- This operation requires permissions to perform the @kinesisanalytics.DeleteApplicationReferenceDataSource@ action.
module Network.AWS.KinesisAnalytics.DeleteApplicationReferenceDataSource
    (
    -- * Creating a request
      DeleteApplicationReferenceDataSource (..)
    , mkDeleteApplicationReferenceDataSource
    -- ** Request lenses
    , dardsApplicationName
    , dardsCurrentApplicationVersionId
    , dardsReferenceId

    -- * Destructuring the response
    , DeleteApplicationReferenceDataSourceResponse (..)
    , mkDeleteApplicationReferenceDataSourceResponse
    -- ** Response lenses
    , dardsrrsResponseStatus
    ) where

import qualified Network.AWS.KinesisAnalytics.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteApplicationReferenceDataSource' smart constructor.
data DeleteApplicationReferenceDataSource = DeleteApplicationReferenceDataSource'
  { applicationName :: Types.ApplicationName
    -- ^ Name of an existing application.
  , currentApplicationVersionId :: Core.Natural
    -- ^ Version of the application. You can use the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation to get the current application version. If the version specified is not the current version, the @ConcurrentModificationException@ is returned.
  , referenceId :: Types.ReferenceId
    -- ^ ID of the reference data source. When you add a reference data source to your application using the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_AddApplicationReferenceDataSource.html AddApplicationReferenceDataSource> , Amazon Kinesis Analytics assigns an ID. You can use the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation to get the reference ID. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteApplicationReferenceDataSource' value with any optional fields omitted.
mkDeleteApplicationReferenceDataSource
    :: Types.ApplicationName -- ^ 'applicationName'
    -> Core.Natural -- ^ 'currentApplicationVersionId'
    -> Types.ReferenceId -- ^ 'referenceId'
    -> DeleteApplicationReferenceDataSource
mkDeleteApplicationReferenceDataSource applicationName
  currentApplicationVersionId referenceId
  = DeleteApplicationReferenceDataSource'{applicationName,
                                          currentApplicationVersionId, referenceId}

-- | Name of an existing application.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dardsApplicationName :: Lens.Lens' DeleteApplicationReferenceDataSource Types.ApplicationName
dardsApplicationName = Lens.field @"applicationName"
{-# INLINEABLE dardsApplicationName #-}
{-# DEPRECATED applicationName "Use generic-lens or generic-optics with 'applicationName' instead"  #-}

-- | Version of the application. You can use the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation to get the current application version. If the version specified is not the current version, the @ConcurrentModificationException@ is returned.
--
-- /Note:/ Consider using 'currentApplicationVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dardsCurrentApplicationVersionId :: Lens.Lens' DeleteApplicationReferenceDataSource Core.Natural
dardsCurrentApplicationVersionId = Lens.field @"currentApplicationVersionId"
{-# INLINEABLE dardsCurrentApplicationVersionId #-}
{-# DEPRECATED currentApplicationVersionId "Use generic-lens or generic-optics with 'currentApplicationVersionId' instead"  #-}

-- | ID of the reference data source. When you add a reference data source to your application using the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_AddApplicationReferenceDataSource.html AddApplicationReferenceDataSource> , Amazon Kinesis Analytics assigns an ID. You can use the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation to get the reference ID. 
--
-- /Note:/ Consider using 'referenceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dardsReferenceId :: Lens.Lens' DeleteApplicationReferenceDataSource Types.ReferenceId
dardsReferenceId = Lens.field @"referenceId"
{-# INLINEABLE dardsReferenceId #-}
{-# DEPRECATED referenceId "Use generic-lens or generic-optics with 'referenceId' instead"  #-}

instance Core.ToQuery DeleteApplicationReferenceDataSource where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteApplicationReferenceDataSource where
        toHeaders DeleteApplicationReferenceDataSource{..}
          = Core.pure
              ("X-Amz-Target",
               "KinesisAnalytics_20150814.DeleteApplicationReferenceDataSource")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteApplicationReferenceDataSource where
        toJSON DeleteApplicationReferenceDataSource{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ApplicationName" Core..= applicationName),
                  Core.Just
                    ("CurrentApplicationVersionId" Core..=
                       currentApplicationVersionId),
                  Core.Just ("ReferenceId" Core..= referenceId)])

instance Core.AWSRequest DeleteApplicationReferenceDataSource where
        type Rs DeleteApplicationReferenceDataSource =
             DeleteApplicationReferenceDataSourceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteApplicationReferenceDataSourceResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteApplicationReferenceDataSourceResponse' smart constructor.
newtype DeleteApplicationReferenceDataSourceResponse = DeleteApplicationReferenceDataSourceResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteApplicationReferenceDataSourceResponse' value with any optional fields omitted.
mkDeleteApplicationReferenceDataSourceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteApplicationReferenceDataSourceResponse
mkDeleteApplicationReferenceDataSourceResponse responseStatus
  = DeleteApplicationReferenceDataSourceResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dardsrrsResponseStatus :: Lens.Lens' DeleteApplicationReferenceDataSourceResponse Core.Int
dardsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dardsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
