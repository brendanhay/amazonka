{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DescribeRecord
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified request operation.
--
-- Use this operation after calling a request operation (for example, 'ProvisionProduct' , 'TerminateProvisionedProduct' , or 'UpdateProvisionedProduct' ). 
module Network.AWS.ServiceCatalog.DescribeRecord
    (
    -- * Creating a request
      DescribeRecord (..)
    , mkDescribeRecord
    -- ** Request lenses
    , drId
    , drAcceptLanguage
    , drPageSize
    , drPageToken

    -- * Destructuring the response
    , DescribeRecordResponse (..)
    , mkDescribeRecordResponse
    -- ** Response lenses
    , drrrsNextPageToken
    , drrrsRecordDetail
    , drrrsRecordOutputs
    , drrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkDescribeRecord' smart constructor.
data DescribeRecord = DescribeRecord'
  { id :: Types.Id
    -- ^ The record identifier of the provisioned product. This identifier is returned by the request operation.
  , acceptLanguage :: Core.Maybe Types.AcceptLanguage
    -- ^ The language code.
--
--
--     * @en@ - English (default)
--
--
--     * @jp@ - Japanese
--
--
--     * @zh@ - Chinese
--
--
  , pageSize :: Core.Maybe Core.Natural
    -- ^ The maximum number of items to return with this call.
  , pageToken :: Core.Maybe Types.PageToken
    -- ^ The page token for the next set of results. To retrieve the first set of results, use null.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeRecord' value with any optional fields omitted.
mkDescribeRecord
    :: Types.Id -- ^ 'id'
    -> DescribeRecord
mkDescribeRecord id
  = DescribeRecord'{id, acceptLanguage = Core.Nothing,
                    pageSize = Core.Nothing, pageToken = Core.Nothing}

-- | The record identifier of the provisioned product. This identifier is returned by the request operation.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drId :: Lens.Lens' DescribeRecord Types.Id
drId = Lens.field @"id"
{-# INLINEABLE drId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The language code.
--
--
--     * @en@ - English (default)
--
--
--     * @jp@ - Japanese
--
--
--     * @zh@ - Chinese
--
--
--
-- /Note:/ Consider using 'acceptLanguage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drAcceptLanguage :: Lens.Lens' DescribeRecord (Core.Maybe Types.AcceptLanguage)
drAcceptLanguage = Lens.field @"acceptLanguage"
{-# INLINEABLE drAcceptLanguage #-}
{-# DEPRECATED acceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead"  #-}

-- | The maximum number of items to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drPageSize :: Lens.Lens' DescribeRecord (Core.Maybe Core.Natural)
drPageSize = Lens.field @"pageSize"
{-# INLINEABLE drPageSize #-}
{-# DEPRECATED pageSize "Use generic-lens or generic-optics with 'pageSize' instead"  #-}

-- | The page token for the next set of results. To retrieve the first set of results, use null.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drPageToken :: Lens.Lens' DescribeRecord (Core.Maybe Types.PageToken)
drPageToken = Lens.field @"pageToken"
{-# INLINEABLE drPageToken #-}
{-# DEPRECATED pageToken "Use generic-lens or generic-optics with 'pageToken' instead"  #-}

instance Core.ToQuery DescribeRecord where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeRecord where
        toHeaders DescribeRecord{..}
          = Core.pure
              ("X-Amz-Target", "AWS242ServiceCatalogService.DescribeRecord")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeRecord where
        toJSON DescribeRecord{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Id" Core..= id),
                  ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
                  ("PageSize" Core..=) Core.<$> pageSize,
                  ("PageToken" Core..=) Core.<$> pageToken])

instance Core.AWSRequest DescribeRecord where
        type Rs DescribeRecord = DescribeRecordResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeRecordResponse' Core.<$>
                   (x Core..:? "NextPageToken") Core.<*> x Core..:? "RecordDetail"
                     Core.<*> x Core..:? "RecordOutputs"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeRecordResponse' smart constructor.
data DescribeRecordResponse = DescribeRecordResponse'
  { nextPageToken :: Core.Maybe Types.NextPageToken
    -- ^ The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
  , recordDetail :: Core.Maybe Types.RecordDetail
    -- ^ Information about the product.
  , recordOutputs :: Core.Maybe [Types.RecordOutput]
    -- ^ Information about the product created as the result of a request. For example, the output for a CloudFormation-backed product that creates an S3 bucket would include the S3 bucket URL.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeRecordResponse' value with any optional fields omitted.
mkDescribeRecordResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeRecordResponse
mkDescribeRecordResponse responseStatus
  = DescribeRecordResponse'{nextPageToken = Core.Nothing,
                            recordDetail = Core.Nothing, recordOutputs = Core.Nothing,
                            responseStatus}

-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsNextPageToken :: Lens.Lens' DescribeRecordResponse (Core.Maybe Types.NextPageToken)
drrrsNextPageToken = Lens.field @"nextPageToken"
{-# INLINEABLE drrrsNextPageToken #-}
{-# DEPRECATED nextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead"  #-}

-- | Information about the product.
--
-- /Note:/ Consider using 'recordDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsRecordDetail :: Lens.Lens' DescribeRecordResponse (Core.Maybe Types.RecordDetail)
drrrsRecordDetail = Lens.field @"recordDetail"
{-# INLINEABLE drrrsRecordDetail #-}
{-# DEPRECATED recordDetail "Use generic-lens or generic-optics with 'recordDetail' instead"  #-}

-- | Information about the product created as the result of a request. For example, the output for a CloudFormation-backed product that creates an S3 bucket would include the S3 bucket URL.
--
-- /Note:/ Consider using 'recordOutputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsRecordOutputs :: Lens.Lens' DescribeRecordResponse (Core.Maybe [Types.RecordOutput])
drrrsRecordOutputs = Lens.field @"recordOutputs"
{-# INLINEABLE drrrsRecordOutputs #-}
{-# DEPRECATED recordOutputs "Use generic-lens or generic-optics with 'recordOutputs' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsResponseStatus :: Lens.Lens' DescribeRecordResponse Core.Int
drrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
