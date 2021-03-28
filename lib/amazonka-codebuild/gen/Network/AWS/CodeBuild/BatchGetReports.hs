{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.BatchGetReports
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of reports. 
module Network.AWS.CodeBuild.BatchGetReports
    (
    -- * Creating a request
      BatchGetReports (..)
    , mkBatchGetReports
    -- ** Request lenses
    , bgrReportArns

    -- * Destructuring the response
    , BatchGetReportsResponse (..)
    , mkBatchGetReportsResponse
    -- ** Response lenses
    , bgrrrsReports
    , bgrrrsReportsNotFound
    , bgrrrsResponseStatus
    ) where

import qualified Network.AWS.CodeBuild.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkBatchGetReports' smart constructor.
newtype BatchGetReports = BatchGetReports'
  { reportArns :: Core.NonEmpty Types.NonEmptyString
    -- ^ An array of ARNs that identify the @Report@ objects to return. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'BatchGetReports' value with any optional fields omitted.
mkBatchGetReports
    :: Core.NonEmpty Types.NonEmptyString -- ^ 'reportArns'
    -> BatchGetReports
mkBatchGetReports reportArns = BatchGetReports'{reportArns}

-- | An array of ARNs that identify the @Report@ objects to return. 
--
-- /Note:/ Consider using 'reportArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgrReportArns :: Lens.Lens' BatchGetReports (Core.NonEmpty Types.NonEmptyString)
bgrReportArns = Lens.field @"reportArns"
{-# INLINEABLE bgrReportArns #-}
{-# DEPRECATED reportArns "Use generic-lens or generic-optics with 'reportArns' instead"  #-}

instance Core.ToQuery BatchGetReports where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders BatchGetReports where
        toHeaders BatchGetReports{..}
          = Core.pure ("X-Amz-Target", "CodeBuild_20161006.BatchGetReports")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON BatchGetReports where
        toJSON BatchGetReports{..}
          = Core.object
              (Core.catMaybes [Core.Just ("reportArns" Core..= reportArns)])

instance Core.AWSRequest BatchGetReports where
        type Rs BatchGetReports = BatchGetReportsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 BatchGetReportsResponse' Core.<$>
                   (x Core..:? "reports") Core.<*> x Core..:? "reportsNotFound"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkBatchGetReportsResponse' smart constructor.
data BatchGetReportsResponse = BatchGetReportsResponse'
  { reports :: Core.Maybe (Core.NonEmpty Types.Report)
    -- ^ The array of @Report@ objects returned by @BatchGetReports@ . 
  , reportsNotFound :: Core.Maybe (Core.NonEmpty Types.NonEmptyString)
    -- ^ An array of ARNs passed to @BatchGetReportGroups@ that are not associated with a @Report@ . 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'BatchGetReportsResponse' value with any optional fields omitted.
mkBatchGetReportsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> BatchGetReportsResponse
mkBatchGetReportsResponse responseStatus
  = BatchGetReportsResponse'{reports = Core.Nothing,
                             reportsNotFound = Core.Nothing, responseStatus}

-- | The array of @Report@ objects returned by @BatchGetReports@ . 
--
-- /Note:/ Consider using 'reports' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgrrrsReports :: Lens.Lens' BatchGetReportsResponse (Core.Maybe (Core.NonEmpty Types.Report))
bgrrrsReports = Lens.field @"reports"
{-# INLINEABLE bgrrrsReports #-}
{-# DEPRECATED reports "Use generic-lens or generic-optics with 'reports' instead"  #-}

-- | An array of ARNs passed to @BatchGetReportGroups@ that are not associated with a @Report@ . 
--
-- /Note:/ Consider using 'reportsNotFound' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgrrrsReportsNotFound :: Lens.Lens' BatchGetReportsResponse (Core.Maybe (Core.NonEmpty Types.NonEmptyString))
bgrrrsReportsNotFound = Lens.field @"reportsNotFound"
{-# INLINEABLE bgrrrsReportsNotFound #-}
{-# DEPRECATED reportsNotFound "Use generic-lens or generic-optics with 'reportsNotFound' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgrrrsResponseStatus :: Lens.Lens' BatchGetReportsResponse Core.Int
bgrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE bgrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
