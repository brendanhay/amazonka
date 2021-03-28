{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.BatchGetReportGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of report groups. 
module Network.AWS.CodeBuild.BatchGetReportGroups
    (
    -- * Creating a request
      BatchGetReportGroups (..)
    , mkBatchGetReportGroups
    -- ** Request lenses
    , bgrgReportGroupArns

    -- * Destructuring the response
    , BatchGetReportGroupsResponse (..)
    , mkBatchGetReportGroupsResponse
    -- ** Response lenses
    , bgrgrrsReportGroups
    , bgrgrrsReportGroupsNotFound
    , bgrgrrsResponseStatus
    ) where

import qualified Network.AWS.CodeBuild.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkBatchGetReportGroups' smart constructor.
newtype BatchGetReportGroups = BatchGetReportGroups'
  { reportGroupArns :: Core.NonEmpty Types.NonEmptyString
    -- ^ An array of report group ARNs that identify the report groups to return. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'BatchGetReportGroups' value with any optional fields omitted.
mkBatchGetReportGroups
    :: Core.NonEmpty Types.NonEmptyString -- ^ 'reportGroupArns'
    -> BatchGetReportGroups
mkBatchGetReportGroups reportGroupArns
  = BatchGetReportGroups'{reportGroupArns}

-- | An array of report group ARNs that identify the report groups to return. 
--
-- /Note:/ Consider using 'reportGroupArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgrgReportGroupArns :: Lens.Lens' BatchGetReportGroups (Core.NonEmpty Types.NonEmptyString)
bgrgReportGroupArns = Lens.field @"reportGroupArns"
{-# INLINEABLE bgrgReportGroupArns #-}
{-# DEPRECATED reportGroupArns "Use generic-lens or generic-optics with 'reportGroupArns' instead"  #-}

instance Core.ToQuery BatchGetReportGroups where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders BatchGetReportGroups where
        toHeaders BatchGetReportGroups{..}
          = Core.pure
              ("X-Amz-Target", "CodeBuild_20161006.BatchGetReportGroups")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON BatchGetReportGroups where
        toJSON BatchGetReportGroups{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("reportGroupArns" Core..= reportGroupArns)])

instance Core.AWSRequest BatchGetReportGroups where
        type Rs BatchGetReportGroups = BatchGetReportGroupsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 BatchGetReportGroupsResponse' Core.<$>
                   (x Core..:? "reportGroups") Core.<*>
                     x Core..:? "reportGroupsNotFound"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkBatchGetReportGroupsResponse' smart constructor.
data BatchGetReportGroupsResponse = BatchGetReportGroupsResponse'
  { reportGroups :: Core.Maybe (Core.NonEmpty Types.ReportGroup)
    -- ^ The array of report groups returned by @BatchGetReportGroups@ . 
  , reportGroupsNotFound :: Core.Maybe (Core.NonEmpty Types.NonEmptyString)
    -- ^ An array of ARNs passed to @BatchGetReportGroups@ that are not associated with a @ReportGroup@ . 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'BatchGetReportGroupsResponse' value with any optional fields omitted.
mkBatchGetReportGroupsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> BatchGetReportGroupsResponse
mkBatchGetReportGroupsResponse responseStatus
  = BatchGetReportGroupsResponse'{reportGroups = Core.Nothing,
                                  reportGroupsNotFound = Core.Nothing, responseStatus}

-- | The array of report groups returned by @BatchGetReportGroups@ . 
--
-- /Note:/ Consider using 'reportGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgrgrrsReportGroups :: Lens.Lens' BatchGetReportGroupsResponse (Core.Maybe (Core.NonEmpty Types.ReportGroup))
bgrgrrsReportGroups = Lens.field @"reportGroups"
{-# INLINEABLE bgrgrrsReportGroups #-}
{-# DEPRECATED reportGroups "Use generic-lens or generic-optics with 'reportGroups' instead"  #-}

-- | An array of ARNs passed to @BatchGetReportGroups@ that are not associated with a @ReportGroup@ . 
--
-- /Note:/ Consider using 'reportGroupsNotFound' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgrgrrsReportGroupsNotFound :: Lens.Lens' BatchGetReportGroupsResponse (Core.Maybe (Core.NonEmpty Types.NonEmptyString))
bgrgrrsReportGroupsNotFound = Lens.field @"reportGroupsNotFound"
{-# INLINEABLE bgrgrrsReportGroupsNotFound #-}
{-# DEPRECATED reportGroupsNotFound "Use generic-lens or generic-optics with 'reportGroupsNotFound' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgrgrrsResponseStatus :: Lens.Lens' BatchGetReportGroupsResponse Core.Int
bgrgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE bgrgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
