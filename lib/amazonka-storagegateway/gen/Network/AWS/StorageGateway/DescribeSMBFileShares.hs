{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DescribeSMBFileShares
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a description for one or more Server Message Block (SMB) file shares from a file gateway. This operation is only supported for file gateways.
module Network.AWS.StorageGateway.DescribeSMBFileShares
    (
    -- * Creating a request
      DescribeSMBFileShares (..)
    , mkDescribeSMBFileShares
    -- ** Request lenses
    , dsmbfsFileShareARNList

    -- * Destructuring the response
    , DescribeSMBFileSharesResponse (..)
    , mkDescribeSMBFileSharesResponse
    -- ** Response lenses
    , dsmbfsrrsSMBFileShareInfoList
    , dsmbfsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | DescribeSMBFileSharesInput
--
-- /See:/ 'mkDescribeSMBFileShares' smart constructor.
newtype DescribeSMBFileShares = DescribeSMBFileShares'
  { fileShareARNList :: Core.NonEmpty Types.FileShareARN
    -- ^ An array containing the Amazon Resource Name (ARN) of each file share to be described.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSMBFileShares' value with any optional fields omitted.
mkDescribeSMBFileShares
    :: Core.NonEmpty Types.FileShareARN -- ^ 'fileShareARNList'
    -> DescribeSMBFileShares
mkDescribeSMBFileShares fileShareARNList
  = DescribeSMBFileShares'{fileShareARNList}

-- | An array containing the Amazon Resource Name (ARN) of each file share to be described.
--
-- /Note:/ Consider using 'fileShareARNList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmbfsFileShareARNList :: Lens.Lens' DescribeSMBFileShares (Core.NonEmpty Types.FileShareARN)
dsmbfsFileShareARNList = Lens.field @"fileShareARNList"
{-# INLINEABLE dsmbfsFileShareARNList #-}
{-# DEPRECATED fileShareARNList "Use generic-lens or generic-optics with 'fileShareARNList' instead"  #-}

instance Core.ToQuery DescribeSMBFileShares where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeSMBFileShares where
        toHeaders DescribeSMBFileShares{..}
          = Core.pure
              ("X-Amz-Target", "StorageGateway_20130630.DescribeSMBFileShares")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeSMBFileShares where
        toJSON DescribeSMBFileShares{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("FileShareARNList" Core..= fileShareARNList)])

instance Core.AWSRequest DescribeSMBFileShares where
        type Rs DescribeSMBFileShares = DescribeSMBFileSharesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeSMBFileSharesResponse' Core.<$>
                   (x Core..:? "SMBFileShareInfoList") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | DescribeSMBFileSharesOutput
--
-- /See:/ 'mkDescribeSMBFileSharesResponse' smart constructor.
data DescribeSMBFileSharesResponse = DescribeSMBFileSharesResponse'
  { sMBFileShareInfoList :: Core.Maybe [Types.SMBFileShareInfo]
    -- ^ An array containing a description for each requested file share.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSMBFileSharesResponse' value with any optional fields omitted.
mkDescribeSMBFileSharesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeSMBFileSharesResponse
mkDescribeSMBFileSharesResponse responseStatus
  = DescribeSMBFileSharesResponse'{sMBFileShareInfoList =
                                     Core.Nothing,
                                   responseStatus}

-- | An array containing a description for each requested file share.
--
-- /Note:/ Consider using 'sMBFileShareInfoList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmbfsrrsSMBFileShareInfoList :: Lens.Lens' DescribeSMBFileSharesResponse (Core.Maybe [Types.SMBFileShareInfo])
dsmbfsrrsSMBFileShareInfoList = Lens.field @"sMBFileShareInfoList"
{-# INLINEABLE dsmbfsrrsSMBFileShareInfoList #-}
{-# DEPRECATED sMBFileShareInfoList "Use generic-lens or generic-optics with 'sMBFileShareInfoList' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmbfsrrsResponseStatus :: Lens.Lens' DescribeSMBFileSharesResponse Core.Int
dsmbfsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsmbfsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
