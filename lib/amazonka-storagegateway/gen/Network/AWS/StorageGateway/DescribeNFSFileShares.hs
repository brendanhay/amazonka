{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DescribeNFSFileShares
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a description for one or more Network File System (NFS) file shares from a file gateway. This operation is only supported for file gateways.
module Network.AWS.StorageGateway.DescribeNFSFileShares
    (
    -- * Creating a request
      DescribeNFSFileShares (..)
    , mkDescribeNFSFileShares
    -- ** Request lenses
    , dnfsfsFileShareARNList

    -- * Destructuring the response
    , DescribeNFSFileSharesResponse (..)
    , mkDescribeNFSFileSharesResponse
    -- ** Response lenses
    , dnfsfsrrsNFSFileShareInfoList
    , dnfsfsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | DescribeNFSFileSharesInput
--
-- /See:/ 'mkDescribeNFSFileShares' smart constructor.
newtype DescribeNFSFileShares = DescribeNFSFileShares'
  { fileShareARNList :: Core.NonEmpty Types.FileShareARN
    -- ^ An array containing the Amazon Resource Name (ARN) of each file share to be described.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeNFSFileShares' value with any optional fields omitted.
mkDescribeNFSFileShares
    :: Core.NonEmpty Types.FileShareARN -- ^ 'fileShareARNList'
    -> DescribeNFSFileShares
mkDescribeNFSFileShares fileShareARNList
  = DescribeNFSFileShares'{fileShareARNList}

-- | An array containing the Amazon Resource Name (ARN) of each file share to be described.
--
-- /Note:/ Consider using 'fileShareARNList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnfsfsFileShareARNList :: Lens.Lens' DescribeNFSFileShares (Core.NonEmpty Types.FileShareARN)
dnfsfsFileShareARNList = Lens.field @"fileShareARNList"
{-# INLINEABLE dnfsfsFileShareARNList #-}
{-# DEPRECATED fileShareARNList "Use generic-lens or generic-optics with 'fileShareARNList' instead"  #-}

instance Core.ToQuery DescribeNFSFileShares where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeNFSFileShares where
        toHeaders DescribeNFSFileShares{..}
          = Core.pure
              ("X-Amz-Target", "StorageGateway_20130630.DescribeNFSFileShares")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeNFSFileShares where
        toJSON DescribeNFSFileShares{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("FileShareARNList" Core..= fileShareARNList)])

instance Core.AWSRequest DescribeNFSFileShares where
        type Rs DescribeNFSFileShares = DescribeNFSFileSharesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeNFSFileSharesResponse' Core.<$>
                   (x Core..:? "NFSFileShareInfoList") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | DescribeNFSFileSharesOutput
--
-- /See:/ 'mkDescribeNFSFileSharesResponse' smart constructor.
data DescribeNFSFileSharesResponse = DescribeNFSFileSharesResponse'
  { nFSFileShareInfoList :: Core.Maybe [Types.NFSFileShareInfo]
    -- ^ An array containing a description for each requested file share.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeNFSFileSharesResponse' value with any optional fields omitted.
mkDescribeNFSFileSharesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeNFSFileSharesResponse
mkDescribeNFSFileSharesResponse responseStatus
  = DescribeNFSFileSharesResponse'{nFSFileShareInfoList =
                                     Core.Nothing,
                                   responseStatus}

-- | An array containing a description for each requested file share.
--
-- /Note:/ Consider using 'nFSFileShareInfoList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnfsfsrrsNFSFileShareInfoList :: Lens.Lens' DescribeNFSFileSharesResponse (Core.Maybe [Types.NFSFileShareInfo])
dnfsfsrrsNFSFileShareInfoList = Lens.field @"nFSFileShareInfoList"
{-# INLINEABLE dnfsfsrrsNFSFileShareInfoList #-}
{-# DEPRECATED nFSFileShareInfoList "Use generic-lens or generic-optics with 'nFSFileShareInfoList' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnfsfsrrsResponseStatus :: Lens.Lens' DescribeNFSFileSharesResponse Core.Int
dnfsfsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dnfsfsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
