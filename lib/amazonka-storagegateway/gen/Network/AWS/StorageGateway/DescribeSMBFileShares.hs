{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DescribeSMBFileShares (..),
    mkDescribeSMBFileShares,

    -- ** Request lenses
    dsmbfsFileShareARNList,

    -- * Destructuring the response
    DescribeSMBFileSharesResponse (..),
    mkDescribeSMBFileSharesResponse,

    -- ** Response lenses
    dsmbfsrrsSMBFileShareInfoList,
    dsmbfsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | DescribeSMBFileSharesInput
--
-- /See:/ 'mkDescribeSMBFileShares' smart constructor.
newtype DescribeSMBFileShares = DescribeSMBFileShares'
  { -- | An array containing the Amazon Resource Name (ARN) of each file share to be described.
    fileShareARNList :: Core.NonEmpty Types.FileShareARN
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSMBFileShares' value with any optional fields omitted.
mkDescribeSMBFileShares ::
  -- | 'fileShareARNList'
  Core.NonEmpty Types.FileShareARN ->
  DescribeSMBFileShares
mkDescribeSMBFileShares fileShareARNList =
  DescribeSMBFileShares' {fileShareARNList}

-- | An array containing the Amazon Resource Name (ARN) of each file share to be described.
--
-- /Note:/ Consider using 'fileShareARNList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmbfsFileShareARNList :: Lens.Lens' DescribeSMBFileShares (Core.NonEmpty Types.FileShareARN)
dsmbfsFileShareARNList = Lens.field @"fileShareARNList"
{-# DEPRECATED dsmbfsFileShareARNList "Use generic-lens or generic-optics with 'fileShareARNList' instead." #-}

instance Core.FromJSON DescribeSMBFileShares where
  toJSON DescribeSMBFileShares {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("FileShareARNList" Core..= fileShareARNList)]
      )

instance Core.AWSRequest DescribeSMBFileShares where
  type Rs DescribeSMBFileShares = DescribeSMBFileSharesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "StorageGateway_20130630.DescribeSMBFileShares")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSMBFileSharesResponse'
            Core.<$> (x Core..:? "SMBFileShareInfoList")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | DescribeSMBFileSharesOutput
--
-- /See:/ 'mkDescribeSMBFileSharesResponse' smart constructor.
data DescribeSMBFileSharesResponse = DescribeSMBFileSharesResponse'
  { -- | An array containing a description for each requested file share.
    sMBFileShareInfoList :: Core.Maybe [Types.SMBFileShareInfo],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSMBFileSharesResponse' value with any optional fields omitted.
mkDescribeSMBFileSharesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeSMBFileSharesResponse
mkDescribeSMBFileSharesResponse responseStatus =
  DescribeSMBFileSharesResponse'
    { sMBFileShareInfoList =
        Core.Nothing,
      responseStatus
    }

-- | An array containing a description for each requested file share.
--
-- /Note:/ Consider using 'sMBFileShareInfoList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmbfsrrsSMBFileShareInfoList :: Lens.Lens' DescribeSMBFileSharesResponse (Core.Maybe [Types.SMBFileShareInfo])
dsmbfsrrsSMBFileShareInfoList = Lens.field @"sMBFileShareInfoList"
{-# DEPRECATED dsmbfsrrsSMBFileShareInfoList "Use generic-lens or generic-optics with 'sMBFileShareInfoList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmbfsrrsResponseStatus :: Lens.Lens' DescribeSMBFileSharesResponse Core.Int
dsmbfsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dsmbfsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
