{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.RetrieveTapeRecoveryPoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the recovery point for the specified virtual tape. This operation is only supported in the tape gateway type.
--
-- A recovery point is a point in time view of a virtual tape at which all the data on the tape is consistent. If your gateway crashes, virtual tapes that have recovery points can be recovered to a new gateway.
module Network.AWS.StorageGateway.RetrieveTapeRecoveryPoint
  ( -- * Creating a request
    RetrieveTapeRecoveryPoint (..),
    mkRetrieveTapeRecoveryPoint,

    -- ** Request lenses
    rtrpTapeARN,
    rtrpGatewayARN,

    -- * Destructuring the response
    RetrieveTapeRecoveryPointResponse (..),
    mkRetrieveTapeRecoveryPointResponse,

    -- ** Response lenses
    rtrprrsTapeARN,
    rtrprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | RetrieveTapeRecoveryPointInput
--
-- /See:/ 'mkRetrieveTapeRecoveryPoint' smart constructor.
data RetrieveTapeRecoveryPoint = RetrieveTapeRecoveryPoint'
  { -- | The Amazon Resource Name (ARN) of the virtual tape for which you want to retrieve the recovery point.
    tapeARN :: Types.TapeARN,
    gatewayARN :: Types.GatewayARN
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RetrieveTapeRecoveryPoint' value with any optional fields omitted.
mkRetrieveTapeRecoveryPoint ::
  -- | 'tapeARN'
  Types.TapeARN ->
  -- | 'gatewayARN'
  Types.GatewayARN ->
  RetrieveTapeRecoveryPoint
mkRetrieveTapeRecoveryPoint tapeARN gatewayARN =
  RetrieveTapeRecoveryPoint' {tapeARN, gatewayARN}

-- | The Amazon Resource Name (ARN) of the virtual tape for which you want to retrieve the recovery point.
--
-- /Note:/ Consider using 'tapeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrpTapeARN :: Lens.Lens' RetrieveTapeRecoveryPoint Types.TapeARN
rtrpTapeARN = Lens.field @"tapeARN"
{-# DEPRECATED rtrpTapeARN "Use generic-lens or generic-optics with 'tapeARN' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrpGatewayARN :: Lens.Lens' RetrieveTapeRecoveryPoint Types.GatewayARN
rtrpGatewayARN = Lens.field @"gatewayARN"
{-# DEPRECATED rtrpGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

instance Core.FromJSON RetrieveTapeRecoveryPoint where
  toJSON RetrieveTapeRecoveryPoint {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TapeARN" Core..= tapeARN),
            Core.Just ("GatewayARN" Core..= gatewayARN)
          ]
      )

instance Core.AWSRequest RetrieveTapeRecoveryPoint where
  type
    Rs RetrieveTapeRecoveryPoint =
      RetrieveTapeRecoveryPointResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "StorageGateway_20130630.RetrieveTapeRecoveryPoint"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          RetrieveTapeRecoveryPointResponse'
            Core.<$> (x Core..:? "TapeARN") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | RetrieveTapeRecoveryPointOutput
--
-- /See:/ 'mkRetrieveTapeRecoveryPointResponse' smart constructor.
data RetrieveTapeRecoveryPointResponse = RetrieveTapeRecoveryPointResponse'
  { -- | The Amazon Resource Name (ARN) of the virtual tape for which the recovery point was retrieved.
    tapeARN :: Core.Maybe Types.TapeARN,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RetrieveTapeRecoveryPointResponse' value with any optional fields omitted.
mkRetrieveTapeRecoveryPointResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RetrieveTapeRecoveryPointResponse
mkRetrieveTapeRecoveryPointResponse responseStatus =
  RetrieveTapeRecoveryPointResponse'
    { tapeARN = Core.Nothing,
      responseStatus
    }

-- | The Amazon Resource Name (ARN) of the virtual tape for which the recovery point was retrieved.
--
-- /Note:/ Consider using 'tapeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrprrsTapeARN :: Lens.Lens' RetrieveTapeRecoveryPointResponse (Core.Maybe Types.TapeARN)
rtrprrsTapeARN = Lens.field @"tapeARN"
{-# DEPRECATED rtrprrsTapeARN "Use generic-lens or generic-optics with 'tapeARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrprrsResponseStatus :: Lens.Lens' RetrieveTapeRecoveryPointResponse Core.Int
rtrprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rtrprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
