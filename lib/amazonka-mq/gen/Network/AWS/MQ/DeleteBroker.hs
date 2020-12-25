{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.DeleteBroker
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a broker. Note: This API is asynchronous.
module Network.AWS.MQ.DeleteBroker
  ( -- * Creating a request
    DeleteBroker (..),
    mkDeleteBroker,

    -- ** Request lenses
    dbBrokerId,

    -- * Destructuring the response
    DeleteBrokerResponse (..),
    mkDeleteBrokerResponse,

    -- ** Response lenses
    drsBrokerId,
    drsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MQ.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteBroker' smart constructor.
newtype DeleteBroker = DeleteBroker'
  { -- | The unique ID that Amazon MQ generates for the broker.
    brokerId :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBroker' value with any optional fields omitted.
mkDeleteBroker ::
  -- | 'brokerId'
  Core.Text ->
  DeleteBroker
mkDeleteBroker brokerId = DeleteBroker' {brokerId}

-- | The unique ID that Amazon MQ generates for the broker.
--
-- /Note:/ Consider using 'brokerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbBrokerId :: Lens.Lens' DeleteBroker Core.Text
dbBrokerId = Lens.field @"brokerId"
{-# DEPRECATED dbBrokerId "Use generic-lens or generic-optics with 'brokerId' instead." #-}

instance Core.AWSRequest DeleteBroker where
  type Rs DeleteBroker = DeleteBrokerResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath ("/v1/brokers/" Core.<> (Core.toText brokerId)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteBrokerResponse'
            Core.<$> (x Core..:? "brokerId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteBrokerResponse' smart constructor.
data DeleteBrokerResponse = DeleteBrokerResponse'
  { -- | The unique ID that Amazon MQ generates for the broker.
    brokerId :: Core.Maybe Core.Text,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBrokerResponse' value with any optional fields omitted.
mkDeleteBrokerResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteBrokerResponse
mkDeleteBrokerResponse responseStatus =
  DeleteBrokerResponse' {brokerId = Core.Nothing, responseStatus}

-- | The unique ID that Amazon MQ generates for the broker.
--
-- /Note:/ Consider using 'brokerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsBrokerId :: Lens.Lens' DeleteBrokerResponse (Core.Maybe Core.Text)
drsBrokerId = Lens.field @"brokerId"
{-# DEPRECATED drsBrokerId "Use generic-lens or generic-optics with 'brokerId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteBrokerResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
