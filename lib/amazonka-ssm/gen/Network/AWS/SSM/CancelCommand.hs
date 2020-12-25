{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.CancelCommand
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attempts to cancel the command specified by the Command ID. There is no guarantee that the command will be terminated and the underlying process stopped.
module Network.AWS.SSM.CancelCommand
  ( -- * Creating a request
    CancelCommand (..),
    mkCancelCommand,

    -- ** Request lenses
    ccCommandId,
    ccInstanceIds,

    -- * Destructuring the response
    CancelCommandResponse (..),
    mkCancelCommandResponse,

    -- ** Response lenses
    ccrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- |
--
-- /See:/ 'mkCancelCommand' smart constructor.
data CancelCommand = CancelCommand'
  { -- | The ID of the command you want to cancel.
    commandId :: Types.CommandId,
    -- | (Optional) A list of instance IDs on which you want to cancel the command. If not provided, the command is canceled on every instance on which it was requested.
    instanceIds :: Core.Maybe [Types.InstanceId]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CancelCommand' value with any optional fields omitted.
mkCancelCommand ::
  -- | 'commandId'
  Types.CommandId ->
  CancelCommand
mkCancelCommand commandId =
  CancelCommand' {commandId, instanceIds = Core.Nothing}

-- | The ID of the command you want to cancel.
--
-- /Note:/ Consider using 'commandId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccCommandId :: Lens.Lens' CancelCommand Types.CommandId
ccCommandId = Lens.field @"commandId"
{-# DEPRECATED ccCommandId "Use generic-lens or generic-optics with 'commandId' instead." #-}

-- | (Optional) A list of instance IDs on which you want to cancel the command. If not provided, the command is canceled on every instance on which it was requested.
--
-- /Note:/ Consider using 'instanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccInstanceIds :: Lens.Lens' CancelCommand (Core.Maybe [Types.InstanceId])
ccInstanceIds = Lens.field @"instanceIds"
{-# DEPRECATED ccInstanceIds "Use generic-lens or generic-optics with 'instanceIds' instead." #-}

instance Core.FromJSON CancelCommand where
  toJSON CancelCommand {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("CommandId" Core..= commandId),
            ("InstanceIds" Core..=) Core.<$> instanceIds
          ]
      )

instance Core.AWSRequest CancelCommand where
  type Rs CancelCommand = CancelCommandResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonSSM.CancelCommand")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          CancelCommandResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | Whether or not the command was successfully canceled. There is no guarantee that a request can be canceled.
--
-- /See:/ 'mkCancelCommandResponse' smart constructor.
newtype CancelCommandResponse = CancelCommandResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CancelCommandResponse' value with any optional fields omitted.
mkCancelCommandResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CancelCommandResponse
mkCancelCommandResponse responseStatus =
  CancelCommandResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsResponseStatus :: Lens.Lens' CancelCommandResponse Core.Int
ccrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ccrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
