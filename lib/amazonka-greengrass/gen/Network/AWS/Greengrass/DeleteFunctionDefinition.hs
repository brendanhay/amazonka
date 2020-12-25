{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.DeleteFunctionDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Lambda function definition.
module Network.AWS.Greengrass.DeleteFunctionDefinition
  ( -- * Creating a request
    DeleteFunctionDefinition (..),
    mkDeleteFunctionDefinition,

    -- ** Request lenses
    dfdFunctionDefinitionId,

    -- * Destructuring the response
    DeleteFunctionDefinitionResponse (..),
    mkDeleteFunctionDefinitionResponse,

    -- ** Response lenses
    dfdrrsResponseStatus,
  )
where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteFunctionDefinition' smart constructor.
newtype DeleteFunctionDefinition = DeleteFunctionDefinition'
  { -- | The ID of the Lambda function definition.
    functionDefinitionId :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteFunctionDefinition' value with any optional fields omitted.
mkDeleteFunctionDefinition ::
  -- | 'functionDefinitionId'
  Core.Text ->
  DeleteFunctionDefinition
mkDeleteFunctionDefinition functionDefinitionId =
  DeleteFunctionDefinition' {functionDefinitionId}

-- | The ID of the Lambda function definition.
--
-- /Note:/ Consider using 'functionDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfdFunctionDefinitionId :: Lens.Lens' DeleteFunctionDefinition Core.Text
dfdFunctionDefinitionId = Lens.field @"functionDefinitionId"
{-# DEPRECATED dfdFunctionDefinitionId "Use generic-lens or generic-optics with 'functionDefinitionId' instead." #-}

instance Core.AWSRequest DeleteFunctionDefinition where
  type Rs DeleteFunctionDefinition = DeleteFunctionDefinitionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath
            ( "/greengrass/definition/functions/"
                Core.<> (Core.toText functionDefinitionId)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteFunctionDefinitionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteFunctionDefinitionResponse' smart constructor.
newtype DeleteFunctionDefinitionResponse = DeleteFunctionDefinitionResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteFunctionDefinitionResponse' value with any optional fields omitted.
mkDeleteFunctionDefinitionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteFunctionDefinitionResponse
mkDeleteFunctionDefinitionResponse responseStatus =
  DeleteFunctionDefinitionResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfdrrsResponseStatus :: Lens.Lens' DeleteFunctionDefinitionResponse Core.Int
dfdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dfdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
