{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.UpdateFunctionDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a Lambda function definition.
module Network.AWS.Greengrass.UpdateFunctionDefinition
  ( -- * Creating a request
    UpdateFunctionDefinition (..),
    mkUpdateFunctionDefinition,

    -- ** Request lenses
    ufdFunctionDefinitionId,
    ufdName,

    -- * Destructuring the response
    UpdateFunctionDefinitionResponse (..),
    mkUpdateFunctionDefinitionResponse,

    -- ** Response lenses
    ufdrrsResponseStatus,
  )
where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateFunctionDefinition' smart constructor.
data UpdateFunctionDefinition = UpdateFunctionDefinition'
  { -- | The ID of the Lambda function definition.
    functionDefinitionId :: Core.Text,
    -- | The name of the definition.
    name :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateFunctionDefinition' value with any optional fields omitted.
mkUpdateFunctionDefinition ::
  -- | 'functionDefinitionId'
  Core.Text ->
  UpdateFunctionDefinition
mkUpdateFunctionDefinition functionDefinitionId =
  UpdateFunctionDefinition'
    { functionDefinitionId,
      name = Core.Nothing
    }

-- | The ID of the Lambda function definition.
--
-- /Note:/ Consider using 'functionDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufdFunctionDefinitionId :: Lens.Lens' UpdateFunctionDefinition Core.Text
ufdFunctionDefinitionId = Lens.field @"functionDefinitionId"
{-# DEPRECATED ufdFunctionDefinitionId "Use generic-lens or generic-optics with 'functionDefinitionId' instead." #-}

-- | The name of the definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufdName :: Lens.Lens' UpdateFunctionDefinition (Core.Maybe Core.Text)
ufdName = Lens.field @"name"
{-# DEPRECATED ufdName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON UpdateFunctionDefinition where
  toJSON UpdateFunctionDefinition {..} =
    Core.object (Core.catMaybes [("Name" Core..=) Core.<$> name])

instance Core.AWSRequest UpdateFunctionDefinition where
  type Rs UpdateFunctionDefinition = UpdateFunctionDefinitionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ( "/greengrass/definition/functions/"
                Core.<> (Core.toText functionDefinitionId)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateFunctionDefinitionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateFunctionDefinitionResponse' smart constructor.
newtype UpdateFunctionDefinitionResponse = UpdateFunctionDefinitionResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateFunctionDefinitionResponse' value with any optional fields omitted.
mkUpdateFunctionDefinitionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateFunctionDefinitionResponse
mkUpdateFunctionDefinitionResponse responseStatus =
  UpdateFunctionDefinitionResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufdrrsResponseStatus :: Lens.Lens' UpdateFunctionDefinitionResponse Core.Int
ufdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ufdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
