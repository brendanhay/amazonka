{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.UpdateCoreDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a core definition.
module Network.AWS.Greengrass.UpdateCoreDefinition
  ( -- * Creating a request
    UpdateCoreDefinition (..),
    mkUpdateCoreDefinition,

    -- ** Request lenses
    ucdCoreDefinitionId,
    ucdName,

    -- * Destructuring the response
    UpdateCoreDefinitionResponse (..),
    mkUpdateCoreDefinitionResponse,

    -- ** Response lenses
    ursResponseStatus,
  )
where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateCoreDefinition' smart constructor.
data UpdateCoreDefinition = UpdateCoreDefinition'
  { -- | The ID of the core definition.
    coreDefinitionId :: Core.Text,
    -- | The name of the definition.
    name :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateCoreDefinition' value with any optional fields omitted.
mkUpdateCoreDefinition ::
  -- | 'coreDefinitionId'
  Core.Text ->
  UpdateCoreDefinition
mkUpdateCoreDefinition coreDefinitionId =
  UpdateCoreDefinition' {coreDefinitionId, name = Core.Nothing}

-- | The ID of the core definition.
--
-- /Note:/ Consider using 'coreDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucdCoreDefinitionId :: Lens.Lens' UpdateCoreDefinition Core.Text
ucdCoreDefinitionId = Lens.field @"coreDefinitionId"
{-# DEPRECATED ucdCoreDefinitionId "Use generic-lens or generic-optics with 'coreDefinitionId' instead." #-}

-- | The name of the definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucdName :: Lens.Lens' UpdateCoreDefinition (Core.Maybe Core.Text)
ucdName = Lens.field @"name"
{-# DEPRECATED ucdName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON UpdateCoreDefinition where
  toJSON UpdateCoreDefinition {..} =
    Core.object (Core.catMaybes [("Name" Core..=) Core.<$> name])

instance Core.AWSRequest UpdateCoreDefinition where
  type Rs UpdateCoreDefinition = UpdateCoreDefinitionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ( "/greengrass/definition/cores/"
                Core.<> (Core.toText coreDefinitionId)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateCoreDefinitionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateCoreDefinitionResponse' smart constructor.
newtype UpdateCoreDefinitionResponse = UpdateCoreDefinitionResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateCoreDefinitionResponse' value with any optional fields omitted.
mkUpdateCoreDefinitionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateCoreDefinitionResponse
mkUpdateCoreDefinitionResponse responseStatus =
  UpdateCoreDefinitionResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursResponseStatus :: Lens.Lens' UpdateCoreDefinitionResponse Core.Int
ursResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
