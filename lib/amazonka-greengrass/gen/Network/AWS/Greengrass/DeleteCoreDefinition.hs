{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.DeleteCoreDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a core definition.
module Network.AWS.Greengrass.DeleteCoreDefinition
  ( -- * Creating a request
    DeleteCoreDefinition (..),
    mkDeleteCoreDefinition,

    -- ** Request lenses
    dcdCoreDefinitionId,

    -- * Destructuring the response
    DeleteCoreDefinitionResponse (..),
    mkDeleteCoreDefinitionResponse,

    -- ** Response lenses
    drsResponseStatus,
  )
where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteCoreDefinition' smart constructor.
newtype DeleteCoreDefinition = DeleteCoreDefinition'
  { -- | The ID of the core definition.
    coreDefinitionId :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCoreDefinition' value with any optional fields omitted.
mkDeleteCoreDefinition ::
  -- | 'coreDefinitionId'
  Core.Text ->
  DeleteCoreDefinition
mkDeleteCoreDefinition coreDefinitionId =
  DeleteCoreDefinition' {coreDefinitionId}

-- | The ID of the core definition.
--
-- /Note:/ Consider using 'coreDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcdCoreDefinitionId :: Lens.Lens' DeleteCoreDefinition Core.Text
dcdCoreDefinitionId = Lens.field @"coreDefinitionId"
{-# DEPRECATED dcdCoreDefinitionId "Use generic-lens or generic-optics with 'coreDefinitionId' instead." #-}

instance Core.AWSRequest DeleteCoreDefinition where
  type Rs DeleteCoreDefinition = DeleteCoreDefinitionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath
            ( "/greengrass/definition/cores/"
                Core.<> (Core.toText coreDefinitionId)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteCoreDefinitionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteCoreDefinitionResponse' smart constructor.
newtype DeleteCoreDefinitionResponse = DeleteCoreDefinitionResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCoreDefinitionResponse' value with any optional fields omitted.
mkDeleteCoreDefinitionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteCoreDefinitionResponse
mkDeleteCoreDefinitionResponse responseStatus =
  DeleteCoreDefinitionResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteCoreDefinitionResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
