{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.UpdateConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a connection definition in the Data Catalog.
module Network.AWS.Glue.UpdateConnection
  ( -- * Creating a request
    UpdateConnection (..),
    mkUpdateConnection,

    -- ** Request lenses
    ucName,
    ucConnectionInput,
    ucCatalogId,

    -- * Destructuring the response
    UpdateConnectionResponse (..),
    mkUpdateConnectionResponse,

    -- ** Response lenses
    ucrrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateConnection' smart constructor.
data UpdateConnection = UpdateConnection'
  { -- | The name of the connection definition to update.
    name :: Types.NameString,
    -- | A @ConnectionInput@ object that redefines the connection in question.
    connectionInput :: Types.ConnectionInput,
    -- | The ID of the Data Catalog in which the connection resides. If none is provided, the AWS account ID is used by default.
    catalogId :: Core.Maybe Types.CatalogIdString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateConnection' value with any optional fields omitted.
mkUpdateConnection ::
  -- | 'name'
  Types.NameString ->
  -- | 'connectionInput'
  Types.ConnectionInput ->
  UpdateConnection
mkUpdateConnection name connectionInput =
  UpdateConnection'
    { name,
      connectionInput,
      catalogId = Core.Nothing
    }

-- | The name of the connection definition to update.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucName :: Lens.Lens' UpdateConnection Types.NameString
ucName = Lens.field @"name"
{-# DEPRECATED ucName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A @ConnectionInput@ object that redefines the connection in question.
--
-- /Note:/ Consider using 'connectionInput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucConnectionInput :: Lens.Lens' UpdateConnection Types.ConnectionInput
ucConnectionInput = Lens.field @"connectionInput"
{-# DEPRECATED ucConnectionInput "Use generic-lens or generic-optics with 'connectionInput' instead." #-}

-- | The ID of the Data Catalog in which the connection resides. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucCatalogId :: Lens.Lens' UpdateConnection (Core.Maybe Types.CatalogIdString)
ucCatalogId = Lens.field @"catalogId"
{-# DEPRECATED ucCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

instance Core.FromJSON UpdateConnection where
  toJSON UpdateConnection {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("ConnectionInput" Core..= connectionInput),
            ("CatalogId" Core..=) Core.<$> catalogId
          ]
      )

instance Core.AWSRequest UpdateConnection where
  type Rs UpdateConnection = UpdateConnectionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.UpdateConnection")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateConnectionResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateConnectionResponse' smart constructor.
newtype UpdateConnectionResponse = UpdateConnectionResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateConnectionResponse' value with any optional fields omitted.
mkUpdateConnectionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateConnectionResponse
mkUpdateConnectionResponse responseStatus =
  UpdateConnectionResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrrsResponseStatus :: Lens.Lens' UpdateConnectionResponse Core.Int
ucrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ucrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
