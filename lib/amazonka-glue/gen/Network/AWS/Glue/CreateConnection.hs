{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.CreateConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a connection definition in the Data Catalog.
module Network.AWS.Glue.CreateConnection
  ( -- * Creating a request
    CreateConnection (..),
    mkCreateConnection,

    -- ** Request lenses
    ccConnectionInput,
    ccCatalogId,

    -- * Destructuring the response
    CreateConnectionResponse (..),
    mkCreateConnectionResponse,

    -- ** Response lenses
    crsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateConnection' smart constructor.
data CreateConnection = CreateConnection'
  { -- | A @ConnectionInput@ object defining the connection to create.
    connectionInput :: Types.ConnectionInput,
    -- | The ID of the Data Catalog in which to create the connection. If none is provided, the AWS account ID is used by default.
    catalogId :: Core.Maybe Types.CatalogId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateConnection' value with any optional fields omitted.
mkCreateConnection ::
  -- | 'connectionInput'
  Types.ConnectionInput ->
  CreateConnection
mkCreateConnection connectionInput =
  CreateConnection' {connectionInput, catalogId = Core.Nothing}

-- | A @ConnectionInput@ object defining the connection to create.
--
-- /Note:/ Consider using 'connectionInput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccConnectionInput :: Lens.Lens' CreateConnection Types.ConnectionInput
ccConnectionInput = Lens.field @"connectionInput"
{-# DEPRECATED ccConnectionInput "Use generic-lens or generic-optics with 'connectionInput' instead." #-}

-- | The ID of the Data Catalog in which to create the connection. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccCatalogId :: Lens.Lens' CreateConnection (Core.Maybe Types.CatalogId)
ccCatalogId = Lens.field @"catalogId"
{-# DEPRECATED ccCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

instance Core.FromJSON CreateConnection where
  toJSON CreateConnection {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ConnectionInput" Core..= connectionInput),
            ("CatalogId" Core..=) Core.<$> catalogId
          ]
      )

instance Core.AWSRequest CreateConnection where
  type Rs CreateConnection = CreateConnectionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.CreateConnection")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateConnectionResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateConnectionResponse' smart constructor.
newtype CreateConnectionResponse = CreateConnectionResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateConnectionResponse' value with any optional fields omitted.
mkCreateConnectionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateConnectionResponse
mkCreateConnectionResponse responseStatus =
  CreateConnectionResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsResponseStatus :: Lens.Lens' CreateConnectionResponse Core.Int
crsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED crsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
