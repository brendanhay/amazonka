{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.RebootRelationalDatabase
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restarts a specific database in Amazon Lightsail.
--
-- The @reboot relational database@ operation supports tag-based access control via resource tags applied to the resource identified by relationalDatabaseName. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.RebootRelationalDatabase
  ( -- * Creating a request
    RebootRelationalDatabase (..),
    mkRebootRelationalDatabase,

    -- ** Request lenses
    rrdRelationalDatabaseName,

    -- * Destructuring the response
    RebootRelationalDatabaseResponse (..),
    mkRebootRelationalDatabaseResponse,

    -- ** Response lenses
    rrdrrsOperations,
    rrdrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRebootRelationalDatabase' smart constructor.
newtype RebootRelationalDatabase = RebootRelationalDatabase'
  { -- | The name of your database to reboot.
    relationalDatabaseName :: Types.ResourceName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RebootRelationalDatabase' value with any optional fields omitted.
mkRebootRelationalDatabase ::
  -- | 'relationalDatabaseName'
  Types.ResourceName ->
  RebootRelationalDatabase
mkRebootRelationalDatabase relationalDatabaseName =
  RebootRelationalDatabase' {relationalDatabaseName}

-- | The name of your database to reboot.
--
-- /Note:/ Consider using 'relationalDatabaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrdRelationalDatabaseName :: Lens.Lens' RebootRelationalDatabase Types.ResourceName
rrdRelationalDatabaseName = Lens.field @"relationalDatabaseName"
{-# DEPRECATED rrdRelationalDatabaseName "Use generic-lens or generic-optics with 'relationalDatabaseName' instead." #-}

instance Core.FromJSON RebootRelationalDatabase where
  toJSON RebootRelationalDatabase {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("relationalDatabaseName" Core..= relationalDatabaseName)
          ]
      )

instance Core.AWSRequest RebootRelationalDatabase where
  type Rs RebootRelationalDatabase = RebootRelationalDatabaseResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Lightsail_20161128.RebootRelationalDatabase")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          RebootRelationalDatabaseResponse'
            Core.<$> (x Core..:? "operations") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRebootRelationalDatabaseResponse' smart constructor.
data RebootRelationalDatabaseResponse = RebootRelationalDatabaseResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operations :: Core.Maybe [Types.Operation],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'RebootRelationalDatabaseResponse' value with any optional fields omitted.
mkRebootRelationalDatabaseResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RebootRelationalDatabaseResponse
mkRebootRelationalDatabaseResponse responseStatus =
  RebootRelationalDatabaseResponse'
    { operations = Core.Nothing,
      responseStatus
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrdrrsOperations :: Lens.Lens' RebootRelationalDatabaseResponse (Core.Maybe [Types.Operation])
rrdrrsOperations = Lens.field @"operations"
{-# DEPRECATED rrdrrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrdrrsResponseStatus :: Lens.Lens' RebootRelationalDatabaseResponse Core.Int
rrdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rrdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
