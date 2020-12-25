{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.UpdateRelationalDatabaseParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows the update of one or more parameters of a database in Amazon Lightsail.
--
-- Parameter updates don't cause outages; therefore, their application is not subject to the preferred maintenance window. However, there are two ways in which parameter updates are applied: @dynamic@ or @pending-reboot@ . Parameters marked with a @dynamic@ apply type are applied immediately. Parameters marked with a @pending-reboot@ apply type are applied only after the database is rebooted using the @reboot relational database@ operation.
-- The @update relational database parameters@ operation supports tag-based access control via resource tags applied to the resource identified by relationalDatabaseName. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.UpdateRelationalDatabaseParameters
  ( -- * Creating a request
    UpdateRelationalDatabaseParameters (..),
    mkUpdateRelationalDatabaseParameters,

    -- ** Request lenses
    urdpRelationalDatabaseName,
    urdpParameters,

    -- * Destructuring the response
    UpdateRelationalDatabaseParametersResponse (..),
    mkUpdateRelationalDatabaseParametersResponse,

    -- ** Response lenses
    urdprrsOperations,
    urdprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateRelationalDatabaseParameters' smart constructor.
data UpdateRelationalDatabaseParameters = UpdateRelationalDatabaseParameters'
  { -- | The name of your database for which to update parameters.
    relationalDatabaseName :: Types.RelationalDatabaseName,
    -- | The database parameters to update.
    parameters :: [Types.RelationalDatabaseParameter]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateRelationalDatabaseParameters' value with any optional fields omitted.
mkUpdateRelationalDatabaseParameters ::
  -- | 'relationalDatabaseName'
  Types.RelationalDatabaseName ->
  UpdateRelationalDatabaseParameters
mkUpdateRelationalDatabaseParameters relationalDatabaseName =
  UpdateRelationalDatabaseParameters'
    { relationalDatabaseName,
      parameters = Core.mempty
    }

-- | The name of your database for which to update parameters.
--
-- /Note:/ Consider using 'relationalDatabaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urdpRelationalDatabaseName :: Lens.Lens' UpdateRelationalDatabaseParameters Types.RelationalDatabaseName
urdpRelationalDatabaseName = Lens.field @"relationalDatabaseName"
{-# DEPRECATED urdpRelationalDatabaseName "Use generic-lens or generic-optics with 'relationalDatabaseName' instead." #-}

-- | The database parameters to update.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urdpParameters :: Lens.Lens' UpdateRelationalDatabaseParameters [Types.RelationalDatabaseParameter]
urdpParameters = Lens.field @"parameters"
{-# DEPRECATED urdpParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

instance Core.FromJSON UpdateRelationalDatabaseParameters where
  toJSON UpdateRelationalDatabaseParameters {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("relationalDatabaseName" Core..= relationalDatabaseName),
            Core.Just ("parameters" Core..= parameters)
          ]
      )

instance Core.AWSRequest UpdateRelationalDatabaseParameters where
  type
    Rs UpdateRelationalDatabaseParameters =
      UpdateRelationalDatabaseParametersResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "Lightsail_20161128.UpdateRelationalDatabaseParameters"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateRelationalDatabaseParametersResponse'
            Core.<$> (x Core..:? "operations") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateRelationalDatabaseParametersResponse' smart constructor.
data UpdateRelationalDatabaseParametersResponse = UpdateRelationalDatabaseParametersResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operations :: Core.Maybe [Types.Operation],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdateRelationalDatabaseParametersResponse' value with any optional fields omitted.
mkUpdateRelationalDatabaseParametersResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateRelationalDatabaseParametersResponse
mkUpdateRelationalDatabaseParametersResponse responseStatus =
  UpdateRelationalDatabaseParametersResponse'
    { operations =
        Core.Nothing,
      responseStatus
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urdprrsOperations :: Lens.Lens' UpdateRelationalDatabaseParametersResponse (Core.Maybe [Types.Operation])
urdprrsOperations = Lens.field @"operations"
{-# DEPRECATED urdprrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urdprrsResponseStatus :: Lens.Lens' UpdateRelationalDatabaseParametersResponse Core.Int
urdprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED urdprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
