{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.DeleteAppValidationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the validation configuration for the specified application.
module Network.AWS.SMS.DeleteAppValidationConfiguration
  ( -- * Creating a request
    DeleteAppValidationConfiguration (..),
    mkDeleteAppValidationConfiguration,

    -- ** Request lenses
    davcAppId,

    -- * Destructuring the response
    DeleteAppValidationConfigurationResponse (..),
    mkDeleteAppValidationConfigurationResponse,

    -- ** Response lenses
    davcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SMS.Types as Types

-- | /See:/ 'mkDeleteAppValidationConfiguration' smart constructor.
newtype DeleteAppValidationConfiguration = DeleteAppValidationConfiguration'
  { -- | The ID of the application.
    appId :: Types.AppIdWithValidation
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAppValidationConfiguration' value with any optional fields omitted.
mkDeleteAppValidationConfiguration ::
  -- | 'appId'
  Types.AppIdWithValidation ->
  DeleteAppValidationConfiguration
mkDeleteAppValidationConfiguration appId =
  DeleteAppValidationConfiguration' {appId}

-- | The ID of the application.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
davcAppId :: Lens.Lens' DeleteAppValidationConfiguration Types.AppIdWithValidation
davcAppId = Lens.field @"appId"
{-# DEPRECATED davcAppId "Use generic-lens or generic-optics with 'appId' instead." #-}

instance Core.FromJSON DeleteAppValidationConfiguration where
  toJSON DeleteAppValidationConfiguration {..} =
    Core.object (Core.catMaybes [Core.Just ("appId" Core..= appId)])

instance Core.AWSRequest DeleteAppValidationConfiguration where
  type
    Rs DeleteAppValidationConfiguration =
      DeleteAppValidationConfigurationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSServerMigrationService_V2016_10_24.DeleteAppValidationConfiguration"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteAppValidationConfigurationResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteAppValidationConfigurationResponse' smart constructor.
newtype DeleteAppValidationConfigurationResponse = DeleteAppValidationConfigurationResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAppValidationConfigurationResponse' value with any optional fields omitted.
mkDeleteAppValidationConfigurationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteAppValidationConfigurationResponse
mkDeleteAppValidationConfigurationResponse responseStatus =
  DeleteAppValidationConfigurationResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
davcrrsResponseStatus :: Lens.Lens' DeleteAppValidationConfigurationResponse Core.Int
davcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED davcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
