{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.PutRemediationConfigurations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates the remediation configuration with a specific AWS Config rule with the selected target or action. The API creates the @RemediationConfiguration@ object for the AWS Config rule. The AWS Config rule must already exist for you to add a remediation configuration. The target (SSM document) must exist and have permissions to use the target.
module Network.AWS.Config.PutRemediationConfigurations
  ( -- * Creating a request
    PutRemediationConfigurations (..),
    mkPutRemediationConfigurations,

    -- ** Request lenses
    prcRemediationConfigurations,

    -- * Destructuring the response
    PutRemediationConfigurationsResponse (..),
    mkPutRemediationConfigurationsResponse,

    -- ** Response lenses
    prcrrsFailedBatches,
    prcrrsResponseStatus,
  )
where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutRemediationConfigurations' smart constructor.
newtype PutRemediationConfigurations = PutRemediationConfigurations'
  { -- | A list of remediation configuration objects.
    remediationConfigurations :: [Types.RemediationConfiguration]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PutRemediationConfigurations' value with any optional fields omitted.
mkPutRemediationConfigurations ::
  PutRemediationConfigurations
mkPutRemediationConfigurations =
  PutRemediationConfigurations'
    { remediationConfigurations =
        Core.mempty
    }

-- | A list of remediation configuration objects.
--
-- /Note:/ Consider using 'remediationConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prcRemediationConfigurations :: Lens.Lens' PutRemediationConfigurations [Types.RemediationConfiguration]
prcRemediationConfigurations = Lens.field @"remediationConfigurations"
{-# DEPRECATED prcRemediationConfigurations "Use generic-lens or generic-optics with 'remediationConfigurations' instead." #-}

instance Core.FromJSON PutRemediationConfigurations where
  toJSON PutRemediationConfigurations {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("RemediationConfigurations" Core..= remediationConfigurations)
          ]
      )

instance Core.AWSRequest PutRemediationConfigurations where
  type
    Rs PutRemediationConfigurations =
      PutRemediationConfigurationsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "StarlingDoveService.PutRemediationConfigurations"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          PutRemediationConfigurationsResponse'
            Core.<$> (x Core..:? "FailedBatches")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkPutRemediationConfigurationsResponse' smart constructor.
data PutRemediationConfigurationsResponse = PutRemediationConfigurationsResponse'
  { -- | Returns a list of failed remediation batch objects.
    failedBatches :: Core.Maybe [Types.FailedRemediationBatch],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutRemediationConfigurationsResponse' value with any optional fields omitted.
mkPutRemediationConfigurationsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PutRemediationConfigurationsResponse
mkPutRemediationConfigurationsResponse responseStatus =
  PutRemediationConfigurationsResponse'
    { failedBatches =
        Core.Nothing,
      responseStatus
    }

-- | Returns a list of failed remediation batch objects.
--
-- /Note:/ Consider using 'failedBatches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prcrrsFailedBatches :: Lens.Lens' PutRemediationConfigurationsResponse (Core.Maybe [Types.FailedRemediationBatch])
prcrrsFailedBatches = Lens.field @"failedBatches"
{-# DEPRECATED prcrrsFailedBatches "Use generic-lens or generic-optics with 'failedBatches' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prcrrsResponseStatus :: Lens.Lens' PutRemediationConfigurationsResponse Core.Int
prcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED prcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
