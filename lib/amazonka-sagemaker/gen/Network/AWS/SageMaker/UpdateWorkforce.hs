{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.UpdateWorkforce
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this operation to update your workforce. You can use this operation to require that workers use specific IP addresses to work on tasks and to update your OpenID Connect (OIDC) Identity Provider (IdP) workforce configuration.
--
-- Use @SourceIpConfig@ to restrict worker access to tasks to a specific range of IP addresses. You specify allowed IP addresses by creating a list of up to ten <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html CIDRs> . By default, a workforce isn't restricted to specific IP addresses. If you specify a range of IP addresses, workers who attempt to access tasks using any IP address outside the specified range are denied and get a @Not Found@ error message on the worker portal.
-- Use @OidcConfig@ to update the configuration of a workforce created using your own OIDC IdP.
-- /Important:/ You can only update your OIDC IdP configuration when there are no work teams associated with your workforce. You can delete work teams using the operation.
-- After restricting access to a range of IP addresses or updating your OIDC IdP configuration with this operation, you can view details about your update workforce using the operation.
-- /Important:/ This operation only applies to private workforces.
module Network.AWS.SageMaker.UpdateWorkforce
  ( -- * Creating a request
    UpdateWorkforce (..),
    mkUpdateWorkforce,

    -- ** Request lenses
    uwWorkforceName,
    uwOidcConfig,
    uwSourceIpConfig,

    -- * Destructuring the response
    UpdateWorkforceResponse (..),
    mkUpdateWorkforceResponse,

    -- ** Response lenses
    ursWorkforce,
    ursResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkUpdateWorkforce' smart constructor.
data UpdateWorkforce = UpdateWorkforce'
  { -- | The name of the private workforce that you want to update. You can find your workforce name by using the operation.
    workforceName :: Types.WorkforceName,
    -- | Use this parameter to update your OIDC Identity Provider (IdP) configuration for a workforce made using your own IdP.
    oidcConfig :: Core.Maybe Types.OidcConfig,
    -- | A list of one to ten worker IP address ranges (<https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html CIDRs> ) that can be used to access tasks assigned to this workforce.
    --
    -- Maximum: Ten CIDR values
    sourceIpConfig :: Core.Maybe Types.SourceIpConfig
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateWorkforce' value with any optional fields omitted.
mkUpdateWorkforce ::
  -- | 'workforceName'
  Types.WorkforceName ->
  UpdateWorkforce
mkUpdateWorkforce workforceName =
  UpdateWorkforce'
    { workforceName,
      oidcConfig = Core.Nothing,
      sourceIpConfig = Core.Nothing
    }

-- | The name of the private workforce that you want to update. You can find your workforce name by using the operation.
--
-- /Note:/ Consider using 'workforceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwWorkforceName :: Lens.Lens' UpdateWorkforce Types.WorkforceName
uwWorkforceName = Lens.field @"workforceName"
{-# DEPRECATED uwWorkforceName "Use generic-lens or generic-optics with 'workforceName' instead." #-}

-- | Use this parameter to update your OIDC Identity Provider (IdP) configuration for a workforce made using your own IdP.
--
-- /Note:/ Consider using 'oidcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwOidcConfig :: Lens.Lens' UpdateWorkforce (Core.Maybe Types.OidcConfig)
uwOidcConfig = Lens.field @"oidcConfig"
{-# DEPRECATED uwOidcConfig "Use generic-lens or generic-optics with 'oidcConfig' instead." #-}

-- | A list of one to ten worker IP address ranges (<https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html CIDRs> ) that can be used to access tasks assigned to this workforce.
--
-- Maximum: Ten CIDR values
--
-- /Note:/ Consider using 'sourceIpConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwSourceIpConfig :: Lens.Lens' UpdateWorkforce (Core.Maybe Types.SourceIpConfig)
uwSourceIpConfig = Lens.field @"sourceIpConfig"
{-# DEPRECATED uwSourceIpConfig "Use generic-lens or generic-optics with 'sourceIpConfig' instead." #-}

instance Core.FromJSON UpdateWorkforce where
  toJSON UpdateWorkforce {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("WorkforceName" Core..= workforceName),
            ("OidcConfig" Core..=) Core.<$> oidcConfig,
            ("SourceIpConfig" Core..=) Core.<$> sourceIpConfig
          ]
      )

instance Core.AWSRequest UpdateWorkforce where
  type Rs UpdateWorkforce = UpdateWorkforceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.UpdateWorkforce")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateWorkforceResponse'
            Core.<$> (x Core..: "Workforce") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateWorkforceResponse' smart constructor.
data UpdateWorkforceResponse = UpdateWorkforceResponse'
  { -- | A single private workforce. You can create one private work force in each AWS Region. By default, any workforce-related API operation used in a specific region will apply to the workforce created in that region. To learn how to create a private workforce, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-workforce-create-private.html Create a Private Workforce> .
    workforce :: Types.Workforce,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdateWorkforceResponse' value with any optional fields omitted.
mkUpdateWorkforceResponse ::
  -- | 'workforce'
  Types.Workforce ->
  -- | 'responseStatus'
  Core.Int ->
  UpdateWorkforceResponse
mkUpdateWorkforceResponse workforce responseStatus =
  UpdateWorkforceResponse' {workforce, responseStatus}

-- | A single private workforce. You can create one private work force in each AWS Region. By default, any workforce-related API operation used in a specific region will apply to the workforce created in that region. To learn how to create a private workforce, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-workforce-create-private.html Create a Private Workforce> .
--
-- /Note:/ Consider using 'workforce' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursWorkforce :: Lens.Lens' UpdateWorkforceResponse Types.Workforce
ursWorkforce = Lens.field @"workforce"
{-# DEPRECATED ursWorkforce "Use generic-lens or generic-optics with 'workforce' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursResponseStatus :: Lens.Lens' UpdateWorkforceResponse Core.Int
ursResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
