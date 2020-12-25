{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.UpdateServiceAccessPolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures the access rules that control access to the domain's document and search endpoints. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-access.html Configuring Access for an Amazon CloudSearch Domain> .
module Network.AWS.CloudSearch.UpdateServiceAccessPolicies
  ( -- * Creating a request
    UpdateServiceAccessPolicies (..),
    mkUpdateServiceAccessPolicies,

    -- ** Request lenses
    usapDomainName,
    usapAccessPolicies,

    -- * Destructuring the response
    UpdateServiceAccessPoliciesResponse (..),
    mkUpdateServiceAccessPoliciesResponse,

    -- ** Response lenses
    usaprrsAccessPolicies,
    usaprrsResponseStatus,
  )
where

import qualified Network.AWS.CloudSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @'UpdateServiceAccessPolicies' @ operation. Specifies the name of the domain you want to update and the access rules you want to configure.
--
-- /See:/ 'mkUpdateServiceAccessPolicies' smart constructor.
data UpdateServiceAccessPolicies = UpdateServiceAccessPolicies'
  { domainName :: Types.DomainName,
    -- | The access rules you want to configure. These rules replace any existing rules.
    accessPolicies :: Types.PolicyDocument
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateServiceAccessPolicies' value with any optional fields omitted.
mkUpdateServiceAccessPolicies ::
  -- | 'domainName'
  Types.DomainName ->
  -- | 'accessPolicies'
  Types.PolicyDocument ->
  UpdateServiceAccessPolicies
mkUpdateServiceAccessPolicies domainName accessPolicies =
  UpdateServiceAccessPolicies' {domainName, accessPolicies}

-- | Undocumented field.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usapDomainName :: Lens.Lens' UpdateServiceAccessPolicies Types.DomainName
usapDomainName = Lens.field @"domainName"
{-# DEPRECATED usapDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | The access rules you want to configure. These rules replace any existing rules.
--
-- /Note:/ Consider using 'accessPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usapAccessPolicies :: Lens.Lens' UpdateServiceAccessPolicies Types.PolicyDocument
usapAccessPolicies = Lens.field @"accessPolicies"
{-# DEPRECATED usapAccessPolicies "Use generic-lens or generic-optics with 'accessPolicies' instead." #-}

instance Core.AWSRequest UpdateServiceAccessPolicies where
  type
    Rs UpdateServiceAccessPolicies =
      UpdateServiceAccessPoliciesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "UpdateServiceAccessPolicies")
                Core.<> (Core.pure ("Version", "2013-01-01"))
                Core.<> (Core.toQueryValue "DomainName" domainName)
                Core.<> (Core.toQueryValue "AccessPolicies" accessPolicies)
            )
      }
  response =
    Response.receiveXMLWrapper
      "UpdateServiceAccessPoliciesResult"
      ( \s h x ->
          UpdateServiceAccessPoliciesResponse'
            Core.<$> (x Core..@ "AccessPolicies")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The result of an @UpdateServiceAccessPolicies@ request. Contains the new access policies.
--
-- /See:/ 'mkUpdateServiceAccessPoliciesResponse' smart constructor.
data UpdateServiceAccessPoliciesResponse = UpdateServiceAccessPoliciesResponse'
  { -- | The access rules configured for the domain.
    accessPolicies :: Types.AccessPoliciesStatus,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdateServiceAccessPoliciesResponse' value with any optional fields omitted.
mkUpdateServiceAccessPoliciesResponse ::
  -- | 'accessPolicies'
  Types.AccessPoliciesStatus ->
  -- | 'responseStatus'
  Core.Int ->
  UpdateServiceAccessPoliciesResponse
mkUpdateServiceAccessPoliciesResponse accessPolicies responseStatus =
  UpdateServiceAccessPoliciesResponse'
    { accessPolicies,
      responseStatus
    }

-- | The access rules configured for the domain.
--
-- /Note:/ Consider using 'accessPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usaprrsAccessPolicies :: Lens.Lens' UpdateServiceAccessPoliciesResponse Types.AccessPoliciesStatus
usaprrsAccessPolicies = Lens.field @"accessPolicies"
{-# DEPRECATED usaprrsAccessPolicies "Use generic-lens or generic-optics with 'accessPolicies' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usaprrsResponseStatus :: Lens.Lens' UpdateServiceAccessPoliciesResponse Core.Int
usaprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED usaprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
