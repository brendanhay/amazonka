{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.UpdateAvailabilityOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures the availability options for a domain. Enabling the Multi-AZ option expands an Amazon CloudSearch domain to an additional Availability Zone in the same Region to increase fault tolerance in the event of a service disruption. Changes to the Multi-AZ option can take about half an hour to become active. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-availability-options.html Configuring Availability Options> in the /Amazon CloudSearch Developer Guide/ .
module Network.AWS.CloudSearch.UpdateAvailabilityOptions
  ( -- * Creating a request
    UpdateAvailabilityOptions (..),
    mkUpdateAvailabilityOptions,

    -- ** Request lenses
    uaoDomainName,
    uaoMultiAZ,

    -- * Destructuring the response
    UpdateAvailabilityOptionsResponse (..),
    mkUpdateAvailabilityOptionsResponse,

    -- ** Response lenses
    uaorrsAvailabilityOptions,
    uaorrsResponseStatus,
  )
where

import qualified Network.AWS.CloudSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @'UpdateAvailabilityOptions' @ operation. Specifies the name of the domain you want to update and the Multi-AZ availability option.
--
-- /See:/ 'mkUpdateAvailabilityOptions' smart constructor.
data UpdateAvailabilityOptions = UpdateAvailabilityOptions'
  { domainName :: Types.DomainName,
    -- | You expand an existing search domain to a second Availability Zone by setting the Multi-AZ option to true. Similarly, you can turn off the Multi-AZ option to downgrade the domain to a single Availability Zone by setting the Multi-AZ option to @false@ .
    multiAZ :: Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateAvailabilityOptions' value with any optional fields omitted.
mkUpdateAvailabilityOptions ::
  -- | 'domainName'
  Types.DomainName ->
  -- | 'multiAZ'
  Core.Bool ->
  UpdateAvailabilityOptions
mkUpdateAvailabilityOptions domainName multiAZ =
  UpdateAvailabilityOptions' {domainName, multiAZ}

-- | Undocumented field.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaoDomainName :: Lens.Lens' UpdateAvailabilityOptions Types.DomainName
uaoDomainName = Lens.field @"domainName"
{-# DEPRECATED uaoDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | You expand an existing search domain to a second Availability Zone by setting the Multi-AZ option to true. Similarly, you can turn off the Multi-AZ option to downgrade the domain to a single Availability Zone by setting the Multi-AZ option to @false@ .
--
-- /Note:/ Consider using 'multiAZ' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaoMultiAZ :: Lens.Lens' UpdateAvailabilityOptions Core.Bool
uaoMultiAZ = Lens.field @"multiAZ"
{-# DEPRECATED uaoMultiAZ "Use generic-lens or generic-optics with 'multiAZ' instead." #-}

instance Core.AWSRequest UpdateAvailabilityOptions where
  type
    Rs UpdateAvailabilityOptions =
      UpdateAvailabilityOptionsResponse
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
            ( Core.pure ("Action", "UpdateAvailabilityOptions")
                Core.<> (Core.pure ("Version", "2013-01-01"))
                Core.<> (Core.toQueryValue "DomainName" domainName)
                Core.<> (Core.toQueryValue "MultiAZ" multiAZ)
            )
      }
  response =
    Response.receiveXMLWrapper
      "UpdateAvailabilityOptionsResult"
      ( \s h x ->
          UpdateAvailabilityOptionsResponse'
            Core.<$> (x Core..@? "AvailabilityOptions")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The result of a @UpdateAvailabilityOptions@ request. Contains the status of the domain's availability options.
--
-- /See:/ 'mkUpdateAvailabilityOptionsResponse' smart constructor.
data UpdateAvailabilityOptionsResponse = UpdateAvailabilityOptionsResponse'
  { -- | The newly-configured availability options. Indicates whether Multi-AZ is enabled for the domain.
    availabilityOptions :: Core.Maybe Types.AvailabilityOptionsStatus,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdateAvailabilityOptionsResponse' value with any optional fields omitted.
mkUpdateAvailabilityOptionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateAvailabilityOptionsResponse
mkUpdateAvailabilityOptionsResponse responseStatus =
  UpdateAvailabilityOptionsResponse'
    { availabilityOptions =
        Core.Nothing,
      responseStatus
    }

-- | The newly-configured availability options. Indicates whether Multi-AZ is enabled for the domain.
--
-- /Note:/ Consider using 'availabilityOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaorrsAvailabilityOptions :: Lens.Lens' UpdateAvailabilityOptionsResponse (Core.Maybe Types.AvailabilityOptionsStatus)
uaorrsAvailabilityOptions = Lens.field @"availabilityOptions"
{-# DEPRECATED uaorrsAvailabilityOptions "Use generic-lens or generic-optics with 'availabilityOptions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaorrsResponseStatus :: Lens.Lens' UpdateAvailabilityOptionsResponse Core.Int
uaorrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uaorrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
