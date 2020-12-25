{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.CheckDNSAvailability
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Checks if the specified CNAME is available.
module Network.AWS.ElasticBeanstalk.CheckDNSAvailability
  ( -- * Creating a request
    CheckDNSAvailability (..),
    mkCheckDNSAvailability,

    -- ** Request lenses
    cdnsaCNAMEPrefix,

    -- * Destructuring the response
    CheckDNSAvailabilityResponse (..),
    mkCheckDNSAvailabilityResponse,

    -- ** Response lenses
    cdnsarrsAvailable,
    cdnsarrsFullyQualifiedCNAME,
    cdnsarrsResponseStatus,
  )
where

import qualified Network.AWS.ElasticBeanstalk.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Results message indicating whether a CNAME is available.
--
-- /See:/ 'mkCheckDNSAvailability' smart constructor.
newtype CheckDNSAvailability = CheckDNSAvailability'
  { -- | The prefix used when this CNAME is reserved.
    cNAMEPrefix :: Types.DNSCnamePrefix
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CheckDNSAvailability' value with any optional fields omitted.
mkCheckDNSAvailability ::
  -- | 'cNAMEPrefix'
  Types.DNSCnamePrefix ->
  CheckDNSAvailability
mkCheckDNSAvailability cNAMEPrefix =
  CheckDNSAvailability' {cNAMEPrefix}

-- | The prefix used when this CNAME is reserved.
--
-- /Note:/ Consider using 'cNAMEPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdnsaCNAMEPrefix :: Lens.Lens' CheckDNSAvailability Types.DNSCnamePrefix
cdnsaCNAMEPrefix = Lens.field @"cNAMEPrefix"
{-# DEPRECATED cdnsaCNAMEPrefix "Use generic-lens or generic-optics with 'cNAMEPrefix' instead." #-}

instance Core.AWSRequest CheckDNSAvailability where
  type Rs CheckDNSAvailability = CheckDNSAvailabilityResponse
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
            ( Core.pure ("Action", "CheckDNSAvailability")
                Core.<> (Core.pure ("Version", "2010-12-01"))
                Core.<> (Core.toQueryValue "CNAMEPrefix" cNAMEPrefix)
            )
      }
  response =
    Response.receiveXMLWrapper
      "CheckDNSAvailabilityResult"
      ( \s h x ->
          CheckDNSAvailabilityResponse'
            Core.<$> (x Core..@? "Available")
            Core.<*> (x Core..@? "FullyQualifiedCNAME")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Indicates if the specified CNAME is available.
--
-- /See:/ 'mkCheckDNSAvailabilityResponse' smart constructor.
data CheckDNSAvailabilityResponse = CheckDNSAvailabilityResponse'
  { -- | Indicates if the specified CNAME is available:
    --
    --
    --     * @true@ : The CNAME is available.
    --
    --
    --     * @false@ : The CNAME is not available.
    available :: Core.Maybe Core.Bool,
    -- | The fully qualified CNAME to reserve when 'CreateEnvironment' is called with the provided prefix.
    fullyQualifiedCNAME :: Core.Maybe Types.FullyQualifiedCNAME,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CheckDNSAvailabilityResponse' value with any optional fields omitted.
mkCheckDNSAvailabilityResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CheckDNSAvailabilityResponse
mkCheckDNSAvailabilityResponse responseStatus =
  CheckDNSAvailabilityResponse'
    { available = Core.Nothing,
      fullyQualifiedCNAME = Core.Nothing,
      responseStatus
    }

-- | Indicates if the specified CNAME is available:
--
--
--     * @true@ : The CNAME is available.
--
--
--     * @false@ : The CNAME is not available.
--
--
--
-- /Note:/ Consider using 'available' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdnsarrsAvailable :: Lens.Lens' CheckDNSAvailabilityResponse (Core.Maybe Core.Bool)
cdnsarrsAvailable = Lens.field @"available"
{-# DEPRECATED cdnsarrsAvailable "Use generic-lens or generic-optics with 'available' instead." #-}

-- | The fully qualified CNAME to reserve when 'CreateEnvironment' is called with the provided prefix.
--
-- /Note:/ Consider using 'fullyQualifiedCNAME' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdnsarrsFullyQualifiedCNAME :: Lens.Lens' CheckDNSAvailabilityResponse (Core.Maybe Types.FullyQualifiedCNAME)
cdnsarrsFullyQualifiedCNAME = Lens.field @"fullyQualifiedCNAME"
{-# DEPRECATED cdnsarrsFullyQualifiedCNAME "Use generic-lens or generic-optics with 'fullyQualifiedCNAME' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdnsarrsResponseStatus :: Lens.Lens' CheckDNSAvailabilityResponse Core.Int
cdnsarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cdnsarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
