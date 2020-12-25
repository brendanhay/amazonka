{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.GetDefaultCreditSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the default credit option for CPU usage of a burstable performance instance family.
--
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/burstable-performance-instances.html Burstable performance instances> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.GetDefaultCreditSpecification
  ( -- * Creating a request
    GetDefaultCreditSpecification (..),
    mkGetDefaultCreditSpecification,

    -- ** Request lenses
    gdcsInstanceFamily,
    gdcsDryRun,

    -- * Destructuring the response
    GetDefaultCreditSpecificationResponse (..),
    mkGetDefaultCreditSpecificationResponse,

    -- ** Response lenses
    gdcsrrsInstanceFamilyCreditSpecification,
    gdcsrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetDefaultCreditSpecification' smart constructor.
data GetDefaultCreditSpecification = GetDefaultCreditSpecification'
  { -- | The instance family.
    instanceFamily :: Types.UnlimitedSupportedInstanceFamily,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDefaultCreditSpecification' value with any optional fields omitted.
mkGetDefaultCreditSpecification ::
  -- | 'instanceFamily'
  Types.UnlimitedSupportedInstanceFamily ->
  GetDefaultCreditSpecification
mkGetDefaultCreditSpecification instanceFamily =
  GetDefaultCreditSpecification'
    { instanceFamily,
      dryRun = Core.Nothing
    }

-- | The instance family.
--
-- /Note:/ Consider using 'instanceFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdcsInstanceFamily :: Lens.Lens' GetDefaultCreditSpecification Types.UnlimitedSupportedInstanceFamily
gdcsInstanceFamily = Lens.field @"instanceFamily"
{-# DEPRECATED gdcsInstanceFamily "Use generic-lens or generic-optics with 'instanceFamily' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdcsDryRun :: Lens.Lens' GetDefaultCreditSpecification (Core.Maybe Core.Bool)
gdcsDryRun = Lens.field @"dryRun"
{-# DEPRECATED gdcsDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest GetDefaultCreditSpecification where
  type
    Rs GetDefaultCreditSpecification =
      GetDefaultCreditSpecificationResponse
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
            ( Core.pure ("Action", "GetDefaultCreditSpecification")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "InstanceFamily" instanceFamily)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          GetDefaultCreditSpecificationResponse'
            Core.<$> (x Core..@? "instanceFamilyCreditSpecification")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetDefaultCreditSpecificationResponse' smart constructor.
data GetDefaultCreditSpecificationResponse = GetDefaultCreditSpecificationResponse'
  { -- | The default credit option for CPU usage of the instance family.
    instanceFamilyCreditSpecification :: Core.Maybe Types.InstanceFamilyCreditSpecification,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDefaultCreditSpecificationResponse' value with any optional fields omitted.
mkGetDefaultCreditSpecificationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetDefaultCreditSpecificationResponse
mkGetDefaultCreditSpecificationResponse responseStatus =
  GetDefaultCreditSpecificationResponse'
    { instanceFamilyCreditSpecification =
        Core.Nothing,
      responseStatus
    }

-- | The default credit option for CPU usage of the instance family.
--
-- /Note:/ Consider using 'instanceFamilyCreditSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdcsrrsInstanceFamilyCreditSpecification :: Lens.Lens' GetDefaultCreditSpecificationResponse (Core.Maybe Types.InstanceFamilyCreditSpecification)
gdcsrrsInstanceFamilyCreditSpecification = Lens.field @"instanceFamilyCreditSpecification"
{-# DEPRECATED gdcsrrsInstanceFamilyCreditSpecification "Use generic-lens or generic-optics with 'instanceFamilyCreditSpecification' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdcsrrsResponseStatus :: Lens.Lens' GetDefaultCreditSpecificationResponse Core.Int
gdcsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gdcsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
