{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyDefaultCreditSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the default credit option for CPU usage of burstable performance instances. The default credit option is set at the account level per AWS Region, and is specified per instance family. All new burstable performance instances in the account launch using the default credit option.
--
-- @ModifyDefaultCreditSpecification@ is an asynchronous operation, which works at an AWS Region level and modifies the credit option for each Availability Zone. All zones in a Region are updated within five minutes. But if instances are launched during this operation, they might not get the new credit option until the zone is updated. To verify whether the update has occurred, you can call @GetDefaultCreditSpecification@ and check @DefaultCreditSpecification@ for updates.
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/burstable-performance-instances.html Burstable performance instances> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.ModifyDefaultCreditSpecification
  ( -- * Creating a request
    ModifyDefaultCreditSpecification (..),
    mkModifyDefaultCreditSpecification,

    -- ** Request lenses
    mdcsInstanceFamily,
    mdcsCpuCredits,
    mdcsDryRun,

    -- * Destructuring the response
    ModifyDefaultCreditSpecificationResponse (..),
    mkModifyDefaultCreditSpecificationResponse,

    -- ** Response lenses
    mdcsrrsInstanceFamilyCreditSpecification,
    mdcsrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyDefaultCreditSpecification' smart constructor.
data ModifyDefaultCreditSpecification = ModifyDefaultCreditSpecification'
  { -- | The instance family.
    instanceFamily :: Types.UnlimitedSupportedInstanceFamily,
    -- | The credit option for CPU usage of the instance family.
    --
    -- Valid Values: @standard@ | @unlimited@
    cpuCredits :: Types.String,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyDefaultCreditSpecification' value with any optional fields omitted.
mkModifyDefaultCreditSpecification ::
  -- | 'instanceFamily'
  Types.UnlimitedSupportedInstanceFamily ->
  -- | 'cpuCredits'
  Types.String ->
  ModifyDefaultCreditSpecification
mkModifyDefaultCreditSpecification instanceFamily cpuCredits =
  ModifyDefaultCreditSpecification'
    { instanceFamily,
      cpuCredits,
      dryRun = Core.Nothing
    }

-- | The instance family.
--
-- /Note:/ Consider using 'instanceFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdcsInstanceFamily :: Lens.Lens' ModifyDefaultCreditSpecification Types.UnlimitedSupportedInstanceFamily
mdcsInstanceFamily = Lens.field @"instanceFamily"
{-# DEPRECATED mdcsInstanceFamily "Use generic-lens or generic-optics with 'instanceFamily' instead." #-}

-- | The credit option for CPU usage of the instance family.
--
-- Valid Values: @standard@ | @unlimited@
--
-- /Note:/ Consider using 'cpuCredits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdcsCpuCredits :: Lens.Lens' ModifyDefaultCreditSpecification Types.String
mdcsCpuCredits = Lens.field @"cpuCredits"
{-# DEPRECATED mdcsCpuCredits "Use generic-lens or generic-optics with 'cpuCredits' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdcsDryRun :: Lens.Lens' ModifyDefaultCreditSpecification (Core.Maybe Core.Bool)
mdcsDryRun = Lens.field @"dryRun"
{-# DEPRECATED mdcsDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest ModifyDefaultCreditSpecification where
  type
    Rs ModifyDefaultCreditSpecification =
      ModifyDefaultCreditSpecificationResponse
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
            ( Core.pure ("Action", "ModifyDefaultCreditSpecification")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "InstanceFamily" instanceFamily)
                Core.<> (Core.toQueryValue "CpuCredits" cpuCredits)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyDefaultCreditSpecificationResponse'
            Core.<$> (x Core..@? "instanceFamilyCreditSpecification")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkModifyDefaultCreditSpecificationResponse' smart constructor.
data ModifyDefaultCreditSpecificationResponse = ModifyDefaultCreditSpecificationResponse'
  { -- | The default credit option for CPU usage of the instance family.
    instanceFamilyCreditSpecification :: Core.Maybe Types.InstanceFamilyCreditSpecification,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyDefaultCreditSpecificationResponse' value with any optional fields omitted.
mkModifyDefaultCreditSpecificationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ModifyDefaultCreditSpecificationResponse
mkModifyDefaultCreditSpecificationResponse responseStatus =
  ModifyDefaultCreditSpecificationResponse'
    { instanceFamilyCreditSpecification =
        Core.Nothing,
      responseStatus
    }

-- | The default credit option for CPU usage of the instance family.
--
-- /Note:/ Consider using 'instanceFamilyCreditSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdcsrrsInstanceFamilyCreditSpecification :: Lens.Lens' ModifyDefaultCreditSpecificationResponse (Core.Maybe Types.InstanceFamilyCreditSpecification)
mdcsrrsInstanceFamilyCreditSpecification = Lens.field @"instanceFamilyCreditSpecification"
{-# DEPRECATED mdcsrrsInstanceFamilyCreditSpecification "Use generic-lens or generic-optics with 'instanceFamilyCreditSpecification' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdcsrrsResponseStatus :: Lens.Lens' ModifyDefaultCreditSpecificationResponse Core.Int
mdcsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED mdcsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
