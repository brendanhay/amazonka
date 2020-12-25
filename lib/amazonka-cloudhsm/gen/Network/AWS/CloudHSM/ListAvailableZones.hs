{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.ListAvailableZones
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__ . For more information, see <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs> , the <http://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference> .
--
-- __For information about the current version of AWS CloudHSM__ , see <http://aws.amazon.com/cloudhsm/ AWS CloudHSM> , the <http://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference> .
-- Lists the Availability Zones that have available AWS CloudHSM capacity.
module Network.AWS.CloudHSM.ListAvailableZones
  ( -- * Creating a request
    ListAvailableZones (..),
    mkListAvailableZones,

    -- * Destructuring the response
    ListAvailableZonesResponse (..),
    mkListAvailableZonesResponse,

    -- ** Response lenses
    lazrrsAZList,
    lazrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudHSM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the inputs for the 'ListAvailableZones' action.
--
-- /See:/ 'mkListAvailableZones' smart constructor.
data ListAvailableZones = ListAvailableZones'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListAvailableZones' value with any optional fields omitted.
mkListAvailableZones ::
  ListAvailableZones
mkListAvailableZones = ListAvailableZones'

instance Core.FromJSON ListAvailableZones where
  toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest ListAvailableZones where
  type Rs ListAvailableZones = ListAvailableZonesResponse
  request x@_ =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CloudHsmFrontendService.ListAvailableZones")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAvailableZonesResponse'
            Core.<$> (x Core..:? "AZList") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListAvailableZonesResponse' smart constructor.
data ListAvailableZonesResponse = ListAvailableZonesResponse'
  { -- | The list of Availability Zones that have available AWS CloudHSM capacity.
    aZList :: Core.Maybe [Types.AZ],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListAvailableZonesResponse' value with any optional fields omitted.
mkListAvailableZonesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListAvailableZonesResponse
mkListAvailableZonesResponse responseStatus =
  ListAvailableZonesResponse'
    { aZList = Core.Nothing,
      responseStatus
    }

-- | The list of Availability Zones that have available AWS CloudHSM capacity.
--
-- /Note:/ Consider using 'aZList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lazrrsAZList :: Lens.Lens' ListAvailableZonesResponse (Core.Maybe [Types.AZ])
lazrrsAZList = Lens.field @"aZList"
{-# DEPRECATED lazrrsAZList "Use generic-lens or generic-optics with 'aZList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lazrrsResponseStatus :: Lens.Lens' ListAvailableZonesResponse Core.Int
lazrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lazrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
