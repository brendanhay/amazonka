{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.DescribeServices
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the current list of AWS services and a list of service categories for each service. You then use service names and categories in your 'CreateCase' requests. Each AWS service has its own set of categories.
--
-- The service codes and category codes correspond to the values that appear in the __Service__ and __Category__ lists on the AWS Support Center <https://console.aws.amazon.com/support/home#/case/create Create Case> page. The values in those fields don't necessarily match the service codes and categories returned by the @DescribeServices@ operation. Always use the service codes and categories that the @DescribeServices@ operation returns, so that you have the most recent set of service and category codes.
module Network.AWS.Support.DescribeServices
  ( -- * Creating a request
    DescribeServices (..),
    mkDescribeServices,

    -- ** Request lenses
    dsLanguage,
    dsServiceCodeList,

    -- * Destructuring the response
    DescribeServicesResponse (..),
    mkDescribeServicesResponse,

    -- ** Response lenses
    dsrrsServices,
    dsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Support.Types as Types

-- | /See:/ 'mkDescribeServices' smart constructor.
data DescribeServices = DescribeServices'
  { -- | The ISO 639-1 code for the language in which AWS provides support. AWS Support currently supports English ("en") and Japanese ("ja"). Language parameters must be passed explicitly for operations that take them.
    language :: Core.Maybe Types.Language,
    -- | A JSON-formatted list of service codes available for AWS services.
    serviceCodeList :: Core.Maybe [Types.ServiceCode]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeServices' value with any optional fields omitted.
mkDescribeServices ::
  DescribeServices
mkDescribeServices =
  DescribeServices'
    { language = Core.Nothing,
      serviceCodeList = Core.Nothing
    }

-- | The ISO 639-1 code for the language in which AWS provides support. AWS Support currently supports English ("en") and Japanese ("ja"). Language parameters must be passed explicitly for operations that take them.
--
-- /Note:/ Consider using 'language' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsLanguage :: Lens.Lens' DescribeServices (Core.Maybe Types.Language)
dsLanguage = Lens.field @"language"
{-# DEPRECATED dsLanguage "Use generic-lens or generic-optics with 'language' instead." #-}

-- | A JSON-formatted list of service codes available for AWS services.
--
-- /Note:/ Consider using 'serviceCodeList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsServiceCodeList :: Lens.Lens' DescribeServices (Core.Maybe [Types.ServiceCode])
dsServiceCodeList = Lens.field @"serviceCodeList"
{-# DEPRECATED dsServiceCodeList "Use generic-lens or generic-optics with 'serviceCodeList' instead." #-}

instance Core.FromJSON DescribeServices where
  toJSON DescribeServices {..} =
    Core.object
      ( Core.catMaybes
          [ ("language" Core..=) Core.<$> language,
            ("serviceCodeList" Core..=) Core.<$> serviceCodeList
          ]
      )

instance Core.AWSRequest DescribeServices where
  type Rs DescribeServices = DescribeServicesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSSupport_20130415.DescribeServices")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeServicesResponse'
            Core.<$> (x Core..:? "services") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The list of AWS services returned by the 'DescribeServices' operation.
--
-- /See:/ 'mkDescribeServicesResponse' smart constructor.
data DescribeServicesResponse = DescribeServicesResponse'
  { -- | A JSON-formatted list of AWS services.
    services :: Core.Maybe [Types.SupportService],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeServicesResponse' value with any optional fields omitted.
mkDescribeServicesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeServicesResponse
mkDescribeServicesResponse responseStatus =
  DescribeServicesResponse'
    { services = Core.Nothing,
      responseStatus
    }

-- | A JSON-formatted list of AWS services.
--
-- /Note:/ Consider using 'services' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsServices :: Lens.Lens' DescribeServicesResponse (Core.Maybe [Types.SupportService])
dsrrsServices = Lens.field @"services"
{-# DEPRECATED dsrrsServices "Use generic-lens or generic-optics with 'services' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsResponseStatus :: Lens.Lens' DescribeServicesResponse Core.Int
dsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
