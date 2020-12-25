{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DescribeEventConfigurations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes event configurations.
module Network.AWS.IoT.DescribeEventConfigurations
  ( -- * Creating a request
    DescribeEventConfigurations (..),
    mkDescribeEventConfigurations,

    -- * Destructuring the response
    DescribeEventConfigurationsResponse (..),
    mkDescribeEventConfigurationsResponse,

    -- ** Response lenses
    decrrsCreationDate,
    decrrsEventConfigurations,
    decrrsLastModifiedDate,
    decrrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeEventConfigurations' smart constructor.
data DescribeEventConfigurations = DescribeEventConfigurations'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEventConfigurations' value with any optional fields omitted.
mkDescribeEventConfigurations ::
  DescribeEventConfigurations
mkDescribeEventConfigurations = DescribeEventConfigurations'

instance Core.AWSRequest DescribeEventConfigurations where
  type
    Rs DescribeEventConfigurations =
      DescribeEventConfigurationsResponse
  request x@_ =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/event-configurations",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEventConfigurationsResponse'
            Core.<$> (x Core..:? "creationDate")
            Core.<*> (x Core..:? "eventConfigurations")
            Core.<*> (x Core..:? "lastModifiedDate")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeEventConfigurationsResponse' smart constructor.
data DescribeEventConfigurationsResponse = DescribeEventConfigurationsResponse'
  { -- | The creation date of the event configuration.
    creationDate :: Core.Maybe Core.NominalDiffTime,
    -- | The event configurations.
    eventConfigurations :: Core.Maybe (Core.HashMap Types.EventType Types.Configuration),
    -- | The date the event configurations were last modified.
    lastModifiedDate :: Core.Maybe Core.NominalDiffTime,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeEventConfigurationsResponse' value with any optional fields omitted.
mkDescribeEventConfigurationsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeEventConfigurationsResponse
mkDescribeEventConfigurationsResponse responseStatus =
  DescribeEventConfigurationsResponse'
    { creationDate = Core.Nothing,
      eventConfigurations = Core.Nothing,
      lastModifiedDate = Core.Nothing,
      responseStatus
    }

-- | The creation date of the event configuration.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decrrsCreationDate :: Lens.Lens' DescribeEventConfigurationsResponse (Core.Maybe Core.NominalDiffTime)
decrrsCreationDate = Lens.field @"creationDate"
{-# DEPRECATED decrrsCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The event configurations.
--
-- /Note:/ Consider using 'eventConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decrrsEventConfigurations :: Lens.Lens' DescribeEventConfigurationsResponse (Core.Maybe (Core.HashMap Types.EventType Types.Configuration))
decrrsEventConfigurations = Lens.field @"eventConfigurations"
{-# DEPRECATED decrrsEventConfigurations "Use generic-lens or generic-optics with 'eventConfigurations' instead." #-}

-- | The date the event configurations were last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decrrsLastModifiedDate :: Lens.Lens' DescribeEventConfigurationsResponse (Core.Maybe Core.NominalDiffTime)
decrrsLastModifiedDate = Lens.field @"lastModifiedDate"
{-# DEPRECATED decrrsLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decrrsResponseStatus :: Lens.Lens' DescribeEventConfigurationsResponse Core.Int
decrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED decrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
