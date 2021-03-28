{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeEventConfigurations (..)
    , mkDescribeEventConfigurations

    -- * Destructuring the response
    , DescribeEventConfigurationsResponse (..)
    , mkDescribeEventConfigurationsResponse
    -- ** Response lenses
    , decrrsCreationDate
    , decrrsEventConfigurations
    , decrrsLastModifiedDate
    , decrrsResponseStatus
    ) where

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
mkDescribeEventConfigurations
    :: DescribeEventConfigurations
mkDescribeEventConfigurations = DescribeEventConfigurations'

instance Core.ToQuery DescribeEventConfigurations where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeEventConfigurations where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeEventConfigurations where
        type Rs DescribeEventConfigurations =
             DescribeEventConfigurationsResponse
        toRequest x@_
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/event-configurations",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeEventConfigurationsResponse' Core.<$>
                   (x Core..:? "creationDate") Core.<*>
                     x Core..:? "eventConfigurations"
                     Core.<*> x Core..:? "lastModifiedDate"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeEventConfigurationsResponse' smart constructor.
data DescribeEventConfigurationsResponse = DescribeEventConfigurationsResponse'
  { creationDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The creation date of the event configuration.
  , eventConfigurations :: Core.Maybe (Core.HashMap Types.EventType Types.Configuration)
    -- ^ The event configurations.
  , lastModifiedDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date the event configurations were last modified.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeEventConfigurationsResponse' value with any optional fields omitted.
mkDescribeEventConfigurationsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeEventConfigurationsResponse
mkDescribeEventConfigurationsResponse responseStatus
  = DescribeEventConfigurationsResponse'{creationDate = Core.Nothing,
                                         eventConfigurations = Core.Nothing,
                                         lastModifiedDate = Core.Nothing, responseStatus}

-- | The creation date of the event configuration.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decrrsCreationDate :: Lens.Lens' DescribeEventConfigurationsResponse (Core.Maybe Core.NominalDiffTime)
decrrsCreationDate = Lens.field @"creationDate"
{-# INLINEABLE decrrsCreationDate #-}
{-# DEPRECATED creationDate "Use generic-lens or generic-optics with 'creationDate' instead"  #-}

-- | The event configurations.
--
-- /Note:/ Consider using 'eventConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decrrsEventConfigurations :: Lens.Lens' DescribeEventConfigurationsResponse (Core.Maybe (Core.HashMap Types.EventType Types.Configuration))
decrrsEventConfigurations = Lens.field @"eventConfigurations"
{-# INLINEABLE decrrsEventConfigurations #-}
{-# DEPRECATED eventConfigurations "Use generic-lens or generic-optics with 'eventConfigurations' instead"  #-}

-- | The date the event configurations were last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decrrsLastModifiedDate :: Lens.Lens' DescribeEventConfigurationsResponse (Core.Maybe Core.NominalDiffTime)
decrrsLastModifiedDate = Lens.field @"lastModifiedDate"
{-# INLINEABLE decrrsLastModifiedDate #-}
{-# DEPRECATED lastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decrrsResponseStatus :: Lens.Lens' DescribeEventConfigurationsResponse Core.Int
decrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE decrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
