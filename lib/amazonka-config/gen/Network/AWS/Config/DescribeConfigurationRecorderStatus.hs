{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DescribeConfigurationRecorderStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the current status of the specified configuration recorder. If a configuration recorder is not specified, this action returns the status of all configuration recorders associated with the account.
module Network.AWS.Config.DescribeConfigurationRecorderStatus
  ( -- * Creating a request
    DescribeConfigurationRecorderStatus (..),
    mkDescribeConfigurationRecorderStatus,

    -- ** Request lenses
    dcrsConfigurationRecorderNames,

    -- * Destructuring the response
    DescribeConfigurationRecorderStatusResponse (..),
    mkDescribeConfigurationRecorderStatusResponse,

    -- ** Response lenses
    dcrsrrsConfigurationRecordersStatus,
    dcrsrrsResponseStatus,
  )
where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the 'DescribeConfigurationRecorderStatus' action.
--
-- /See:/ 'mkDescribeConfigurationRecorderStatus' smart constructor.
newtype DescribeConfigurationRecorderStatus = DescribeConfigurationRecorderStatus'
  { -- | The name(s) of the configuration recorder. If the name is not specified, the action returns the current status of all the configuration recorders associated with the account.
    configurationRecorderNames :: Core.Maybe [Types.RecorderName]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeConfigurationRecorderStatus' value with any optional fields omitted.
mkDescribeConfigurationRecorderStatus ::
  DescribeConfigurationRecorderStatus
mkDescribeConfigurationRecorderStatus =
  DescribeConfigurationRecorderStatus'
    { configurationRecorderNames =
        Core.Nothing
    }

-- | The name(s) of the configuration recorder. If the name is not specified, the action returns the current status of all the configuration recorders associated with the account.
--
-- /Note:/ Consider using 'configurationRecorderNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsConfigurationRecorderNames :: Lens.Lens' DescribeConfigurationRecorderStatus (Core.Maybe [Types.RecorderName])
dcrsConfigurationRecorderNames = Lens.field @"configurationRecorderNames"
{-# DEPRECATED dcrsConfigurationRecorderNames "Use generic-lens or generic-optics with 'configurationRecorderNames' instead." #-}

instance Core.FromJSON DescribeConfigurationRecorderStatus where
  toJSON DescribeConfigurationRecorderStatus {..} =
    Core.object
      ( Core.catMaybes
          [ ("ConfigurationRecorderNames" Core..=)
              Core.<$> configurationRecorderNames
          ]
      )

instance Core.AWSRequest DescribeConfigurationRecorderStatus where
  type
    Rs DescribeConfigurationRecorderStatus =
      DescribeConfigurationRecorderStatusResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "StarlingDoveService.DescribeConfigurationRecorderStatus"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeConfigurationRecorderStatusResponse'
            Core.<$> (x Core..:? "ConfigurationRecordersStatus")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The output for the 'DescribeConfigurationRecorderStatus' action, in JSON format.
--
-- /See:/ 'mkDescribeConfigurationRecorderStatusResponse' smart constructor.
data DescribeConfigurationRecorderStatusResponse = DescribeConfigurationRecorderStatusResponse'
  { -- | A list that contains status of the specified recorders.
    configurationRecordersStatus :: Core.Maybe [Types.ConfigurationRecorderStatus],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeConfigurationRecorderStatusResponse' value with any optional fields omitted.
mkDescribeConfigurationRecorderStatusResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeConfigurationRecorderStatusResponse
mkDescribeConfigurationRecorderStatusResponse responseStatus =
  DescribeConfigurationRecorderStatusResponse'
    { configurationRecordersStatus =
        Core.Nothing,
      responseStatus
    }

-- | A list that contains status of the specified recorders.
--
-- /Note:/ Consider using 'configurationRecordersStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsrrsConfigurationRecordersStatus :: Lens.Lens' DescribeConfigurationRecorderStatusResponse (Core.Maybe [Types.ConfigurationRecorderStatus])
dcrsrrsConfigurationRecordersStatus = Lens.field @"configurationRecordersStatus"
{-# DEPRECATED dcrsrrsConfigurationRecordersStatus "Use generic-lens or generic-optics with 'configurationRecordersStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsrrsResponseStatus :: Lens.Lens' DescribeConfigurationRecorderStatusResponse Core.Int
dcrsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcrsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
