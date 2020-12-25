{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DescribeConfigurationRecorders
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the details for the specified configuration recorders. If the configuration recorder is not specified, this action returns the details for all configuration recorders associated with the account.
module Network.AWS.Config.DescribeConfigurationRecorders
  ( -- * Creating a request
    DescribeConfigurationRecorders (..),
    mkDescribeConfigurationRecorders,

    -- ** Request lenses
    dcrConfigurationRecorderNames,

    -- * Destructuring the response
    DescribeConfigurationRecordersResponse (..),
    mkDescribeConfigurationRecordersResponse,

    -- ** Response lenses
    dcrrfrsConfigurationRecorders,
    dcrrfrsResponseStatus,
  )
where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the 'DescribeConfigurationRecorders' action.
--
-- /See:/ 'mkDescribeConfigurationRecorders' smart constructor.
newtype DescribeConfigurationRecorders = DescribeConfigurationRecorders'
  { -- | A list of configuration recorder names.
    configurationRecorderNames :: Core.Maybe [Types.RecorderName]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeConfigurationRecorders' value with any optional fields omitted.
mkDescribeConfigurationRecorders ::
  DescribeConfigurationRecorders
mkDescribeConfigurationRecorders =
  DescribeConfigurationRecorders'
    { configurationRecorderNames =
        Core.Nothing
    }

-- | A list of configuration recorder names.
--
-- /Note:/ Consider using 'configurationRecorderNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrConfigurationRecorderNames :: Lens.Lens' DescribeConfigurationRecorders (Core.Maybe [Types.RecorderName])
dcrConfigurationRecorderNames = Lens.field @"configurationRecorderNames"
{-# DEPRECATED dcrConfigurationRecorderNames "Use generic-lens or generic-optics with 'configurationRecorderNames' instead." #-}

instance Core.FromJSON DescribeConfigurationRecorders where
  toJSON DescribeConfigurationRecorders {..} =
    Core.object
      ( Core.catMaybes
          [ ("ConfigurationRecorderNames" Core..=)
              Core.<$> configurationRecorderNames
          ]
      )

instance Core.AWSRequest DescribeConfigurationRecorders where
  type
    Rs DescribeConfigurationRecorders =
      DescribeConfigurationRecordersResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "StarlingDoveService.DescribeConfigurationRecorders"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeConfigurationRecordersResponse'
            Core.<$> (x Core..:? "ConfigurationRecorders")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The output for the 'DescribeConfigurationRecorders' action.
--
-- /See:/ 'mkDescribeConfigurationRecordersResponse' smart constructor.
data DescribeConfigurationRecordersResponse = DescribeConfigurationRecordersResponse'
  { -- | A list that contains the descriptions of the specified configuration recorders.
    configurationRecorders :: Core.Maybe [Types.ConfigurationRecorder],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeConfigurationRecordersResponse' value with any optional fields omitted.
mkDescribeConfigurationRecordersResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeConfigurationRecordersResponse
mkDescribeConfigurationRecordersResponse responseStatus =
  DescribeConfigurationRecordersResponse'
    { configurationRecorders =
        Core.Nothing,
      responseStatus
    }

-- | A list that contains the descriptions of the specified configuration recorders.
--
-- /Note:/ Consider using 'configurationRecorders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrfrsConfigurationRecorders :: Lens.Lens' DescribeConfigurationRecordersResponse (Core.Maybe [Types.ConfigurationRecorder])
dcrrfrsConfigurationRecorders = Lens.field @"configurationRecorders"
{-# DEPRECATED dcrrfrsConfigurationRecorders "Use generic-lens or generic-optics with 'configurationRecorders' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrfrsResponseStatus :: Lens.Lens' DescribeConfigurationRecordersResponse Core.Int
dcrrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcrrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
