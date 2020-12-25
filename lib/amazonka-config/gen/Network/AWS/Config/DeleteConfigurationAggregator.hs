{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DeleteConfigurationAggregator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified configuration aggregator and the aggregated data associated with the aggregator.
module Network.AWS.Config.DeleteConfigurationAggregator
  ( -- * Creating a request
    DeleteConfigurationAggregator (..),
    mkDeleteConfigurationAggregator,

    -- ** Request lenses
    dcaConfigurationAggregatorName,

    -- * Destructuring the response
    DeleteConfigurationAggregatorResponse (..),
    mkDeleteConfigurationAggregatorResponse,
  )
where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteConfigurationAggregator' smart constructor.
newtype DeleteConfigurationAggregator = DeleteConfigurationAggregator'
  { -- | The name of the configuration aggregator.
    configurationAggregatorName :: Types.ConfigurationAggregatorName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteConfigurationAggregator' value with any optional fields omitted.
mkDeleteConfigurationAggregator ::
  -- | 'configurationAggregatorName'
  Types.ConfigurationAggregatorName ->
  DeleteConfigurationAggregator
mkDeleteConfigurationAggregator configurationAggregatorName =
  DeleteConfigurationAggregator' {configurationAggregatorName}

-- | The name of the configuration aggregator.
--
-- /Note:/ Consider using 'configurationAggregatorName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaConfigurationAggregatorName :: Lens.Lens' DeleteConfigurationAggregator Types.ConfigurationAggregatorName
dcaConfigurationAggregatorName = Lens.field @"configurationAggregatorName"
{-# DEPRECATED dcaConfigurationAggregatorName "Use generic-lens or generic-optics with 'configurationAggregatorName' instead." #-}

instance Core.FromJSON DeleteConfigurationAggregator where
  toJSON DeleteConfigurationAggregator {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "ConfigurationAggregatorName"
                  Core..= configurationAggregatorName
              )
          ]
      )

instance Core.AWSRequest DeleteConfigurationAggregator where
  type
    Rs DeleteConfigurationAggregator =
      DeleteConfigurationAggregatorResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "StarlingDoveService.DeleteConfigurationAggregator"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveNull DeleteConfigurationAggregatorResponse'

-- | /See:/ 'mkDeleteConfigurationAggregatorResponse' smart constructor.
data DeleteConfigurationAggregatorResponse = DeleteConfigurationAggregatorResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteConfigurationAggregatorResponse' value with any optional fields omitted.
mkDeleteConfigurationAggregatorResponse ::
  DeleteConfigurationAggregatorResponse
mkDeleteConfigurationAggregatorResponse =
  DeleteConfigurationAggregatorResponse'
