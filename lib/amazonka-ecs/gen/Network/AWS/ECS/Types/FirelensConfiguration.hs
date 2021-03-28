{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.FirelensConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.FirelensConfiguration
  ( FirelensConfiguration (..)
  -- * Smart constructor
  , mkFirelensConfiguration
  -- * Lenses
  , fcType
  , fcOptions
  ) where

import qualified Network.AWS.ECS.Types.FirelensConfigurationType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The FireLens configuration for the container. This is used to specify and configure a log router for container logs. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using_firelens.html Custom Log Routing> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /See:/ 'mkFirelensConfiguration' smart constructor.
data FirelensConfiguration = FirelensConfiguration'
  { type' :: Types.FirelensConfigurationType
    -- ^ The log router to use. The valid values are @fluentd@ or @fluentbit@ .
  , options :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ The options to use when configuring the log router. This field is optional and can be used to specify a custom configuration file or to add additional metadata, such as the task, task definition, cluster, and container instance details to the log event. If specified, the syntax to use is @"options":{"enable-ecs-log-metadata":"true|false","config-file-type:"s3|file","config-file-value":"arn:aws:s3:::mybucket/fluent.conf|filepath"}@ . For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using_firelens.html#firelens-taskdef Creating a Task Definition that Uses a FireLens Configuration> in the /Amazon Elastic Container Service Developer Guide/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FirelensConfiguration' value with any optional fields omitted.
mkFirelensConfiguration
    :: Types.FirelensConfigurationType -- ^ 'type\''
    -> FirelensConfiguration
mkFirelensConfiguration type'
  = FirelensConfiguration'{type', options = Core.Nothing}

-- | The log router to use. The valid values are @fluentd@ or @fluentbit@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcType :: Lens.Lens' FirelensConfiguration Types.FirelensConfigurationType
fcType = Lens.field @"type'"
{-# INLINEABLE fcType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The options to use when configuring the log router. This field is optional and can be used to specify a custom configuration file or to add additional metadata, such as the task, task definition, cluster, and container instance details to the log event. If specified, the syntax to use is @"options":{"enable-ecs-log-metadata":"true|false","config-file-type:"s3|file","config-file-value":"arn:aws:s3:::mybucket/fluent.conf|filepath"}@ . For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using_firelens.html#firelens-taskdef Creating a Task Definition that Uses a FireLens Configuration> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcOptions :: Lens.Lens' FirelensConfiguration (Core.Maybe (Core.HashMap Core.Text Core.Text))
fcOptions = Lens.field @"options"
{-# INLINEABLE fcOptions #-}
{-# DEPRECATED options "Use generic-lens or generic-optics with 'options' instead"  #-}

instance Core.FromJSON FirelensConfiguration where
        toJSON FirelensConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("type" Core..= type'),
                  ("options" Core..=) Core.<$> options])

instance Core.FromJSON FirelensConfiguration where
        parseJSON
          = Core.withObject "FirelensConfiguration" Core.$
              \ x ->
                FirelensConfiguration' Core.<$>
                  (x Core..: "type") Core.<*> x Core..:? "options"
