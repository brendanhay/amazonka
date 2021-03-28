{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.SystemControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.SystemControl
  ( SystemControl (..)
  -- * Smart constructor
  , mkSystemControl
  -- * Lenses
  , scNamespace
  , scValue
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A list of namespaced kernel parameters to set in the container. This parameter maps to @Sysctls@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--sysctl@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
--
-- It is not recommended that you specify network-related @systemControls@ parameters for multiple containers in a single task that also uses either the @awsvpc@ or @host@ network mode for the following reasons:
--
--     * For tasks that use the @awsvpc@ network mode, if you set @systemControls@ for any container, it applies to all containers in the task. If you set different @systemControls@ for multiple containers in a single task, the container that is started last determines which @systemControls@ take effect.
--
--
--     * For tasks that use the @host@ network mode, the @systemControls@ parameter applies to the container instance's kernel parameter as well as that of all containers of any tasks running on that container instance.
--
--
--
-- /See:/ 'mkSystemControl' smart constructor.
data SystemControl = SystemControl'
  { namespace :: Core.Maybe Core.Text
    -- ^ The namespaced kernel parameter for which to set a @value@ .
  , value :: Core.Maybe Core.Text
    -- ^ The value for the namespaced kernel parameter specified in @namespace@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SystemControl' value with any optional fields omitted.
mkSystemControl
    :: SystemControl
mkSystemControl
  = SystemControl'{namespace = Core.Nothing, value = Core.Nothing}

-- | The namespaced kernel parameter for which to set a @value@ .
--
-- /Note:/ Consider using 'namespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scNamespace :: Lens.Lens' SystemControl (Core.Maybe Core.Text)
scNamespace = Lens.field @"namespace"
{-# INLINEABLE scNamespace #-}
{-# DEPRECATED namespace "Use generic-lens or generic-optics with 'namespace' instead"  #-}

-- | The value for the namespaced kernel parameter specified in @namespace@ .
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scValue :: Lens.Lens' SystemControl (Core.Maybe Core.Text)
scValue = Lens.field @"value"
{-# INLINEABLE scValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromJSON SystemControl where
        toJSON SystemControl{..}
          = Core.object
              (Core.catMaybes
                 [("namespace" Core..=) Core.<$> namespace,
                  ("value" Core..=) Core.<$> value])

instance Core.FromJSON SystemControl where
        parseJSON
          = Core.withObject "SystemControl" Core.$
              \ x ->
                SystemControl' Core.<$>
                  (x Core..:? "namespace") Core.<*> x Core..:? "value"
