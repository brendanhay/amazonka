{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.GroupVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Greengrass.Types.GroupVersion
  ( GroupVersion (..)
  -- * Smart constructor
  , mkGroupVersion
  -- * Lenses
  , gvConnectorDefinitionVersionArn
  , gvCoreDefinitionVersionArn
  , gvDeviceDefinitionVersionArn
  , gvFunctionDefinitionVersionArn
  , gvLoggerDefinitionVersionArn
  , gvResourceDefinitionVersionArn
  , gvSubscriptionDefinitionVersionArn
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a group version.
--
-- /See:/ 'mkGroupVersion' smart constructor.
data GroupVersion = GroupVersion'
  { connectorDefinitionVersionArn :: Core.Maybe Core.Text
    -- ^ The ARN of the connector definition version for this group.
  , coreDefinitionVersionArn :: Core.Maybe Core.Text
    -- ^ The ARN of the core definition version for this group.
  , deviceDefinitionVersionArn :: Core.Maybe Core.Text
    -- ^ The ARN of the device definition version for this group.
  , functionDefinitionVersionArn :: Core.Maybe Core.Text
    -- ^ The ARN of the function definition version for this group.
  , loggerDefinitionVersionArn :: Core.Maybe Core.Text
    -- ^ The ARN of the logger definition version for this group.
  , resourceDefinitionVersionArn :: Core.Maybe Core.Text
    -- ^ The ARN of the resource definition version for this group.
  , subscriptionDefinitionVersionArn :: Core.Maybe Core.Text
    -- ^ The ARN of the subscription definition version for this group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GroupVersion' value with any optional fields omitted.
mkGroupVersion
    :: GroupVersion
mkGroupVersion
  = GroupVersion'{connectorDefinitionVersionArn = Core.Nothing,
                  coreDefinitionVersionArn = Core.Nothing,
                  deviceDefinitionVersionArn = Core.Nothing,
                  functionDefinitionVersionArn = Core.Nothing,
                  loggerDefinitionVersionArn = Core.Nothing,
                  resourceDefinitionVersionArn = Core.Nothing,
                  subscriptionDefinitionVersionArn = Core.Nothing}

-- | The ARN of the connector definition version for this group.
--
-- /Note:/ Consider using 'connectorDefinitionVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvConnectorDefinitionVersionArn :: Lens.Lens' GroupVersion (Core.Maybe Core.Text)
gvConnectorDefinitionVersionArn = Lens.field @"connectorDefinitionVersionArn"
{-# INLINEABLE gvConnectorDefinitionVersionArn #-}
{-# DEPRECATED connectorDefinitionVersionArn "Use generic-lens or generic-optics with 'connectorDefinitionVersionArn' instead"  #-}

-- | The ARN of the core definition version for this group.
--
-- /Note:/ Consider using 'coreDefinitionVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvCoreDefinitionVersionArn :: Lens.Lens' GroupVersion (Core.Maybe Core.Text)
gvCoreDefinitionVersionArn = Lens.field @"coreDefinitionVersionArn"
{-# INLINEABLE gvCoreDefinitionVersionArn #-}
{-# DEPRECATED coreDefinitionVersionArn "Use generic-lens or generic-optics with 'coreDefinitionVersionArn' instead"  #-}

-- | The ARN of the device definition version for this group.
--
-- /Note:/ Consider using 'deviceDefinitionVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvDeviceDefinitionVersionArn :: Lens.Lens' GroupVersion (Core.Maybe Core.Text)
gvDeviceDefinitionVersionArn = Lens.field @"deviceDefinitionVersionArn"
{-# INLINEABLE gvDeviceDefinitionVersionArn #-}
{-# DEPRECATED deviceDefinitionVersionArn "Use generic-lens or generic-optics with 'deviceDefinitionVersionArn' instead"  #-}

-- | The ARN of the function definition version for this group.
--
-- /Note:/ Consider using 'functionDefinitionVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvFunctionDefinitionVersionArn :: Lens.Lens' GroupVersion (Core.Maybe Core.Text)
gvFunctionDefinitionVersionArn = Lens.field @"functionDefinitionVersionArn"
{-# INLINEABLE gvFunctionDefinitionVersionArn #-}
{-# DEPRECATED functionDefinitionVersionArn "Use generic-lens or generic-optics with 'functionDefinitionVersionArn' instead"  #-}

-- | The ARN of the logger definition version for this group.
--
-- /Note:/ Consider using 'loggerDefinitionVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvLoggerDefinitionVersionArn :: Lens.Lens' GroupVersion (Core.Maybe Core.Text)
gvLoggerDefinitionVersionArn = Lens.field @"loggerDefinitionVersionArn"
{-# INLINEABLE gvLoggerDefinitionVersionArn #-}
{-# DEPRECATED loggerDefinitionVersionArn "Use generic-lens or generic-optics with 'loggerDefinitionVersionArn' instead"  #-}

-- | The ARN of the resource definition version for this group.
--
-- /Note:/ Consider using 'resourceDefinitionVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvResourceDefinitionVersionArn :: Lens.Lens' GroupVersion (Core.Maybe Core.Text)
gvResourceDefinitionVersionArn = Lens.field @"resourceDefinitionVersionArn"
{-# INLINEABLE gvResourceDefinitionVersionArn #-}
{-# DEPRECATED resourceDefinitionVersionArn "Use generic-lens or generic-optics with 'resourceDefinitionVersionArn' instead"  #-}

-- | The ARN of the subscription definition version for this group.
--
-- /Note:/ Consider using 'subscriptionDefinitionVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvSubscriptionDefinitionVersionArn :: Lens.Lens' GroupVersion (Core.Maybe Core.Text)
gvSubscriptionDefinitionVersionArn = Lens.field @"subscriptionDefinitionVersionArn"
{-# INLINEABLE gvSubscriptionDefinitionVersionArn #-}
{-# DEPRECATED subscriptionDefinitionVersionArn "Use generic-lens or generic-optics with 'subscriptionDefinitionVersionArn' instead"  #-}

instance Core.FromJSON GroupVersion where
        toJSON GroupVersion{..}
          = Core.object
              (Core.catMaybes
                 [("ConnectorDefinitionVersionArn" Core..=) Core.<$>
                    connectorDefinitionVersionArn,
                  ("CoreDefinitionVersionArn" Core..=) Core.<$>
                    coreDefinitionVersionArn,
                  ("DeviceDefinitionVersionArn" Core..=) Core.<$>
                    deviceDefinitionVersionArn,
                  ("FunctionDefinitionVersionArn" Core..=) Core.<$>
                    functionDefinitionVersionArn,
                  ("LoggerDefinitionVersionArn" Core..=) Core.<$>
                    loggerDefinitionVersionArn,
                  ("ResourceDefinitionVersionArn" Core..=) Core.<$>
                    resourceDefinitionVersionArn,
                  ("SubscriptionDefinitionVersionArn" Core..=) Core.<$>
                    subscriptionDefinitionVersionArn])

instance Core.FromJSON GroupVersion where
        parseJSON
          = Core.withObject "GroupVersion" Core.$
              \ x ->
                GroupVersion' Core.<$>
                  (x Core..:? "ConnectorDefinitionVersionArn") Core.<*>
                    x Core..:? "CoreDefinitionVersionArn"
                    Core.<*> x Core..:? "DeviceDefinitionVersionArn"
                    Core.<*> x Core..:? "FunctionDefinitionVersionArn"
                    Core.<*> x Core..:? "LoggerDefinitionVersionArn"
                    Core.<*> x Core..:? "ResourceDefinitionVersionArn"
                    Core.<*> x Core..:? "SubscriptionDefinitionVersionArn"
