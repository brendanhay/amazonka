{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.MonitoredResourceInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.MonitoredResourceInfo
  ( MonitoredResourceInfo (..)
  -- * Smart constructor
  , mkMonitoredResourceInfo
  -- * Lenses
  , mriArn
  , mriName
  , mriResourceType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.ResourceArn as Types
import qualified Network.AWS.Lightsail.Types.ResourceName as Types
import qualified Network.AWS.Lightsail.Types.ResourceType as Types
import qualified Network.AWS.Prelude as Core

-- | Describes resource being monitored by an alarm.
--
-- An alarm is a way to monitor your Amazon Lightsail resource metrics. For more information, see <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-alarms Alarms in Amazon Lightsail> .
--
-- /See:/ 'mkMonitoredResourceInfo' smart constructor.
data MonitoredResourceInfo = MonitoredResourceInfo'
  { arn :: Core.Maybe Types.ResourceArn
    -- ^ The Amazon Resource Name (ARN) of the resource being monitored.
  , name :: Core.Maybe Types.ResourceName
    -- ^ The name of the Lightsail resource being monitored.
  , resourceType :: Core.Maybe Types.ResourceType
    -- ^ The Lightsail resource type of the resource being monitored.
--
-- Instances, load balancers, and relational databases are the only Lightsail resources that can currently be monitored by alarms.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MonitoredResourceInfo' value with any optional fields omitted.
mkMonitoredResourceInfo
    :: MonitoredResourceInfo
mkMonitoredResourceInfo
  = MonitoredResourceInfo'{arn = Core.Nothing, name = Core.Nothing,
                           resourceType = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the resource being monitored.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mriArn :: Lens.Lens' MonitoredResourceInfo (Core.Maybe Types.ResourceArn)
mriArn = Lens.field @"arn"
{-# INLINEABLE mriArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The name of the Lightsail resource being monitored.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mriName :: Lens.Lens' MonitoredResourceInfo (Core.Maybe Types.ResourceName)
mriName = Lens.field @"name"
{-# INLINEABLE mriName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The Lightsail resource type of the resource being monitored.
--
-- Instances, load balancers, and relational databases are the only Lightsail resources that can currently be monitored by alarms.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mriResourceType :: Lens.Lens' MonitoredResourceInfo (Core.Maybe Types.ResourceType)
mriResourceType = Lens.field @"resourceType"
{-# INLINEABLE mriResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

instance Core.FromJSON MonitoredResourceInfo where
        parseJSON
          = Core.withObject "MonitoredResourceInfo" Core.$
              \ x ->
                MonitoredResourceInfo' Core.<$>
                  (x Core..:? "arn") Core.<*> x Core..:? "name" Core.<*>
                    x Core..:? "resourceType"
