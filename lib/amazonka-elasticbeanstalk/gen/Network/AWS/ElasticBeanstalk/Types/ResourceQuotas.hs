{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.ResourceQuotas
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticBeanstalk.Types.ResourceQuotas
  ( ResourceQuotas (..)
  -- * Smart constructor
  , mkResourceQuotas
  -- * Lenses
  , rqApplicationQuota
  , rqApplicationVersionQuota
  , rqConfigurationTemplateQuota
  , rqCustomPlatformQuota
  , rqEnvironmentQuota
  ) where

import qualified Network.AWS.ElasticBeanstalk.Types.ResourceQuota as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A set of per-resource AWS Elastic Beanstalk quotas associated with an AWS account. They reflect Elastic Beanstalk resource limits for this account.
--
-- /See:/ 'mkResourceQuotas' smart constructor.
data ResourceQuotas = ResourceQuotas'
  { applicationQuota :: Core.Maybe Types.ResourceQuota
    -- ^ The quota for applications in the AWS account.
  , applicationVersionQuota :: Core.Maybe Types.ResourceQuota
    -- ^ The quota for application versions in the AWS account.
  , configurationTemplateQuota :: Core.Maybe Types.ResourceQuota
    -- ^ The quota for configuration templates in the AWS account.
  , customPlatformQuota :: Core.Maybe Types.ResourceQuota
    -- ^ The quota for custom platforms in the AWS account.
  , environmentQuota :: Core.Maybe Types.ResourceQuota
    -- ^ The quota for environments in the AWS account.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResourceQuotas' value with any optional fields omitted.
mkResourceQuotas
    :: ResourceQuotas
mkResourceQuotas
  = ResourceQuotas'{applicationQuota = Core.Nothing,
                    applicationVersionQuota = Core.Nothing,
                    configurationTemplateQuota = Core.Nothing,
                    customPlatformQuota = Core.Nothing,
                    environmentQuota = Core.Nothing}

-- | The quota for applications in the AWS account.
--
-- /Note:/ Consider using 'applicationQuota' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rqApplicationQuota :: Lens.Lens' ResourceQuotas (Core.Maybe Types.ResourceQuota)
rqApplicationQuota = Lens.field @"applicationQuota"
{-# INLINEABLE rqApplicationQuota #-}
{-# DEPRECATED applicationQuota "Use generic-lens or generic-optics with 'applicationQuota' instead"  #-}

-- | The quota for application versions in the AWS account.
--
-- /Note:/ Consider using 'applicationVersionQuota' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rqApplicationVersionQuota :: Lens.Lens' ResourceQuotas (Core.Maybe Types.ResourceQuota)
rqApplicationVersionQuota = Lens.field @"applicationVersionQuota"
{-# INLINEABLE rqApplicationVersionQuota #-}
{-# DEPRECATED applicationVersionQuota "Use generic-lens or generic-optics with 'applicationVersionQuota' instead"  #-}

-- | The quota for configuration templates in the AWS account.
--
-- /Note:/ Consider using 'configurationTemplateQuota' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rqConfigurationTemplateQuota :: Lens.Lens' ResourceQuotas (Core.Maybe Types.ResourceQuota)
rqConfigurationTemplateQuota = Lens.field @"configurationTemplateQuota"
{-# INLINEABLE rqConfigurationTemplateQuota #-}
{-# DEPRECATED configurationTemplateQuota "Use generic-lens or generic-optics with 'configurationTemplateQuota' instead"  #-}

-- | The quota for custom platforms in the AWS account.
--
-- /Note:/ Consider using 'customPlatformQuota' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rqCustomPlatformQuota :: Lens.Lens' ResourceQuotas (Core.Maybe Types.ResourceQuota)
rqCustomPlatformQuota = Lens.field @"customPlatformQuota"
{-# INLINEABLE rqCustomPlatformQuota #-}
{-# DEPRECATED customPlatformQuota "Use generic-lens or generic-optics with 'customPlatformQuota' instead"  #-}

-- | The quota for environments in the AWS account.
--
-- /Note:/ Consider using 'environmentQuota' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rqEnvironmentQuota :: Lens.Lens' ResourceQuotas (Core.Maybe Types.ResourceQuota)
rqEnvironmentQuota = Lens.field @"environmentQuota"
{-# INLINEABLE rqEnvironmentQuota #-}
{-# DEPRECATED environmentQuota "Use generic-lens or generic-optics with 'environmentQuota' instead"  #-}

instance Core.FromXML ResourceQuotas where
        parseXML x
          = ResourceQuotas' Core.<$>
              (x Core..@? "ApplicationQuota") Core.<*>
                x Core..@? "ApplicationVersionQuota"
                Core.<*> x Core..@? "ConfigurationTemplateQuota"
                Core.<*> x Core..@? "CustomPlatformQuota"
                Core.<*> x Core..@? "EnvironmentQuota"
