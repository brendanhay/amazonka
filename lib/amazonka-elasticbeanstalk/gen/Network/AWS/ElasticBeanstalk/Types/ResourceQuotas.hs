-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.ResourceQuotas
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.ResourceQuotas
  ( ResourceQuotas (..),

    -- * Smart constructor
    mkResourceQuotas,

    -- * Lenses
    rqApplicationQuota,
    rqCustomPlatformQuota,
    rqApplicationVersionQuota,
    rqEnvironmentQuota,
    rqConfigurationTemplateQuota,
  )
where

import Network.AWS.ElasticBeanstalk.Types.ResourceQuota
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A set of per-resource AWS Elastic Beanstalk quotas associated with an AWS account. They reflect Elastic Beanstalk resource limits for this account.
--
-- /See:/ 'mkResourceQuotas' smart constructor.
data ResourceQuotas = ResourceQuotas'
  { applicationQuota ::
      Lude.Maybe ResourceQuota,
    customPlatformQuota :: Lude.Maybe ResourceQuota,
    applicationVersionQuota :: Lude.Maybe ResourceQuota,
    environmentQuota :: Lude.Maybe ResourceQuota,
    configurationTemplateQuota :: Lude.Maybe ResourceQuota
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourceQuotas' with the minimum fields required to make a request.
--
-- * 'applicationQuota' - The quota for applications in the AWS account.
-- * 'applicationVersionQuota' - The quota for application versions in the AWS account.
-- * 'configurationTemplateQuota' - The quota for configuration templates in the AWS account.
-- * 'customPlatformQuota' - The quota for custom platforms in the AWS account.
-- * 'environmentQuota' - The quota for environments in the AWS account.
mkResourceQuotas ::
  ResourceQuotas
mkResourceQuotas =
  ResourceQuotas'
    { applicationQuota = Lude.Nothing,
      customPlatformQuota = Lude.Nothing,
      applicationVersionQuota = Lude.Nothing,
      environmentQuota = Lude.Nothing,
      configurationTemplateQuota = Lude.Nothing
    }

-- | The quota for applications in the AWS account.
--
-- /Note:/ Consider using 'applicationQuota' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rqApplicationQuota :: Lens.Lens' ResourceQuotas (Lude.Maybe ResourceQuota)
rqApplicationQuota = Lens.lens (applicationQuota :: ResourceQuotas -> Lude.Maybe ResourceQuota) (\s a -> s {applicationQuota = a} :: ResourceQuotas)
{-# DEPRECATED rqApplicationQuota "Use generic-lens or generic-optics with 'applicationQuota' instead." #-}

-- | The quota for custom platforms in the AWS account.
--
-- /Note:/ Consider using 'customPlatformQuota' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rqCustomPlatformQuota :: Lens.Lens' ResourceQuotas (Lude.Maybe ResourceQuota)
rqCustomPlatformQuota = Lens.lens (customPlatformQuota :: ResourceQuotas -> Lude.Maybe ResourceQuota) (\s a -> s {customPlatformQuota = a} :: ResourceQuotas)
{-# DEPRECATED rqCustomPlatformQuota "Use generic-lens or generic-optics with 'customPlatformQuota' instead." #-}

-- | The quota for application versions in the AWS account.
--
-- /Note:/ Consider using 'applicationVersionQuota' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rqApplicationVersionQuota :: Lens.Lens' ResourceQuotas (Lude.Maybe ResourceQuota)
rqApplicationVersionQuota = Lens.lens (applicationVersionQuota :: ResourceQuotas -> Lude.Maybe ResourceQuota) (\s a -> s {applicationVersionQuota = a} :: ResourceQuotas)
{-# DEPRECATED rqApplicationVersionQuota "Use generic-lens or generic-optics with 'applicationVersionQuota' instead." #-}

-- | The quota for environments in the AWS account.
--
-- /Note:/ Consider using 'environmentQuota' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rqEnvironmentQuota :: Lens.Lens' ResourceQuotas (Lude.Maybe ResourceQuota)
rqEnvironmentQuota = Lens.lens (environmentQuota :: ResourceQuotas -> Lude.Maybe ResourceQuota) (\s a -> s {environmentQuota = a} :: ResourceQuotas)
{-# DEPRECATED rqEnvironmentQuota "Use generic-lens or generic-optics with 'environmentQuota' instead." #-}

-- | The quota for configuration templates in the AWS account.
--
-- /Note:/ Consider using 'configurationTemplateQuota' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rqConfigurationTemplateQuota :: Lens.Lens' ResourceQuotas (Lude.Maybe ResourceQuota)
rqConfigurationTemplateQuota = Lens.lens (configurationTemplateQuota :: ResourceQuotas -> Lude.Maybe ResourceQuota) (\s a -> s {configurationTemplateQuota = a} :: ResourceQuotas)
{-# DEPRECATED rqConfigurationTemplateQuota "Use generic-lens or generic-optics with 'configurationTemplateQuota' instead." #-}

instance Lude.FromXML ResourceQuotas where
  parseXML x =
    ResourceQuotas'
      Lude.<$> (x Lude..@? "ApplicationQuota")
      Lude.<*> (x Lude..@? "CustomPlatformQuota")
      Lude.<*> (x Lude..@? "ApplicationVersionQuota")
      Lude.<*> (x Lude..@? "EnvironmentQuota")
      Lude.<*> (x Lude..@? "ConfigurationTemplateQuota")
