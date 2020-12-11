-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.ApplicationResourceLifecycleConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.ApplicationResourceLifecycleConfig
  ( ApplicationResourceLifecycleConfig (..),

    -- * Smart constructor
    mkApplicationResourceLifecycleConfig,

    -- * Lenses
    arlcVersionLifecycleConfig,
    arlcServiceRole,
  )
where

import Network.AWS.ElasticBeanstalk.Types.ApplicationVersionLifecycleConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The resource lifecycle configuration for an application. Defines lifecycle settings for resources that belong to the application, and the service role that AWS Elastic Beanstalk assumes in order to apply lifecycle settings. The version lifecycle configuration defines lifecycle settings for application versions.
--
-- /See:/ 'mkApplicationResourceLifecycleConfig' smart constructor.
data ApplicationResourceLifecycleConfig = ApplicationResourceLifecycleConfig'
  { versionLifecycleConfig ::
      Lude.Maybe
        ApplicationVersionLifecycleConfig,
    serviceRole ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ApplicationResourceLifecycleConfig' with the minimum fields required to make a request.
--
-- * 'serviceRole' - The ARN of an IAM service role that Elastic Beanstalk has permission to assume.
--
-- The @ServiceRole@ property is required the first time that you provide a @VersionLifecycleConfig@ for the application in one of the supporting calls (@CreateApplication@ or @UpdateApplicationResourceLifecycle@ ). After you provide it once, in either one of the calls, Elastic Beanstalk persists the Service Role with the application, and you don't need to specify it again in subsequent @UpdateApplicationResourceLifecycle@ calls. You can, however, specify it in subsequent calls to change the Service Role to another value.
-- * 'versionLifecycleConfig' - Defines lifecycle settings for application versions.
mkApplicationResourceLifecycleConfig ::
  ApplicationResourceLifecycleConfig
mkApplicationResourceLifecycleConfig =
  ApplicationResourceLifecycleConfig'
    { versionLifecycleConfig =
        Lude.Nothing,
      serviceRole = Lude.Nothing
    }

-- | Defines lifecycle settings for application versions.
--
-- /Note:/ Consider using 'versionLifecycleConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arlcVersionLifecycleConfig :: Lens.Lens' ApplicationResourceLifecycleConfig (Lude.Maybe ApplicationVersionLifecycleConfig)
arlcVersionLifecycleConfig = Lens.lens (versionLifecycleConfig :: ApplicationResourceLifecycleConfig -> Lude.Maybe ApplicationVersionLifecycleConfig) (\s a -> s {versionLifecycleConfig = a} :: ApplicationResourceLifecycleConfig)
{-# DEPRECATED arlcVersionLifecycleConfig "Use generic-lens or generic-optics with 'versionLifecycleConfig' instead." #-}

-- | The ARN of an IAM service role that Elastic Beanstalk has permission to assume.
--
-- The @ServiceRole@ property is required the first time that you provide a @VersionLifecycleConfig@ for the application in one of the supporting calls (@CreateApplication@ or @UpdateApplicationResourceLifecycle@ ). After you provide it once, in either one of the calls, Elastic Beanstalk persists the Service Role with the application, and you don't need to specify it again in subsequent @UpdateApplicationResourceLifecycle@ calls. You can, however, specify it in subsequent calls to change the Service Role to another value.
--
-- /Note:/ Consider using 'serviceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arlcServiceRole :: Lens.Lens' ApplicationResourceLifecycleConfig (Lude.Maybe Lude.Text)
arlcServiceRole = Lens.lens (serviceRole :: ApplicationResourceLifecycleConfig -> Lude.Maybe Lude.Text) (\s a -> s {serviceRole = a} :: ApplicationResourceLifecycleConfig)
{-# DEPRECATED arlcServiceRole "Use generic-lens or generic-optics with 'serviceRole' instead." #-}

instance Lude.FromXML ApplicationResourceLifecycleConfig where
  parseXML x =
    ApplicationResourceLifecycleConfig'
      Lude.<$> (x Lude..@? "VersionLifecycleConfig")
      Lude.<*> (x Lude..@? "ServiceRole")

instance Lude.ToQuery ApplicationResourceLifecycleConfig where
  toQuery ApplicationResourceLifecycleConfig' {..} =
    Lude.mconcat
      [ "VersionLifecycleConfig" Lude.=: versionLifecycleConfig,
        "ServiceRole" Lude.=: serviceRole
      ]
