{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.ApplicationResourceLifecycleConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticBeanstalk.Types.ApplicationResourceLifecycleConfig
  ( ApplicationResourceLifecycleConfig (..)
  -- * Smart constructor
  , mkApplicationResourceLifecycleConfig
  -- * Lenses
  , arlcServiceRole
  , arlcVersionLifecycleConfig
  ) where

import qualified Network.AWS.ElasticBeanstalk.Types.ApplicationVersionLifecycleConfig as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The resource lifecycle configuration for an application. Defines lifecycle settings for resources that belong to the application, and the service role that AWS Elastic Beanstalk assumes in order to apply lifecycle settings. The version lifecycle configuration defines lifecycle settings for application versions.
--
-- /See:/ 'mkApplicationResourceLifecycleConfig' smart constructor.
data ApplicationResourceLifecycleConfig = ApplicationResourceLifecycleConfig'
  { serviceRole :: Core.Maybe Core.Text
    -- ^ The ARN of an IAM service role that Elastic Beanstalk has permission to assume.
--
-- The @ServiceRole@ property is required the first time that you provide a @VersionLifecycleConfig@ for the application in one of the supporting calls (@CreateApplication@ or @UpdateApplicationResourceLifecycle@ ). After you provide it once, in either one of the calls, Elastic Beanstalk persists the Service Role with the application, and you don't need to specify it again in subsequent @UpdateApplicationResourceLifecycle@ calls. You can, however, specify it in subsequent calls to change the Service Role to another value.
  , versionLifecycleConfig :: Core.Maybe Types.ApplicationVersionLifecycleConfig
    -- ^ Defines lifecycle settings for application versions.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ApplicationResourceLifecycleConfig' value with any optional fields omitted.
mkApplicationResourceLifecycleConfig
    :: ApplicationResourceLifecycleConfig
mkApplicationResourceLifecycleConfig
  = ApplicationResourceLifecycleConfig'{serviceRole = Core.Nothing,
                                        versionLifecycleConfig = Core.Nothing}

-- | The ARN of an IAM service role that Elastic Beanstalk has permission to assume.
--
-- The @ServiceRole@ property is required the first time that you provide a @VersionLifecycleConfig@ for the application in one of the supporting calls (@CreateApplication@ or @UpdateApplicationResourceLifecycle@ ). After you provide it once, in either one of the calls, Elastic Beanstalk persists the Service Role with the application, and you don't need to specify it again in subsequent @UpdateApplicationResourceLifecycle@ calls. You can, however, specify it in subsequent calls to change the Service Role to another value.
--
-- /Note:/ Consider using 'serviceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arlcServiceRole :: Lens.Lens' ApplicationResourceLifecycleConfig (Core.Maybe Core.Text)
arlcServiceRole = Lens.field @"serviceRole"
{-# INLINEABLE arlcServiceRole #-}
{-# DEPRECATED serviceRole "Use generic-lens or generic-optics with 'serviceRole' instead"  #-}

-- | Defines lifecycle settings for application versions.
--
-- /Note:/ Consider using 'versionLifecycleConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arlcVersionLifecycleConfig :: Lens.Lens' ApplicationResourceLifecycleConfig (Core.Maybe Types.ApplicationVersionLifecycleConfig)
arlcVersionLifecycleConfig = Lens.field @"versionLifecycleConfig"
{-# INLINEABLE arlcVersionLifecycleConfig #-}
{-# DEPRECATED versionLifecycleConfig "Use generic-lens or generic-optics with 'versionLifecycleConfig' instead"  #-}

instance Core.ToQuery ApplicationResourceLifecycleConfig where
        toQuery ApplicationResourceLifecycleConfig{..}
          = Core.maybe Core.mempty (Core.toQueryPair "ServiceRole")
              serviceRole
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "VersionLifecycleConfig")
                versionLifecycleConfig

instance Core.FromXML ApplicationResourceLifecycleConfig where
        parseXML x
          = ApplicationResourceLifecycleConfig' Core.<$>
              (x Core..@? "ServiceRole") Core.<*>
                x Core..@? "VersionLifecycleConfig"
