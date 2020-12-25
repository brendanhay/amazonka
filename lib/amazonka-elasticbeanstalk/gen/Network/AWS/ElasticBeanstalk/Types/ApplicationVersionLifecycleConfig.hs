{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.ApplicationVersionLifecycleConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.ApplicationVersionLifecycleConfig
  ( ApplicationVersionLifecycleConfig (..),

    -- * Smart constructor
    mkApplicationVersionLifecycleConfig,

    -- * Lenses
    avlcMaxAgeRule,
    avlcMaxCountRule,
  )
where

import qualified Network.AWS.ElasticBeanstalk.Types.MaxAgeRule as Types
import qualified Network.AWS.ElasticBeanstalk.Types.MaxCountRule as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The application version lifecycle settings for an application. Defines the rules that Elastic Beanstalk applies to an application's versions in order to avoid hitting the per-region limit for application versions.
--
-- When Elastic Beanstalk deletes an application version from its database, you can no longer deploy that version to an environment. The source bundle remains in S3 unless you configure the rule to delete it.
--
-- /See:/ 'mkApplicationVersionLifecycleConfig' smart constructor.
data ApplicationVersionLifecycleConfig = ApplicationVersionLifecycleConfig'
  { -- | Specify a max age rule to restrict the length of time that application versions are retained for an application.
    maxAgeRule :: Core.Maybe Types.MaxAgeRule,
    -- | Specify a max count rule to restrict the number of application versions that are retained for an application.
    maxCountRule :: Core.Maybe Types.MaxCountRule
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ApplicationVersionLifecycleConfig' value with any optional fields omitted.
mkApplicationVersionLifecycleConfig ::
  ApplicationVersionLifecycleConfig
mkApplicationVersionLifecycleConfig =
  ApplicationVersionLifecycleConfig'
    { maxAgeRule = Core.Nothing,
      maxCountRule = Core.Nothing
    }

-- | Specify a max age rule to restrict the length of time that application versions are retained for an application.
--
-- /Note:/ Consider using 'maxAgeRule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avlcMaxAgeRule :: Lens.Lens' ApplicationVersionLifecycleConfig (Core.Maybe Types.MaxAgeRule)
avlcMaxAgeRule = Lens.field @"maxAgeRule"
{-# DEPRECATED avlcMaxAgeRule "Use generic-lens or generic-optics with 'maxAgeRule' instead." #-}

-- | Specify a max count rule to restrict the number of application versions that are retained for an application.
--
-- /Note:/ Consider using 'maxCountRule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avlcMaxCountRule :: Lens.Lens' ApplicationVersionLifecycleConfig (Core.Maybe Types.MaxCountRule)
avlcMaxCountRule = Lens.field @"maxCountRule"
{-# DEPRECATED avlcMaxCountRule "Use generic-lens or generic-optics with 'maxCountRule' instead." #-}

instance Core.FromXML ApplicationVersionLifecycleConfig where
  parseXML x =
    ApplicationVersionLifecycleConfig'
      Core.<$> (x Core..@? "MaxAgeRule") Core.<*> (x Core..@? "MaxCountRule")
