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

import Network.AWS.ElasticBeanstalk.Types.MaxAgeRule
import Network.AWS.ElasticBeanstalk.Types.MaxCountRule
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The application version lifecycle settings for an application. Defines the rules that Elastic Beanstalk applies to an application's versions in order to avoid hitting the per-region limit for application versions.
--
-- When Elastic Beanstalk deletes an application version from its database, you can no longer deploy that version to an environment. The source bundle remains in S3 unless you configure the rule to delete it.
--
-- /See:/ 'mkApplicationVersionLifecycleConfig' smart constructor.
data ApplicationVersionLifecycleConfig = ApplicationVersionLifecycleConfig'
  { -- | Specify a max age rule to restrict the length of time that application versions are retained for an application.
    maxAgeRule :: Lude.Maybe MaxAgeRule,
    -- | Specify a max count rule to restrict the number of application versions that are retained for an application.
    maxCountRule :: Lude.Maybe MaxCountRule
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ApplicationVersionLifecycleConfig' with the minimum fields required to make a request.
--
-- * 'maxAgeRule' - Specify a max age rule to restrict the length of time that application versions are retained for an application.
-- * 'maxCountRule' - Specify a max count rule to restrict the number of application versions that are retained for an application.
mkApplicationVersionLifecycleConfig ::
  ApplicationVersionLifecycleConfig
mkApplicationVersionLifecycleConfig =
  ApplicationVersionLifecycleConfig'
    { maxAgeRule = Lude.Nothing,
      maxCountRule = Lude.Nothing
    }

-- | Specify a max age rule to restrict the length of time that application versions are retained for an application.
--
-- /Note:/ Consider using 'maxAgeRule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avlcMaxAgeRule :: Lens.Lens' ApplicationVersionLifecycleConfig (Lude.Maybe MaxAgeRule)
avlcMaxAgeRule = Lens.lens (maxAgeRule :: ApplicationVersionLifecycleConfig -> Lude.Maybe MaxAgeRule) (\s a -> s {maxAgeRule = a} :: ApplicationVersionLifecycleConfig)
{-# DEPRECATED avlcMaxAgeRule "Use generic-lens or generic-optics with 'maxAgeRule' instead." #-}

-- | Specify a max count rule to restrict the number of application versions that are retained for an application.
--
-- /Note:/ Consider using 'maxCountRule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avlcMaxCountRule :: Lens.Lens' ApplicationVersionLifecycleConfig (Lude.Maybe MaxCountRule)
avlcMaxCountRule = Lens.lens (maxCountRule :: ApplicationVersionLifecycleConfig -> Lude.Maybe MaxCountRule) (\s a -> s {maxCountRule = a} :: ApplicationVersionLifecycleConfig)
{-# DEPRECATED avlcMaxCountRule "Use generic-lens or generic-optics with 'maxCountRule' instead." #-}

instance Lude.FromXML ApplicationVersionLifecycleConfig where
  parseXML x =
    ApplicationVersionLifecycleConfig'
      Lude.<$> (x Lude..@? "MaxAgeRule") Lude.<*> (x Lude..@? "MaxCountRule")

instance Lude.ToQuery ApplicationVersionLifecycleConfig where
  toQuery ApplicationVersionLifecycleConfig' {..} =
    Lude.mconcat
      [ "MaxAgeRule" Lude.=: maxAgeRule,
        "MaxCountRule" Lude.=: maxCountRule
      ]
