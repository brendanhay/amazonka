{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.EnvironmentLink
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.EnvironmentLink
  ( EnvironmentLink (..),

    -- * Smart constructor
    mkEnvironmentLink,

    -- * Lenses
    elEnvironmentName,
    elLinkName,
  )
where

import qualified Network.AWS.ElasticBeanstalk.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A link to another environment, defined in the environment's manifest. Links provide connection information in system properties that can be used to connect to another environment in the same group. See <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/environment-cfg-manifest.html Environment Manifest (env.yaml)> for details.
--
-- /See:/ 'mkEnvironmentLink' smart constructor.
data EnvironmentLink = EnvironmentLink'
  { -- | The name of the linked environment (the dependency).
    environmentName :: Core.Maybe Types.String,
    -- | The name of the link.
    linkName :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnvironmentLink' value with any optional fields omitted.
mkEnvironmentLink ::
  EnvironmentLink
mkEnvironmentLink =
  EnvironmentLink'
    { environmentName = Core.Nothing,
      linkName = Core.Nothing
    }

-- | The name of the linked environment (the dependency).
--
-- /Note:/ Consider using 'environmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
elEnvironmentName :: Lens.Lens' EnvironmentLink (Core.Maybe Types.String)
elEnvironmentName = Lens.field @"environmentName"
{-# DEPRECATED elEnvironmentName "Use generic-lens or generic-optics with 'environmentName' instead." #-}

-- | The name of the link.
--
-- /Note:/ Consider using 'linkName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
elLinkName :: Lens.Lens' EnvironmentLink (Core.Maybe Types.String)
elLinkName = Lens.field @"linkName"
{-# DEPRECATED elLinkName "Use generic-lens or generic-optics with 'linkName' instead." #-}

instance Core.FromXML EnvironmentLink where
  parseXML x =
    EnvironmentLink'
      Core.<$> (x Core..@? "EnvironmentName") Core.<*> (x Core..@? "LinkName")
