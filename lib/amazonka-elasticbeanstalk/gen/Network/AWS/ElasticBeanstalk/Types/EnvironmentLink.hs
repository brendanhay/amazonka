{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.EnvironmentLink
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticBeanstalk.Types.EnvironmentLink
  ( EnvironmentLink (..)
  -- * Smart constructor
  , mkEnvironmentLink
  -- * Lenses
  , elEnvironmentName
  , elLinkName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A link to another environment, defined in the environment's manifest. Links provide connection information in system properties that can be used to connect to another environment in the same group. See <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/environment-cfg-manifest.html Environment Manifest (env.yaml)> for details.
--
-- /See:/ 'mkEnvironmentLink' smart constructor.
data EnvironmentLink = EnvironmentLink'
  { environmentName :: Core.Maybe Core.Text
    -- ^ The name of the linked environment (the dependency).
  , linkName :: Core.Maybe Core.Text
    -- ^ The name of the link.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnvironmentLink' value with any optional fields omitted.
mkEnvironmentLink
    :: EnvironmentLink
mkEnvironmentLink
  = EnvironmentLink'{environmentName = Core.Nothing,
                     linkName = Core.Nothing}

-- | The name of the linked environment (the dependency).
--
-- /Note:/ Consider using 'environmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
elEnvironmentName :: Lens.Lens' EnvironmentLink (Core.Maybe Core.Text)
elEnvironmentName = Lens.field @"environmentName"
{-# INLINEABLE elEnvironmentName #-}
{-# DEPRECATED environmentName "Use generic-lens or generic-optics with 'environmentName' instead"  #-}

-- | The name of the link.
--
-- /Note:/ Consider using 'linkName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
elLinkName :: Lens.Lens' EnvironmentLink (Core.Maybe Core.Text)
elLinkName = Lens.field @"linkName"
{-# INLINEABLE elLinkName #-}
{-# DEPRECATED linkName "Use generic-lens or generic-optics with 'linkName' instead"  #-}

instance Core.FromXML EnvironmentLink where
        parseXML x
          = EnvironmentLink' Core.<$>
              (x Core..@? "EnvironmentName") Core.<*> x Core..@? "LinkName"
