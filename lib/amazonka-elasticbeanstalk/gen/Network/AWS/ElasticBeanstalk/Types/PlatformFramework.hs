{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.PlatformFramework
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticBeanstalk.Types.PlatformFramework
  ( PlatformFramework (..)
  -- * Smart constructor
  , mkPlatformFramework
  -- * Lenses
  , pfName
  , pfVersion
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A framework supported by the platform.
--
-- /See:/ 'mkPlatformFramework' smart constructor.
data PlatformFramework = PlatformFramework'
  { name :: Core.Maybe Core.Text
    -- ^ The name of the framework.
  , version :: Core.Maybe Core.Text
    -- ^ The version of the framework.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PlatformFramework' value with any optional fields omitted.
mkPlatformFramework
    :: PlatformFramework
mkPlatformFramework
  = PlatformFramework'{name = Core.Nothing, version = Core.Nothing}

-- | The name of the framework.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfName :: Lens.Lens' PlatformFramework (Core.Maybe Core.Text)
pfName = Lens.field @"name"
{-# INLINEABLE pfName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The version of the framework.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfVersion :: Lens.Lens' PlatformFramework (Core.Maybe Core.Text)
pfVersion = Lens.field @"version"
{-# INLINEABLE pfVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

instance Core.FromXML PlatformFramework where
        parseXML x
          = PlatformFramework' Core.<$>
              (x Core..@? "Name") Core.<*> x Core..@? "Version"
