{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.CacheBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.CacheBehavior
  ( CacheBehavior (..)
  -- * Smart constructor
  , mkCacheBehavior
  -- * Lenses
  , cbBehavior
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.BehaviorEnum as Types
import qualified Network.AWS.Prelude as Core

-- | Describes the default cache behavior of an Amazon Lightsail content delivery network (CDN) distribution.
--
-- /See:/ 'mkCacheBehavior' smart constructor.
newtype CacheBehavior = CacheBehavior'
  { behavior :: Core.Maybe Types.BehaviorEnum
    -- ^ The cache behavior of the distribution.
--
-- The following cache behaviors can be specified:
--
--     * __@cache@ __ - This option is best for static sites. When specified, your distribution caches and serves your entire website as static content. This behavior is ideal for websites with static content that doesn't change depending on who views it, or for websites that don't use cookies, headers, or query strings to personalize content.
--
--
--     * __@dont-cache@ __ - This option is best for sites that serve a mix of static and dynamic content. When specified, your distribution caches and serve only the content that is specified in the distribution's @CacheBehaviorPerPath@ parameter. This behavior is ideal for websites or web applications that use cookies, headers, and query strings to personalize content for individual users.
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CacheBehavior' value with any optional fields omitted.
mkCacheBehavior
    :: CacheBehavior
mkCacheBehavior = CacheBehavior'{behavior = Core.Nothing}

-- | The cache behavior of the distribution.
--
-- The following cache behaviors can be specified:
--
--     * __@cache@ __ - This option is best for static sites. When specified, your distribution caches and serves your entire website as static content. This behavior is ideal for websites with static content that doesn't change depending on who views it, or for websites that don't use cookies, headers, or query strings to personalize content.
--
--
--     * __@dont-cache@ __ - This option is best for sites that serve a mix of static and dynamic content. When specified, your distribution caches and serve only the content that is specified in the distribution's @CacheBehaviorPerPath@ parameter. This behavior is ideal for websites or web applications that use cookies, headers, and query strings to personalize content for individual users.
--
--
--
-- /Note:/ Consider using 'behavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbBehavior :: Lens.Lens' CacheBehavior (Core.Maybe Types.BehaviorEnum)
cbBehavior = Lens.field @"behavior"
{-# INLINEABLE cbBehavior #-}
{-# DEPRECATED behavior "Use generic-lens or generic-optics with 'behavior' instead"  #-}

instance Core.FromJSON CacheBehavior where
        toJSON CacheBehavior{..}
          = Core.object
              (Core.catMaybes [("behavior" Core..=) Core.<$> behavior])

instance Core.FromJSON CacheBehavior where
        parseJSON
          = Core.withObject "CacheBehavior" Core.$
              \ x -> CacheBehavior' Core.<$> (x Core..:? "behavior")
