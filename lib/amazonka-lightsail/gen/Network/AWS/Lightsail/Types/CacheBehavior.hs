{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.CacheBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.CacheBehavior
  ( CacheBehavior (..),

    -- * Smart constructor
    mkCacheBehavior,

    -- * Lenses
    cbBehavior,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.BehaviorEnum
import qualified Network.AWS.Prelude as Lude

-- | Describes the default cache behavior of an Amazon Lightsail content delivery network (CDN) distribution.
--
-- /See:/ 'mkCacheBehavior' smart constructor.
newtype CacheBehavior = CacheBehavior'
  { -- | The cache behavior of the distribution.
    --
    -- The following cache behaviors can be specified:
    --
    --     * __@cache@ __ - This option is best for static sites. When specified, your distribution caches and serves your entire website as static content. This behavior is ideal for websites with static content that doesn't change depending on who views it, or for websites that don't use cookies, headers, or query strings to personalize content.
    --
    --
    --     * __@dont-cache@ __ - This option is best for sites that serve a mix of static and dynamic content. When specified, your distribution caches and serve only the content that is specified in the distribution's @CacheBehaviorPerPath@ parameter. This behavior is ideal for websites or web applications that use cookies, headers, and query strings to personalize content for individual users.
    behavior :: Lude.Maybe BehaviorEnum
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CacheBehavior' with the minimum fields required to make a request.
--
-- * 'behavior' - The cache behavior of the distribution.
--
-- The following cache behaviors can be specified:
--
--     * __@cache@ __ - This option is best for static sites. When specified, your distribution caches and serves your entire website as static content. This behavior is ideal for websites with static content that doesn't change depending on who views it, or for websites that don't use cookies, headers, or query strings to personalize content.
--
--
--     * __@dont-cache@ __ - This option is best for sites that serve a mix of static and dynamic content. When specified, your distribution caches and serve only the content that is specified in the distribution's @CacheBehaviorPerPath@ parameter. This behavior is ideal for websites or web applications that use cookies, headers, and query strings to personalize content for individual users.
mkCacheBehavior ::
  CacheBehavior
mkCacheBehavior = CacheBehavior' {behavior = Lude.Nothing}

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
cbBehavior :: Lens.Lens' CacheBehavior (Lude.Maybe BehaviorEnum)
cbBehavior = Lens.lens (behavior :: CacheBehavior -> Lude.Maybe BehaviorEnum) (\s a -> s {behavior = a} :: CacheBehavior)
{-# DEPRECATED cbBehavior "Use generic-lens or generic-optics with 'behavior' instead." #-}

instance Lude.FromJSON CacheBehavior where
  parseJSON =
    Lude.withObject
      "CacheBehavior"
      (\x -> CacheBehavior' Lude.<$> (x Lude..:? "behavior"))

instance Lude.ToJSON CacheBehavior where
  toJSON CacheBehavior' {..} =
    Lude.object
      (Lude.catMaybes [("behavior" Lude..=) Lude.<$> behavior])
