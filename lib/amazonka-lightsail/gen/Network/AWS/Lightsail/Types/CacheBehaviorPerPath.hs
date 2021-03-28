{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.CacheBehaviorPerPath
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.CacheBehaviorPerPath
  ( CacheBehaviorPerPath (..)
  -- * Smart constructor
  , mkCacheBehaviorPerPath
  -- * Lenses
  , cbppBehavior
  , cbppPath
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.BehaviorEnum as Types
import qualified Network.AWS.Prelude as Core

-- | Describes the per-path cache behavior of an Amazon Lightsail content delivery network (CDN) distribution.
--
-- A per-path cache behavior is used to override, or add an exception to, the default cache behavior of a distribution. For example, if the @cacheBehavior@ is set to @cache@ , then a per-path cache behavior can be used to specify a directory, file, or file type that your distribution will cache. Alternately, if the distribution's @cacheBehavior@ is @dont-cache@ , then a per-path cache behavior can be used to specify a directory, file, or file type that your distribution will not cache.
-- if the cacheBehavior's behavior is set to 'cache', then
--
-- /See:/ 'mkCacheBehaviorPerPath' smart constructor.
data CacheBehaviorPerPath = CacheBehaviorPerPath'
  { behavior :: Core.Maybe Types.BehaviorEnum
    -- ^ The cache behavior for the specified path.
--
-- You can specify one of the following per-path cache behaviors:
--
--     * __@cache@ __ - This behavior caches the specified path. 
--
--
--     * __@dont-cache@ __ - This behavior doesn't cache the specified path. 
--
--
  , path :: Core.Maybe Core.Text
    -- ^ The path to a directory or file to cached, or not cache. Use an asterisk symbol to specify wildcard directories (@path/to/assets/*@ ), and file types (@*.html, *jpg, *js@ ). Directories and file paths are case-sensitive.
--
-- Examples:
--
--     * Specify the following to cache all files in the document root of an Apache web server running on a Lightsail instance.
-- @var/www/html/@ 
--
--
--     * Specify the following file to cache only the index page in the document root of an Apache web server.
-- @var/www/html/index.html@ 
--
--
--     * Specify the following to cache only the .html files in the document root of an Apache web server.
-- @var/www/html/*.html@ 
--
--
--     * Specify the following to cache only the .jpg, .png, and .gif files in the images sub-directory of the document root of an Apache web server.
-- @var/www/html/images/*.jpg@ 
-- @var/www/html/images/*.png@ 
-- @var/www/html/images/*.gif@ 
-- Specify the following to cache all files in the images sub-directory of the document root of an Apache web server.
-- @var/www/html/images/@ 
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CacheBehaviorPerPath' value with any optional fields omitted.
mkCacheBehaviorPerPath
    :: CacheBehaviorPerPath
mkCacheBehaviorPerPath
  = CacheBehaviorPerPath'{behavior = Core.Nothing,
                          path = Core.Nothing}

-- | The cache behavior for the specified path.
--
-- You can specify one of the following per-path cache behaviors:
--
--     * __@cache@ __ - This behavior caches the specified path. 
--
--
--     * __@dont-cache@ __ - This behavior doesn't cache the specified path. 
--
--
--
-- /Note:/ Consider using 'behavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbppBehavior :: Lens.Lens' CacheBehaviorPerPath (Core.Maybe Types.BehaviorEnum)
cbppBehavior = Lens.field @"behavior"
{-# INLINEABLE cbppBehavior #-}
{-# DEPRECATED behavior "Use generic-lens or generic-optics with 'behavior' instead"  #-}

-- | The path to a directory or file to cached, or not cache. Use an asterisk symbol to specify wildcard directories (@path/to/assets/*@ ), and file types (@*.html, *jpg, *js@ ). Directories and file paths are case-sensitive.
--
-- Examples:
--
--     * Specify the following to cache all files in the document root of an Apache web server running on a Lightsail instance.
-- @var/www/html/@ 
--
--
--     * Specify the following file to cache only the index page in the document root of an Apache web server.
-- @var/www/html/index.html@ 
--
--
--     * Specify the following to cache only the .html files in the document root of an Apache web server.
-- @var/www/html/*.html@ 
--
--
--     * Specify the following to cache only the .jpg, .png, and .gif files in the images sub-directory of the document root of an Apache web server.
-- @var/www/html/images/*.jpg@ 
-- @var/www/html/images/*.png@ 
-- @var/www/html/images/*.gif@ 
-- Specify the following to cache all files in the images sub-directory of the document root of an Apache web server.
-- @var/www/html/images/@ 
--
--
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbppPath :: Lens.Lens' CacheBehaviorPerPath (Core.Maybe Core.Text)
cbppPath = Lens.field @"path"
{-# INLINEABLE cbppPath #-}
{-# DEPRECATED path "Use generic-lens or generic-optics with 'path' instead"  #-}

instance Core.FromJSON CacheBehaviorPerPath where
        toJSON CacheBehaviorPerPath{..}
          = Core.object
              (Core.catMaybes
                 [("behavior" Core..=) Core.<$> behavior,
                  ("path" Core..=) Core.<$> path])

instance Core.FromJSON CacheBehaviorPerPath where
        parseJSON
          = Core.withObject "CacheBehaviorPerPath" Core.$
              \ x ->
                CacheBehaviorPerPath' Core.<$>
                  (x Core..:? "behavior") Core.<*> x Core..:? "path"
