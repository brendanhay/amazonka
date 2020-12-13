{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.CacheBehaviorPerPath
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.CacheBehaviorPerPath
  ( CacheBehaviorPerPath (..),

    -- * Smart constructor
    mkCacheBehaviorPerPath,

    -- * Lenses
    cbppPath,
    cbppBehavior,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.BehaviorEnum
import qualified Network.AWS.Prelude as Lude

-- | Describes the per-path cache behavior of an Amazon Lightsail content delivery network (CDN) distribution.
--
-- A per-path cache behavior is used to override, or add an exception to, the default cache behavior of a distribution. For example, if the @cacheBehavior@ is set to @cache@ , then a per-path cache behavior can be used to specify a directory, file, or file type that your distribution will cache. Alternately, if the distribution's @cacheBehavior@ is @dont-cache@ , then a per-path cache behavior can be used to specify a directory, file, or file type that your distribution will not cache.
-- if the cacheBehavior's behavior is set to 'cache', then
--
-- /See:/ 'mkCacheBehaviorPerPath' smart constructor.
data CacheBehaviorPerPath = CacheBehaviorPerPath'
  { -- | The path to a directory or file to cached, or not cache. Use an asterisk symbol to specify wildcard directories (@path/to/assets/*@ ), and file types (@*.html, *jpg, *js@ ). Directories and file paths are case-sensitive.
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
    path :: Lude.Maybe Lude.Text,
    -- | The cache behavior for the specified path.
    --
    -- You can specify one of the following per-path cache behaviors:
    --
    --     * __@cache@ __ - This behavior caches the specified path.
    --
    --
    --     * __@dont-cache@ __ - This behavior doesn't cache the specified path.
    behavior :: Lude.Maybe BehaviorEnum
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CacheBehaviorPerPath' with the minimum fields required to make a request.
--
-- * 'path' - The path to a directory or file to cached, or not cache. Use an asterisk symbol to specify wildcard directories (@path/to/assets/*@ ), and file types (@*.html, *jpg, *js@ ). Directories and file paths are case-sensitive.
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
-- * 'behavior' - The cache behavior for the specified path.
--
-- You can specify one of the following per-path cache behaviors:
--
--     * __@cache@ __ - This behavior caches the specified path.
--
--
--     * __@dont-cache@ __ - This behavior doesn't cache the specified path.
mkCacheBehaviorPerPath ::
  CacheBehaviorPerPath
mkCacheBehaviorPerPath =
  CacheBehaviorPerPath'
    { path = Lude.Nothing,
      behavior = Lude.Nothing
    }

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
cbppPath :: Lens.Lens' CacheBehaviorPerPath (Lude.Maybe Lude.Text)
cbppPath = Lens.lens (path :: CacheBehaviorPerPath -> Lude.Maybe Lude.Text) (\s a -> s {path = a} :: CacheBehaviorPerPath)
{-# DEPRECATED cbppPath "Use generic-lens or generic-optics with 'path' instead." #-}

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
cbppBehavior :: Lens.Lens' CacheBehaviorPerPath (Lude.Maybe BehaviorEnum)
cbppBehavior = Lens.lens (behavior :: CacheBehaviorPerPath -> Lude.Maybe BehaviorEnum) (\s a -> s {behavior = a} :: CacheBehaviorPerPath)
{-# DEPRECATED cbppBehavior "Use generic-lens or generic-optics with 'behavior' instead." #-}

instance Lude.FromJSON CacheBehaviorPerPath where
  parseJSON =
    Lude.withObject
      "CacheBehaviorPerPath"
      ( \x ->
          CacheBehaviorPerPath'
            Lude.<$> (x Lude..:? "path") Lude.<*> (x Lude..:? "behavior")
      )

instance Lude.ToJSON CacheBehaviorPerPath where
  toJSON CacheBehaviorPerPath' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("path" Lude..=) Lude.<$> path,
            ("behavior" Lude..=) Lude.<$> behavior
          ]
      )
