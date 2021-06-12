{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.CacheBehaviorPerPath
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.CacheBehaviorPerPath where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.BehaviorEnum

-- | Describes the per-path cache behavior of an Amazon Lightsail content
-- delivery network (CDN) distribution.
--
-- A per-path cache behavior is used to override, or add an exception to,
-- the default cache behavior of a distribution. For example, if the
-- @cacheBehavior@ is set to @cache@, then a per-path cache behavior can be
-- used to specify a directory, file, or file type that your distribution
-- will cache. Alternately, if the distribution\'s @cacheBehavior@ is
-- @dont-cache@, then a per-path cache behavior can be used to specify a
-- directory, file, or file type that your distribution will not cache.
--
-- if the cacheBehavior\'s behavior is set to \'cache\', then
--
-- /See:/ 'newCacheBehaviorPerPath' smart constructor.
data CacheBehaviorPerPath = CacheBehaviorPerPath'
  { -- | The cache behavior for the specified path.
    --
    -- You can specify one of the following per-path cache behaviors:
    --
    -- -   __@cache@__ - This behavior caches the specified path.
    --
    -- -   __@dont-cache@__ - This behavior doesn\'t cache the specified path.
    behavior :: Core.Maybe BehaviorEnum,
    -- | The path to a directory or file to cached, or not cache. Use an asterisk
    -- symbol to specify wildcard directories (@path\/to\/assets\/*@), and file
    -- types (@*.html, *jpg, *js@). Directories and file paths are
    -- case-sensitive.
    --
    -- Examples:
    --
    -- -   Specify the following to cache all files in the document root of an
    --     Apache web server running on a Lightsail instance.
    --
    --     @var\/www\/html\/@
    --
    -- -   Specify the following file to cache only the index page in the
    --     document root of an Apache web server.
    --
    --     @var\/www\/html\/index.html@
    --
    -- -   Specify the following to cache only the .html files in the document
    --     root of an Apache web server.
    --
    --     @var\/www\/html\/*.html@
    --
    -- -   Specify the following to cache only the .jpg, .png, and .gif files
    --     in the images sub-directory of the document root of an Apache web
    --     server.
    --
    --     @var\/www\/html\/images\/*.jpg@
    --
    --     @var\/www\/html\/images\/*.png@
    --
    --     @var\/www\/html\/images\/*.gif@
    --
    --     Specify the following to cache all files in the images sub-directory
    --     of the document root of an Apache web server.
    --
    --     @var\/www\/html\/images\/@
    path :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CacheBehaviorPerPath' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'behavior', 'cacheBehaviorPerPath_behavior' - The cache behavior for the specified path.
--
-- You can specify one of the following per-path cache behaviors:
--
-- -   __@cache@__ - This behavior caches the specified path.
--
-- -   __@dont-cache@__ - This behavior doesn\'t cache the specified path.
--
-- 'path', 'cacheBehaviorPerPath_path' - The path to a directory or file to cached, or not cache. Use an asterisk
-- symbol to specify wildcard directories (@path\/to\/assets\/*@), and file
-- types (@*.html, *jpg, *js@). Directories and file paths are
-- case-sensitive.
--
-- Examples:
--
-- -   Specify the following to cache all files in the document root of an
--     Apache web server running on a Lightsail instance.
--
--     @var\/www\/html\/@
--
-- -   Specify the following file to cache only the index page in the
--     document root of an Apache web server.
--
--     @var\/www\/html\/index.html@
--
-- -   Specify the following to cache only the .html files in the document
--     root of an Apache web server.
--
--     @var\/www\/html\/*.html@
--
-- -   Specify the following to cache only the .jpg, .png, and .gif files
--     in the images sub-directory of the document root of an Apache web
--     server.
--
--     @var\/www\/html\/images\/*.jpg@
--
--     @var\/www\/html\/images\/*.png@
--
--     @var\/www\/html\/images\/*.gif@
--
--     Specify the following to cache all files in the images sub-directory
--     of the document root of an Apache web server.
--
--     @var\/www\/html\/images\/@
newCacheBehaviorPerPath ::
  CacheBehaviorPerPath
newCacheBehaviorPerPath =
  CacheBehaviorPerPath'
    { behavior = Core.Nothing,
      path = Core.Nothing
    }

-- | The cache behavior for the specified path.
--
-- You can specify one of the following per-path cache behaviors:
--
-- -   __@cache@__ - This behavior caches the specified path.
--
-- -   __@dont-cache@__ - This behavior doesn\'t cache the specified path.
cacheBehaviorPerPath_behavior :: Lens.Lens' CacheBehaviorPerPath (Core.Maybe BehaviorEnum)
cacheBehaviorPerPath_behavior = Lens.lens (\CacheBehaviorPerPath' {behavior} -> behavior) (\s@CacheBehaviorPerPath' {} a -> s {behavior = a} :: CacheBehaviorPerPath)

-- | The path to a directory or file to cached, or not cache. Use an asterisk
-- symbol to specify wildcard directories (@path\/to\/assets\/*@), and file
-- types (@*.html, *jpg, *js@). Directories and file paths are
-- case-sensitive.
--
-- Examples:
--
-- -   Specify the following to cache all files in the document root of an
--     Apache web server running on a Lightsail instance.
--
--     @var\/www\/html\/@
--
-- -   Specify the following file to cache only the index page in the
--     document root of an Apache web server.
--
--     @var\/www\/html\/index.html@
--
-- -   Specify the following to cache only the .html files in the document
--     root of an Apache web server.
--
--     @var\/www\/html\/*.html@
--
-- -   Specify the following to cache only the .jpg, .png, and .gif files
--     in the images sub-directory of the document root of an Apache web
--     server.
--
--     @var\/www\/html\/images\/*.jpg@
--
--     @var\/www\/html\/images\/*.png@
--
--     @var\/www\/html\/images\/*.gif@
--
--     Specify the following to cache all files in the images sub-directory
--     of the document root of an Apache web server.
--
--     @var\/www\/html\/images\/@
cacheBehaviorPerPath_path :: Lens.Lens' CacheBehaviorPerPath (Core.Maybe Core.Text)
cacheBehaviorPerPath_path = Lens.lens (\CacheBehaviorPerPath' {path} -> path) (\s@CacheBehaviorPerPath' {} a -> s {path = a} :: CacheBehaviorPerPath)

instance Core.FromJSON CacheBehaviorPerPath where
  parseJSON =
    Core.withObject
      "CacheBehaviorPerPath"
      ( \x ->
          CacheBehaviorPerPath'
            Core.<$> (x Core..:? "behavior") Core.<*> (x Core..:? "path")
      )

instance Core.Hashable CacheBehaviorPerPath

instance Core.NFData CacheBehaviorPerPath

instance Core.ToJSON CacheBehaviorPerPath where
  toJSON CacheBehaviorPerPath' {..} =
    Core.object
      ( Core.catMaybes
          [ ("behavior" Core..=) Core.<$> behavior,
            ("path" Core..=) Core.<$> path
          ]
      )
