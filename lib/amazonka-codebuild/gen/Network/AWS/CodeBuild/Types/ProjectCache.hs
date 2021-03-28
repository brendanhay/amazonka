{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ProjectCache
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeBuild.Types.ProjectCache
  ( ProjectCache (..)
  -- * Smart constructor
  , mkProjectCache
  -- * Lenses
  , pcType
  , pcLocation
  , pcModes
  ) where

import qualified Network.AWS.CodeBuild.Types.CacheMode as Types
import qualified Network.AWS.CodeBuild.Types.CacheType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the cache for the build project.
--
-- /See:/ 'mkProjectCache' smart constructor.
data ProjectCache = ProjectCache'
  { type' :: Types.CacheType
    -- ^ The type of cache used by the build project. Valid values include:
--
--
--     * @NO_CACHE@ : The build project does not use any cache.
--
--
--     * @S3@ : The build project reads and writes from and to S3.
--
--
--     * @LOCAL@ : The build project stores a cache locally on a build host that is only available to that build host.
--
--
  , location :: Core.Maybe Core.Text
    -- ^ Information about the cache location: 
--
--
--     * @NO_CACHE@ or @LOCAL@ : This value is ignored.
--
--
--     * @S3@ : This is the S3 bucket name/prefix.
--
--
  , modes :: Core.Maybe [Types.CacheMode]
    -- ^ An array of strings that specify the local cache modes. You can use one or more local cache modes at the same time. This is only used for @LOCAL@ cache types.
--
-- Possible values are:
--
--     * LOCAL_SOURCE_CACHE
--
--     * Caches Git metadata for primary and secondary sources. After the cache is created, subsequent builds pull only the change between commits. This mode is a good choice for projects with a clean working directory and a source that is a large Git repository. If you choose this option and your project does not use a Git repository (GitHub, GitHub Enterprise, or Bitbucket), the option is ignored. 
--
--
--     * LOCAL_DOCKER_LAYER_CACHE
--
--     * Caches existing Docker layers. This mode is a good choice for projects that build or pull large Docker images. It can prevent the performance issues caused by pulling large Docker images down from the network. 
--
--
--     * LOCAL_CUSTOM_CACHE
--
--     * Caches directories you specify in the buildspec file. This mode is a good choice if your build scenario is not suited to one of the other three local cache modes. If you use a custom cache: 
--
--     * Only directories can be specified for caching. You cannot specify individual files. 
--
--
--     * Symlinks are used to reference cached directories. 
--
--
--     * Cached directories are linked to your build before it downloads its project sources. Cached items are overridden if a source item has the same name. Directories are specified using cache paths in the buildspec file. 
--
--
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ProjectCache' value with any optional fields omitted.
mkProjectCache
    :: Types.CacheType -- ^ 'type\''
    -> ProjectCache
mkProjectCache type'
  = ProjectCache'{type', location = Core.Nothing,
                  modes = Core.Nothing}

-- | The type of cache used by the build project. Valid values include:
--
--
--     * @NO_CACHE@ : The build project does not use any cache.
--
--
--     * @S3@ : The build project reads and writes from and to S3.
--
--
--     * @LOCAL@ : The build project stores a cache locally on a build host that is only available to that build host.
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcType :: Lens.Lens' ProjectCache Types.CacheType
pcType = Lens.field @"type'"
{-# INLINEABLE pcType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | Information about the cache location: 
--
--
--     * @NO_CACHE@ or @LOCAL@ : This value is ignored.
--
--
--     * @S3@ : This is the S3 bucket name/prefix.
--
--
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcLocation :: Lens.Lens' ProjectCache (Core.Maybe Core.Text)
pcLocation = Lens.field @"location"
{-# INLINEABLE pcLocation #-}
{-# DEPRECATED location "Use generic-lens or generic-optics with 'location' instead"  #-}

-- | An array of strings that specify the local cache modes. You can use one or more local cache modes at the same time. This is only used for @LOCAL@ cache types.
--
-- Possible values are:
--
--     * LOCAL_SOURCE_CACHE
--
--     * Caches Git metadata for primary and secondary sources. After the cache is created, subsequent builds pull only the change between commits. This mode is a good choice for projects with a clean working directory and a source that is a large Git repository. If you choose this option and your project does not use a Git repository (GitHub, GitHub Enterprise, or Bitbucket), the option is ignored. 
--
--
--     * LOCAL_DOCKER_LAYER_CACHE
--
--     * Caches existing Docker layers. This mode is a good choice for projects that build or pull large Docker images. It can prevent the performance issues caused by pulling large Docker images down from the network. 
--
--
--     * LOCAL_CUSTOM_CACHE
--
--     * Caches directories you specify in the buildspec file. This mode is a good choice if your build scenario is not suited to one of the other three local cache modes. If you use a custom cache: 
--
--     * Only directories can be specified for caching. You cannot specify individual files. 
--
--
--     * Symlinks are used to reference cached directories. 
--
--
--     * Cached directories are linked to your build before it downloads its project sources. Cached items are overridden if a source item has the same name. Directories are specified using cache paths in the buildspec file. 
--
--
--
--
--
-- /Note:/ Consider using 'modes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcModes :: Lens.Lens' ProjectCache (Core.Maybe [Types.CacheMode])
pcModes = Lens.field @"modes"
{-# INLINEABLE pcModes #-}
{-# DEPRECATED modes "Use generic-lens or generic-optics with 'modes' instead"  #-}

instance Core.FromJSON ProjectCache where
        toJSON ProjectCache{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("type" Core..= type'),
                  ("location" Core..=) Core.<$> location,
                  ("modes" Core..=) Core.<$> modes])

instance Core.FromJSON ProjectCache where
        parseJSON
          = Core.withObject "ProjectCache" Core.$
              \ x ->
                ProjectCache' Core.<$>
                  (x Core..: "type") Core.<*> x Core..:? "location" Core.<*>
                    x Core..:? "modes"
