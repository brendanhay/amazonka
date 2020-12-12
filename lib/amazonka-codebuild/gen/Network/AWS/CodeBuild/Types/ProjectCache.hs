{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ProjectCache
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ProjectCache
  ( ProjectCache (..),

    -- * Smart constructor
    mkProjectCache,

    -- * Lenses
    pcLocation,
    pcModes,
    pcType,
  )
where

import Network.AWS.CodeBuild.Types.CacheMode
import Network.AWS.CodeBuild.Types.CacheType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the cache for the build project.
--
-- /See:/ 'mkProjectCache' smart constructor.
data ProjectCache = ProjectCache'
  { location :: Lude.Maybe Lude.Text,
    modes :: Lude.Maybe [CacheMode],
    type' :: CacheType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProjectCache' with the minimum fields required to make a request.
--
-- * 'location' - Information about the cache location:
--
--
--     * @NO_CACHE@ or @LOCAL@ : This value is ignored.
--
--
--     * @S3@ : This is the S3 bucket name/prefix.
--
--
-- * 'modes' - An array of strings that specify the local cache modes. You can use one or more local cache modes at the same time. This is only used for @LOCAL@ cache types.
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
-- * 'type'' - The type of cache used by the build project. Valid values include:
--
--
--     * @NO_CACHE@ : The build project does not use any cache.
--
--
--     * @S3@ : The build project reads and writes from and to S3.
--
--
--     * @LOCAL@ : The build project stores a cache locally on a build host that is only available to that build host.
mkProjectCache ::
  -- | 'type''
  CacheType ->
  ProjectCache
mkProjectCache pType_ =
  ProjectCache'
    { location = Lude.Nothing,
      modes = Lude.Nothing,
      type' = pType_
    }

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
pcLocation :: Lens.Lens' ProjectCache (Lude.Maybe Lude.Text)
pcLocation = Lens.lens (location :: ProjectCache -> Lude.Maybe Lude.Text) (\s a -> s {location = a} :: ProjectCache)
{-# DEPRECATED pcLocation "Use generic-lens or generic-optics with 'location' instead." #-}

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
pcModes :: Lens.Lens' ProjectCache (Lude.Maybe [CacheMode])
pcModes = Lens.lens (modes :: ProjectCache -> Lude.Maybe [CacheMode]) (\s a -> s {modes = a} :: ProjectCache)
{-# DEPRECATED pcModes "Use generic-lens or generic-optics with 'modes' instead." #-}

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
pcType :: Lens.Lens' ProjectCache CacheType
pcType = Lens.lens (type' :: ProjectCache -> CacheType) (\s a -> s {type' = a} :: ProjectCache)
{-# DEPRECATED pcType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON ProjectCache where
  parseJSON =
    Lude.withObject
      "ProjectCache"
      ( \x ->
          ProjectCache'
            Lude.<$> (x Lude..:? "location")
            Lude.<*> (x Lude..:? "modes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "type")
      )

instance Lude.ToJSON ProjectCache where
  toJSON ProjectCache' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("location" Lude..=) Lude.<$> location,
            ("modes" Lude..=) Lude.<$> modes,
            Lude.Just ("type" Lude..= type')
          ]
      )
