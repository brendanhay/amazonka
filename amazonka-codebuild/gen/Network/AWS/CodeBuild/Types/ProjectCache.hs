{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CodeBuild.Types.ProjectCache
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ProjectCache where

import Network.AWS.CodeBuild.Types.CacheMode
import Network.AWS.CodeBuild.Types.CacheType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the cache for the build project.
--
-- /See:/ 'newProjectCache' smart constructor.
data ProjectCache = ProjectCache'
  { -- | An array of strings that specify the local cache modes. You can use one
    -- or more local cache modes at the same time. This is only used for
    -- @LOCAL@ cache types.
    --
    -- Possible values are:
    --
    -- [LOCAL_SOURCE_CACHE]
    --     Caches Git metadata for primary and secondary sources. After the
    --     cache is created, subsequent builds pull only the change between
    --     commits. This mode is a good choice for projects with a clean
    --     working directory and a source that is a large Git repository. If
    --     you choose this option and your project does not use a Git
    --     repository (GitHub, GitHub Enterprise, or Bitbucket), the option is
    --     ignored.
    --
    -- [LOCAL_DOCKER_LAYER_CACHE]
    --     Caches existing Docker layers. This mode is a good choice for
    --     projects that build or pull large Docker images. It can prevent the
    --     performance issues caused by pulling large Docker images down from
    --     the network.
    --
    --     -   You can use a Docker layer cache in the Linux environment only.
    --
    --     -   The @privileged@ flag must be set so that your project has the
    --         required Docker permissions.
    --
    --     -   You should consider the security implications before you use a
    --         Docker layer cache.
    --
    -- [LOCAL_CUSTOM_CACHE]
    --     Caches directories you specify in the buildspec file. This mode is a
    --     good choice if your build scenario is not suited to one of the other
    --     three local cache modes. If you use a custom cache:
    --
    --     -   Only directories can be specified for caching. You cannot
    --         specify individual files.
    --
    --     -   Symlinks are used to reference cached directories.
    --
    --     -   Cached directories are linked to your build before it downloads
    --         its project sources. Cached items are overridden if a source
    --         item has the same name. Directories are specified using cache
    --         paths in the buildspec file.
    modes :: Prelude.Maybe [CacheMode],
    -- | Information about the cache location:
    --
    -- -   @NO_CACHE@ or @LOCAL@: This value is ignored.
    --
    -- -   @S3@: This is the S3 bucket name\/prefix.
    location :: Prelude.Maybe Prelude.Text,
    -- | The type of cache used by the build project. Valid values include:
    --
    -- -   @NO_CACHE@: The build project does not use any cache.
    --
    -- -   @S3@: The build project reads and writes from and to S3.
    --
    -- -   @LOCAL@: The build project stores a cache locally on a build host
    --     that is only available to that build host.
    type' :: CacheType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ProjectCache' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modes', 'projectCache_modes' - An array of strings that specify the local cache modes. You can use one
-- or more local cache modes at the same time. This is only used for
-- @LOCAL@ cache types.
--
-- Possible values are:
--
-- [LOCAL_SOURCE_CACHE]
--     Caches Git metadata for primary and secondary sources. After the
--     cache is created, subsequent builds pull only the change between
--     commits. This mode is a good choice for projects with a clean
--     working directory and a source that is a large Git repository. If
--     you choose this option and your project does not use a Git
--     repository (GitHub, GitHub Enterprise, or Bitbucket), the option is
--     ignored.
--
-- [LOCAL_DOCKER_LAYER_CACHE]
--     Caches existing Docker layers. This mode is a good choice for
--     projects that build or pull large Docker images. It can prevent the
--     performance issues caused by pulling large Docker images down from
--     the network.
--
--     -   You can use a Docker layer cache in the Linux environment only.
--
--     -   The @privileged@ flag must be set so that your project has the
--         required Docker permissions.
--
--     -   You should consider the security implications before you use a
--         Docker layer cache.
--
-- [LOCAL_CUSTOM_CACHE]
--     Caches directories you specify in the buildspec file. This mode is a
--     good choice if your build scenario is not suited to one of the other
--     three local cache modes. If you use a custom cache:
--
--     -   Only directories can be specified for caching. You cannot
--         specify individual files.
--
--     -   Symlinks are used to reference cached directories.
--
--     -   Cached directories are linked to your build before it downloads
--         its project sources. Cached items are overridden if a source
--         item has the same name. Directories are specified using cache
--         paths in the buildspec file.
--
-- 'location', 'projectCache_location' - Information about the cache location:
--
-- -   @NO_CACHE@ or @LOCAL@: This value is ignored.
--
-- -   @S3@: This is the S3 bucket name\/prefix.
--
-- 'type'', 'projectCache_type' - The type of cache used by the build project. Valid values include:
--
-- -   @NO_CACHE@: The build project does not use any cache.
--
-- -   @S3@: The build project reads and writes from and to S3.
--
-- -   @LOCAL@: The build project stores a cache locally on a build host
--     that is only available to that build host.
newProjectCache ::
  -- | 'type''
  CacheType ->
  ProjectCache
newProjectCache pType_ =
  ProjectCache'
    { modes = Prelude.Nothing,
      location = Prelude.Nothing,
      type' = pType_
    }

-- | An array of strings that specify the local cache modes. You can use one
-- or more local cache modes at the same time. This is only used for
-- @LOCAL@ cache types.
--
-- Possible values are:
--
-- [LOCAL_SOURCE_CACHE]
--     Caches Git metadata for primary and secondary sources. After the
--     cache is created, subsequent builds pull only the change between
--     commits. This mode is a good choice for projects with a clean
--     working directory and a source that is a large Git repository. If
--     you choose this option and your project does not use a Git
--     repository (GitHub, GitHub Enterprise, or Bitbucket), the option is
--     ignored.
--
-- [LOCAL_DOCKER_LAYER_CACHE]
--     Caches existing Docker layers. This mode is a good choice for
--     projects that build or pull large Docker images. It can prevent the
--     performance issues caused by pulling large Docker images down from
--     the network.
--
--     -   You can use a Docker layer cache in the Linux environment only.
--
--     -   The @privileged@ flag must be set so that your project has the
--         required Docker permissions.
--
--     -   You should consider the security implications before you use a
--         Docker layer cache.
--
-- [LOCAL_CUSTOM_CACHE]
--     Caches directories you specify in the buildspec file. This mode is a
--     good choice if your build scenario is not suited to one of the other
--     three local cache modes. If you use a custom cache:
--
--     -   Only directories can be specified for caching. You cannot
--         specify individual files.
--
--     -   Symlinks are used to reference cached directories.
--
--     -   Cached directories are linked to your build before it downloads
--         its project sources. Cached items are overridden if a source
--         item has the same name. Directories are specified using cache
--         paths in the buildspec file.
projectCache_modes :: Lens.Lens' ProjectCache (Prelude.Maybe [CacheMode])
projectCache_modes = Lens.lens (\ProjectCache' {modes} -> modes) (\s@ProjectCache' {} a -> s {modes = a} :: ProjectCache) Prelude.. Lens.mapping Prelude._Coerce

-- | Information about the cache location:
--
-- -   @NO_CACHE@ or @LOCAL@: This value is ignored.
--
-- -   @S3@: This is the S3 bucket name\/prefix.
projectCache_location :: Lens.Lens' ProjectCache (Prelude.Maybe Prelude.Text)
projectCache_location = Lens.lens (\ProjectCache' {location} -> location) (\s@ProjectCache' {} a -> s {location = a} :: ProjectCache)

-- | The type of cache used by the build project. Valid values include:
--
-- -   @NO_CACHE@: The build project does not use any cache.
--
-- -   @S3@: The build project reads and writes from and to S3.
--
-- -   @LOCAL@: The build project stores a cache locally on a build host
--     that is only available to that build host.
projectCache_type :: Lens.Lens' ProjectCache CacheType
projectCache_type = Lens.lens (\ProjectCache' {type'} -> type') (\s@ProjectCache' {} a -> s {type' = a} :: ProjectCache)

instance Prelude.FromJSON ProjectCache where
  parseJSON =
    Prelude.withObject
      "ProjectCache"
      ( \x ->
          ProjectCache'
            Prelude.<$> (x Prelude..:? "modes" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "location")
            Prelude.<*> (x Prelude..: "type")
      )

instance Prelude.Hashable ProjectCache

instance Prelude.NFData ProjectCache

instance Prelude.ToJSON ProjectCache where
  toJSON ProjectCache' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("modes" Prelude..=) Prelude.<$> modes,
            ("location" Prelude..=) Prelude.<$> location,
            Prelude.Just ("type" Prelude..= type')
          ]
      )
