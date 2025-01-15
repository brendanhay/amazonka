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
-- Module      : Amazonka.CodeBuild.Types.ProjectCache
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.ProjectCache where

import Amazonka.CodeBuild.Types.CacheMode
import Amazonka.CodeBuild.Types.CacheType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the cache for the build project.
--
-- /See:/ 'newProjectCache' smart constructor.
data ProjectCache = ProjectCache'
  { -- | Information about the cache location:
    --
    -- -   @NO_CACHE@ or @LOCAL@: This value is ignored.
    --
    -- -   @S3@: This is the S3 bucket name\/prefix.
    location :: Prelude.Maybe Prelude.Text,
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
    modes :: Prelude.Maybe [CacheMode],
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProjectCache' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'location', 'projectCache_location' - Information about the cache location:
--
-- -   @NO_CACHE@ or @LOCAL@: This value is ignored.
--
-- -   @S3@: This is the S3 bucket name\/prefix.
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
    { location = Prelude.Nothing,
      modes = Prelude.Nothing,
      type' = pType_
    }

-- | Information about the cache location:
--
-- -   @NO_CACHE@ or @LOCAL@: This value is ignored.
--
-- -   @S3@: This is the S3 bucket name\/prefix.
projectCache_location :: Lens.Lens' ProjectCache (Prelude.Maybe Prelude.Text)
projectCache_location = Lens.lens (\ProjectCache' {location} -> location) (\s@ProjectCache' {} a -> s {location = a} :: ProjectCache)

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
projectCache_modes = Lens.lens (\ProjectCache' {modes} -> modes) (\s@ProjectCache' {} a -> s {modes = a} :: ProjectCache) Prelude.. Lens.mapping Lens.coerced

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

instance Data.FromJSON ProjectCache where
  parseJSON =
    Data.withObject
      "ProjectCache"
      ( \x ->
          ProjectCache'
            Prelude.<$> (x Data..:? "location")
            Prelude.<*> (x Data..:? "modes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "type")
      )

instance Prelude.Hashable ProjectCache where
  hashWithSalt _salt ProjectCache' {..} =
    _salt
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` modes
      `Prelude.hashWithSalt` type'

instance Prelude.NFData ProjectCache where
  rnf ProjectCache' {..} =
    Prelude.rnf location `Prelude.seq`
      Prelude.rnf modes `Prelude.seq`
        Prelude.rnf type'

instance Data.ToJSON ProjectCache where
  toJSON ProjectCache' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("location" Data..=) Prelude.<$> location,
            ("modes" Data..=) Prelude.<$> modes,
            Prelude.Just ("type" Data..= type')
          ]
      )
