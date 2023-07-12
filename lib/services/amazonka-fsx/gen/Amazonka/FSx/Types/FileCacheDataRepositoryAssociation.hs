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
-- Module      : Amazonka.FSx.Types.FileCacheDataRepositoryAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.FileCacheDataRepositoryAssociation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types.FileCacheNFSConfiguration
import qualified Amazonka.Prelude as Prelude

-- | The configuration for a data repository association (DRA) to be created
-- during the Amazon File Cache resource creation. The DRA links the cache
-- to either an Amazon S3 bucket or prefix, or a Network File System (NFS)
-- data repository that supports the NFSv3 protocol.
--
-- The DRA does not support automatic import or automatic export.
--
-- /See:/ 'newFileCacheDataRepositoryAssociation' smart constructor.
data FileCacheDataRepositoryAssociation = FileCacheDataRepositoryAssociation'
  { -- | A list of NFS Exports that will be linked with this data repository
    -- association. The Export paths are in the format @\/exportpath1@. To use
    -- this parameter, you must configure @DataRepositoryPath@ as the domain
    -- name of the NFS file system. The NFS file system domain name in effect
    -- is the root of the subdirectories. Note that
    -- @DataRepositorySubdirectories@ is not supported for S3 data
    -- repositories.
    dataRepositorySubdirectories :: Prelude.Maybe [Prelude.Text],
    -- | The configuration for a data repository association that links an Amazon
    -- File Cache resource to an NFS data repository.
    nfs :: Prelude.Maybe FileCacheNFSConfiguration,
    -- | A path on the cache that points to a high-level directory (such as
    -- @\/ns1\/@) or subdirectory (such as @\/ns1\/subdir\/@) that will be
    -- mapped 1-1 with @DataRepositoryPath@. The leading forward slash in the
    -- name is required. Two data repository associations cannot have
    -- overlapping cache paths. For example, if a data repository is associated
    -- with cache path @\/ns1\/@, then you cannot link another data repository
    -- with cache path @\/ns1\/ns2@.
    --
    -- This path specifies where in your cache files will be exported from.
    -- This cache directory can be linked to only one data repository, and no
    -- data repository other can be linked to the directory.
    --
    -- The cache path can only be set to root (\/) on an NFS DRA when
    -- @DataRepositorySubdirectories@ is specified. If you specify root (\/) as
    -- the cache path, you can create only one DRA on the cache.
    --
    -- The cache path cannot be set to root (\/) for an S3 DRA.
    fileCachePath :: Prelude.Text,
    -- | The path to the S3 or NFS data repository that links to the cache. You
    -- must provide one of the following paths:
    --
    -- -   The path can be an NFS data repository that links to the cache. The
    --     path can be in one of two formats:
    --
    --     -   If you are not using the @DataRepositorySubdirectories@
    --         parameter, the path is to an NFS Export directory (or one of its
    --         subdirectories) in the format
    --         @nsf:\/\/nfs-domain-name\/exportpath@. You can therefore link a
    --         single NFS Export to a single data repository association.
    --
    --     -   If you are using the @DataRepositorySubdirectories@ parameter,
    --         the path is the domain name of the NFS file system in the format
    --         @nfs:\/\/filer-domain-name@, which indicates the root of the
    --         subdirectories specified with the @DataRepositorySubdirectories@
    --         parameter.
    --
    -- -   The path can be an S3 bucket or prefix in the format
    --     @s3:\/\/myBucket\/myPrefix\/@.
    dataRepositoryPath :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FileCacheDataRepositoryAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataRepositorySubdirectories', 'fileCacheDataRepositoryAssociation_dataRepositorySubdirectories' - A list of NFS Exports that will be linked with this data repository
-- association. The Export paths are in the format @\/exportpath1@. To use
-- this parameter, you must configure @DataRepositoryPath@ as the domain
-- name of the NFS file system. The NFS file system domain name in effect
-- is the root of the subdirectories. Note that
-- @DataRepositorySubdirectories@ is not supported for S3 data
-- repositories.
--
-- 'nfs', 'fileCacheDataRepositoryAssociation_nfs' - The configuration for a data repository association that links an Amazon
-- File Cache resource to an NFS data repository.
--
-- 'fileCachePath', 'fileCacheDataRepositoryAssociation_fileCachePath' - A path on the cache that points to a high-level directory (such as
-- @\/ns1\/@) or subdirectory (such as @\/ns1\/subdir\/@) that will be
-- mapped 1-1 with @DataRepositoryPath@. The leading forward slash in the
-- name is required. Two data repository associations cannot have
-- overlapping cache paths. For example, if a data repository is associated
-- with cache path @\/ns1\/@, then you cannot link another data repository
-- with cache path @\/ns1\/ns2@.
--
-- This path specifies where in your cache files will be exported from.
-- This cache directory can be linked to only one data repository, and no
-- data repository other can be linked to the directory.
--
-- The cache path can only be set to root (\/) on an NFS DRA when
-- @DataRepositorySubdirectories@ is specified. If you specify root (\/) as
-- the cache path, you can create only one DRA on the cache.
--
-- The cache path cannot be set to root (\/) for an S3 DRA.
--
-- 'dataRepositoryPath', 'fileCacheDataRepositoryAssociation_dataRepositoryPath' - The path to the S3 or NFS data repository that links to the cache. You
-- must provide one of the following paths:
--
-- -   The path can be an NFS data repository that links to the cache. The
--     path can be in one of two formats:
--
--     -   If you are not using the @DataRepositorySubdirectories@
--         parameter, the path is to an NFS Export directory (or one of its
--         subdirectories) in the format
--         @nsf:\/\/nfs-domain-name\/exportpath@. You can therefore link a
--         single NFS Export to a single data repository association.
--
--     -   If you are using the @DataRepositorySubdirectories@ parameter,
--         the path is the domain name of the NFS file system in the format
--         @nfs:\/\/filer-domain-name@, which indicates the root of the
--         subdirectories specified with the @DataRepositorySubdirectories@
--         parameter.
--
-- -   The path can be an S3 bucket or prefix in the format
--     @s3:\/\/myBucket\/myPrefix\/@.
newFileCacheDataRepositoryAssociation ::
  -- | 'fileCachePath'
  Prelude.Text ->
  -- | 'dataRepositoryPath'
  Prelude.Text ->
  FileCacheDataRepositoryAssociation
newFileCacheDataRepositoryAssociation
  pFileCachePath_
  pDataRepositoryPath_ =
    FileCacheDataRepositoryAssociation'
      { dataRepositorySubdirectories =
          Prelude.Nothing,
        nfs = Prelude.Nothing,
        fileCachePath = pFileCachePath_,
        dataRepositoryPath =
          pDataRepositoryPath_
      }

-- | A list of NFS Exports that will be linked with this data repository
-- association. The Export paths are in the format @\/exportpath1@. To use
-- this parameter, you must configure @DataRepositoryPath@ as the domain
-- name of the NFS file system. The NFS file system domain name in effect
-- is the root of the subdirectories. Note that
-- @DataRepositorySubdirectories@ is not supported for S3 data
-- repositories.
fileCacheDataRepositoryAssociation_dataRepositorySubdirectories :: Lens.Lens' FileCacheDataRepositoryAssociation (Prelude.Maybe [Prelude.Text])
fileCacheDataRepositoryAssociation_dataRepositorySubdirectories = Lens.lens (\FileCacheDataRepositoryAssociation' {dataRepositorySubdirectories} -> dataRepositorySubdirectories) (\s@FileCacheDataRepositoryAssociation' {} a -> s {dataRepositorySubdirectories = a} :: FileCacheDataRepositoryAssociation) Prelude.. Lens.mapping Lens.coerced

-- | The configuration for a data repository association that links an Amazon
-- File Cache resource to an NFS data repository.
fileCacheDataRepositoryAssociation_nfs :: Lens.Lens' FileCacheDataRepositoryAssociation (Prelude.Maybe FileCacheNFSConfiguration)
fileCacheDataRepositoryAssociation_nfs = Lens.lens (\FileCacheDataRepositoryAssociation' {nfs} -> nfs) (\s@FileCacheDataRepositoryAssociation' {} a -> s {nfs = a} :: FileCacheDataRepositoryAssociation)

-- | A path on the cache that points to a high-level directory (such as
-- @\/ns1\/@) or subdirectory (such as @\/ns1\/subdir\/@) that will be
-- mapped 1-1 with @DataRepositoryPath@. The leading forward slash in the
-- name is required. Two data repository associations cannot have
-- overlapping cache paths. For example, if a data repository is associated
-- with cache path @\/ns1\/@, then you cannot link another data repository
-- with cache path @\/ns1\/ns2@.
--
-- This path specifies where in your cache files will be exported from.
-- This cache directory can be linked to only one data repository, and no
-- data repository other can be linked to the directory.
--
-- The cache path can only be set to root (\/) on an NFS DRA when
-- @DataRepositorySubdirectories@ is specified. If you specify root (\/) as
-- the cache path, you can create only one DRA on the cache.
--
-- The cache path cannot be set to root (\/) for an S3 DRA.
fileCacheDataRepositoryAssociation_fileCachePath :: Lens.Lens' FileCacheDataRepositoryAssociation Prelude.Text
fileCacheDataRepositoryAssociation_fileCachePath = Lens.lens (\FileCacheDataRepositoryAssociation' {fileCachePath} -> fileCachePath) (\s@FileCacheDataRepositoryAssociation' {} a -> s {fileCachePath = a} :: FileCacheDataRepositoryAssociation)

-- | The path to the S3 or NFS data repository that links to the cache. You
-- must provide one of the following paths:
--
-- -   The path can be an NFS data repository that links to the cache. The
--     path can be in one of two formats:
--
--     -   If you are not using the @DataRepositorySubdirectories@
--         parameter, the path is to an NFS Export directory (or one of its
--         subdirectories) in the format
--         @nsf:\/\/nfs-domain-name\/exportpath@. You can therefore link a
--         single NFS Export to a single data repository association.
--
--     -   If you are using the @DataRepositorySubdirectories@ parameter,
--         the path is the domain name of the NFS file system in the format
--         @nfs:\/\/filer-domain-name@, which indicates the root of the
--         subdirectories specified with the @DataRepositorySubdirectories@
--         parameter.
--
-- -   The path can be an S3 bucket or prefix in the format
--     @s3:\/\/myBucket\/myPrefix\/@.
fileCacheDataRepositoryAssociation_dataRepositoryPath :: Lens.Lens' FileCacheDataRepositoryAssociation Prelude.Text
fileCacheDataRepositoryAssociation_dataRepositoryPath = Lens.lens (\FileCacheDataRepositoryAssociation' {dataRepositoryPath} -> dataRepositoryPath) (\s@FileCacheDataRepositoryAssociation' {} a -> s {dataRepositoryPath = a} :: FileCacheDataRepositoryAssociation)

instance
  Prelude.Hashable
    FileCacheDataRepositoryAssociation
  where
  hashWithSalt
    _salt
    FileCacheDataRepositoryAssociation' {..} =
      _salt
        `Prelude.hashWithSalt` dataRepositorySubdirectories
        `Prelude.hashWithSalt` nfs
        `Prelude.hashWithSalt` fileCachePath
        `Prelude.hashWithSalt` dataRepositoryPath

instance
  Prelude.NFData
    FileCacheDataRepositoryAssociation
  where
  rnf FileCacheDataRepositoryAssociation' {..} =
    Prelude.rnf dataRepositorySubdirectories
      `Prelude.seq` Prelude.rnf nfs
      `Prelude.seq` Prelude.rnf fileCachePath
      `Prelude.seq` Prelude.rnf dataRepositoryPath

instance
  Data.ToJSON
    FileCacheDataRepositoryAssociation
  where
  toJSON FileCacheDataRepositoryAssociation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DataRepositorySubdirectories" Data..=)
              Prelude.<$> dataRepositorySubdirectories,
            ("NFS" Data..=) Prelude.<$> nfs,
            Prelude.Just ("FileCachePath" Data..= fileCachePath),
            Prelude.Just
              ("DataRepositoryPath" Data..= dataRepositoryPath)
          ]
      )
