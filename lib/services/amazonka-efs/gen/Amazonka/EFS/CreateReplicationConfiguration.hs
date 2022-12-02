{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EFS.CreateReplicationConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a replication configuration that replicates an existing EFS file
-- system to a new, read-only file system. For more information, see
-- <https://docs.aws.amazon.com/efs/latest/ug/efs-replication.html Amazon EFS replication>
-- in the /Amazon EFS User Guide/. The replication configuration specifies
-- the following:
--
-- -   __Source file system__ - An existing EFS file system that you want
--     replicated. The source file system cannot be a destination file
--     system in an existing replication configuration.
--
-- -   __Destination file system configuration__ - The configuration of the
--     destination file system to which the source file system will be
--     replicated. There can only be one destination file system in a
--     replication configuration. The destination file system configuration
--     consists of the following properties:
--
--     -   __Amazon Web Services Region__ - The Amazon Web Services Region
--         in which the destination file system is created. Amazon EFS
--         replication is available in all Amazon Web Services Regions that
--         Amazon EFS is available in, except Africa (Cape Town), Asia
--         Pacific (Hong Kong), Asia Pacific (Jakarta), Europe (Milan), and
--         Middle East (Bahrain).
--
--     -   __Availability Zone__ - If you want the destination file system
--         to use EFS One Zone availability and durability, you must
--         specify the Availability Zone to create the file system in. For
--         more information about EFS storage classes, see
--         <https://docs.aws.amazon.com/efs/latest/ug/storage-classes.html Amazon EFS storage classes>
--         in the /Amazon EFS User Guide/.
--
--     -   __Encryption__ - All destination file systems are created with
--         encryption at rest enabled. You can specify the Key Management
--         Service (KMS) key that is used to encrypt the destination file
--         system. If you don\'t specify a KMS key, your service-managed
--         KMS key for Amazon EFS is used.
--
--         After the file system is created, you cannot change the KMS key.
--
-- The following properties are set by default:
--
-- -   __Performance mode__ - The destination file system\'s performance
--     mode matches that of the source file system, unless the destination
--     file system uses EFS One Zone storage. In that case, the General
--     Purpose performance mode is used. The performance mode cannot be
--     changed.
--
-- -   __Throughput mode__ - The destination file system uses the Bursting
--     Throughput mode by default. After the file system is created, you
--     can modify the throughput mode.
--
-- The following properties are turned off by default:
--
-- -   __Lifecycle management__ - EFS lifecycle management and EFS
--     Intelligent-Tiering are not enabled on the destination file system.
--     After the destination file system is created, you can enable EFS
--     lifecycle management and EFS Intelligent-Tiering.
--
-- -   __Automatic backups__ - Automatic daily backups not enabled on the
--     destination file system. After the file system is created, you can
--     change this setting.
--
-- For more information, see
-- <https://docs.aws.amazon.com/efs/latest/ug/efs-replication.html Amazon EFS replication>
-- in the /Amazon EFS User Guide/.
module Amazonka.EFS.CreateReplicationConfiguration
  ( -- * Creating a Request
    CreateReplicationConfiguration (..),
    newCreateReplicationConfiguration,

    -- * Request Lenses
    createReplicationConfiguration_sourceFileSystemId,
    createReplicationConfiguration_destinations,

    -- * Destructuring the Response
    ReplicationConfigurationDescription (..),
    newReplicationConfigurationDescription,

    -- * Response Lenses
    replicationConfigurationDescription_sourceFileSystemId,
    replicationConfigurationDescription_sourceFileSystemRegion,
    replicationConfigurationDescription_sourceFileSystemArn,
    replicationConfigurationDescription_originalSourceFileSystemArn,
    replicationConfigurationDescription_creationTime,
    replicationConfigurationDescription_destinations,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EFS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateReplicationConfiguration' smart constructor.
data CreateReplicationConfiguration = CreateReplicationConfiguration'
  { -- | Specifies the Amazon EFS file system that you want to replicate. This
    -- file system cannot already be a source or destination file system in
    -- another replication configuration.
    sourceFileSystemId :: Prelude.Text,
    -- | An array of destination configuration objects. Only one destination
    -- configuration object is supported.
    destinations :: [DestinationToCreate]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateReplicationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceFileSystemId', 'createReplicationConfiguration_sourceFileSystemId' - Specifies the Amazon EFS file system that you want to replicate. This
-- file system cannot already be a source or destination file system in
-- another replication configuration.
--
-- 'destinations', 'createReplicationConfiguration_destinations' - An array of destination configuration objects. Only one destination
-- configuration object is supported.
newCreateReplicationConfiguration ::
  -- | 'sourceFileSystemId'
  Prelude.Text ->
  CreateReplicationConfiguration
newCreateReplicationConfiguration
  pSourceFileSystemId_ =
    CreateReplicationConfiguration'
      { sourceFileSystemId =
          pSourceFileSystemId_,
        destinations = Prelude.mempty
      }

-- | Specifies the Amazon EFS file system that you want to replicate. This
-- file system cannot already be a source or destination file system in
-- another replication configuration.
createReplicationConfiguration_sourceFileSystemId :: Lens.Lens' CreateReplicationConfiguration Prelude.Text
createReplicationConfiguration_sourceFileSystemId = Lens.lens (\CreateReplicationConfiguration' {sourceFileSystemId} -> sourceFileSystemId) (\s@CreateReplicationConfiguration' {} a -> s {sourceFileSystemId = a} :: CreateReplicationConfiguration)

-- | An array of destination configuration objects. Only one destination
-- configuration object is supported.
createReplicationConfiguration_destinations :: Lens.Lens' CreateReplicationConfiguration [DestinationToCreate]
createReplicationConfiguration_destinations = Lens.lens (\CreateReplicationConfiguration' {destinations} -> destinations) (\s@CreateReplicationConfiguration' {} a -> s {destinations = a} :: CreateReplicationConfiguration) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    CreateReplicationConfiguration
  where
  type
    AWSResponse CreateReplicationConfiguration =
      ReplicationConfigurationDescription
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance
  Prelude.Hashable
    CreateReplicationConfiguration
  where
  hashWithSalt
    _salt
    CreateReplicationConfiguration' {..} =
      _salt `Prelude.hashWithSalt` sourceFileSystemId
        `Prelude.hashWithSalt` destinations

instance
  Prelude.NFData
    CreateReplicationConfiguration
  where
  rnf CreateReplicationConfiguration' {..} =
    Prelude.rnf sourceFileSystemId
      `Prelude.seq` Prelude.rnf destinations

instance
  Data.ToHeaders
    CreateReplicationConfiguration
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreateReplicationConfiguration where
  toJSON CreateReplicationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Destinations" Data..= destinations)]
      )

instance Data.ToPath CreateReplicationConfiguration where
  toPath CreateReplicationConfiguration' {..} =
    Prelude.mconcat
      [ "/2015-02-01/file-systems/",
        Data.toBS sourceFileSystemId,
        "/replication-configuration"
      ]

instance Data.ToQuery CreateReplicationConfiguration where
  toQuery = Prelude.const Prelude.mempty
