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
-- Module      : Amazonka.EFS.UpdateFileSystem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the throughput mode or the amount of provisioned throughput of
-- an existing file system.
module Amazonka.EFS.UpdateFileSystem
  ( -- * Creating a Request
    UpdateFileSystem (..),
    newUpdateFileSystem,

    -- * Request Lenses
    updateFileSystem_provisionedThroughputInMibps,
    updateFileSystem_throughputMode,
    updateFileSystem_fileSystemId,

    -- * Destructuring the Response
    FileSystemDescription (..),
    newFileSystemDescription,

    -- * Response Lenses
    fileSystemDescription_availabilityZoneId,
    fileSystemDescription_availabilityZoneName,
    fileSystemDescription_encrypted,
    fileSystemDescription_fileSystemArn,
    fileSystemDescription_kmsKeyId,
    fileSystemDescription_name,
    fileSystemDescription_provisionedThroughputInMibps,
    fileSystemDescription_throughputMode,
    fileSystemDescription_ownerId,
    fileSystemDescription_creationToken,
    fileSystemDescription_fileSystemId,
    fileSystemDescription_creationTime,
    fileSystemDescription_lifeCycleState,
    fileSystemDescription_numberOfMountTargets,
    fileSystemDescription_sizeInBytes,
    fileSystemDescription_performanceMode,
    fileSystemDescription_tags,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EFS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateFileSystem' smart constructor.
data UpdateFileSystem = UpdateFileSystem'
  { -- | (Optional) Sets the amount of provisioned throughput, in MiB\/s, for the
    -- file system. Valid values are 1-1024. If you are changing the throughput
    -- mode to provisioned, you must also provide the amount of provisioned
    -- throughput. Required if @ThroughputMode@ is changed to @provisioned@ on
    -- update.
    provisionedThroughputInMibps :: Prelude.Maybe Prelude.Double,
    -- | (Optional) Updates the file system\'s throughput mode. If you\'re not
    -- updating your throughput mode, you don\'t need to provide this value in
    -- your request. If you are changing the @ThroughputMode@ to @provisioned@,
    -- you must also set a value for @ProvisionedThroughputInMibps@.
    throughputMode :: Prelude.Maybe ThroughputMode,
    -- | The ID of the file system that you want to update.
    fileSystemId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFileSystem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'provisionedThroughputInMibps', 'updateFileSystem_provisionedThroughputInMibps' - (Optional) Sets the amount of provisioned throughput, in MiB\/s, for the
-- file system. Valid values are 1-1024. If you are changing the throughput
-- mode to provisioned, you must also provide the amount of provisioned
-- throughput. Required if @ThroughputMode@ is changed to @provisioned@ on
-- update.
--
-- 'throughputMode', 'updateFileSystem_throughputMode' - (Optional) Updates the file system\'s throughput mode. If you\'re not
-- updating your throughput mode, you don\'t need to provide this value in
-- your request. If you are changing the @ThroughputMode@ to @provisioned@,
-- you must also set a value for @ProvisionedThroughputInMibps@.
--
-- 'fileSystemId', 'updateFileSystem_fileSystemId' - The ID of the file system that you want to update.
newUpdateFileSystem ::
  -- | 'fileSystemId'
  Prelude.Text ->
  UpdateFileSystem
newUpdateFileSystem pFileSystemId_ =
  UpdateFileSystem'
    { provisionedThroughputInMibps =
        Prelude.Nothing,
      throughputMode = Prelude.Nothing,
      fileSystemId = pFileSystemId_
    }

-- | (Optional) Sets the amount of provisioned throughput, in MiB\/s, for the
-- file system. Valid values are 1-1024. If you are changing the throughput
-- mode to provisioned, you must also provide the amount of provisioned
-- throughput. Required if @ThroughputMode@ is changed to @provisioned@ on
-- update.
updateFileSystem_provisionedThroughputInMibps :: Lens.Lens' UpdateFileSystem (Prelude.Maybe Prelude.Double)
updateFileSystem_provisionedThroughputInMibps = Lens.lens (\UpdateFileSystem' {provisionedThroughputInMibps} -> provisionedThroughputInMibps) (\s@UpdateFileSystem' {} a -> s {provisionedThroughputInMibps = a} :: UpdateFileSystem)

-- | (Optional) Updates the file system\'s throughput mode. If you\'re not
-- updating your throughput mode, you don\'t need to provide this value in
-- your request. If you are changing the @ThroughputMode@ to @provisioned@,
-- you must also set a value for @ProvisionedThroughputInMibps@.
updateFileSystem_throughputMode :: Lens.Lens' UpdateFileSystem (Prelude.Maybe ThroughputMode)
updateFileSystem_throughputMode = Lens.lens (\UpdateFileSystem' {throughputMode} -> throughputMode) (\s@UpdateFileSystem' {} a -> s {throughputMode = a} :: UpdateFileSystem)

-- | The ID of the file system that you want to update.
updateFileSystem_fileSystemId :: Lens.Lens' UpdateFileSystem Prelude.Text
updateFileSystem_fileSystemId = Lens.lens (\UpdateFileSystem' {fileSystemId} -> fileSystemId) (\s@UpdateFileSystem' {} a -> s {fileSystemId = a} :: UpdateFileSystem)

instance Core.AWSRequest UpdateFileSystem where
  type
    AWSResponse UpdateFileSystem =
      FileSystemDescription
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable UpdateFileSystem where
  hashWithSalt _salt UpdateFileSystem' {..} =
    _salt
      `Prelude.hashWithSalt` provisionedThroughputInMibps
      `Prelude.hashWithSalt` throughputMode
      `Prelude.hashWithSalt` fileSystemId

instance Prelude.NFData UpdateFileSystem where
  rnf UpdateFileSystem' {..} =
    Prelude.rnf provisionedThroughputInMibps
      `Prelude.seq` Prelude.rnf throughputMode
      `Prelude.seq` Prelude.rnf fileSystemId

instance Data.ToHeaders UpdateFileSystem where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdateFileSystem where
  toJSON UpdateFileSystem' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ProvisionedThroughputInMibps" Data..=)
              Prelude.<$> provisionedThroughputInMibps,
            ("ThroughputMode" Data..=)
              Prelude.<$> throughputMode
          ]
      )

instance Data.ToPath UpdateFileSystem where
  toPath UpdateFileSystem' {..} =
    Prelude.mconcat
      ["/2015-02-01/file-systems/", Data.toBS fileSystemId]

instance Data.ToQuery UpdateFileSystem where
  toQuery = Prelude.const Prelude.mempty
