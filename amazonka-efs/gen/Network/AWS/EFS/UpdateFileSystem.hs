{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EFS.UpdateFileSystem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the throughput mode or the amount of provisioned throughput of
-- an existing file system.
module Network.AWS.EFS.UpdateFileSystem
  ( -- * Creating a Request
    UpdateFileSystem (..),
    newUpdateFileSystem,

    -- * Request Lenses
    updateFileSystem_throughputMode,
    updateFileSystem_provisionedThroughputInMibps,
    updateFileSystem_fileSystemId,

    -- * Destructuring the Response
    FileSystemDescription (..),
    newFileSystemDescription,

    -- * Response Lenses
    fileSystemDescription_throughputMode,
    fileSystemDescription_encrypted,
    fileSystemDescription_fileSystemArn,
    fileSystemDescription_provisionedThroughputInMibps,
    fileSystemDescription_kmsKeyId,
    fileSystemDescription_name,
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

import Network.AWS.EFS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateFileSystem' smart constructor.
data UpdateFileSystem = UpdateFileSystem'
  { -- | (Optional) The throughput mode that you want your file system to use. If
    -- you\'re not updating your throughput mode, you don\'t need to provide
    -- this value in your request. If you are changing the @ThroughputMode@ to
    -- @provisioned@, you must also set a value for
    -- @ProvisionedThroughputInMibps@.
    throughputMode :: Prelude.Maybe ThroughputMode,
    -- | (Optional) The amount of throughput, in MiB\/s, that you want to
    -- provision for your file system. Valid values are 1-1024. Required if
    -- @ThroughputMode@ is changed to @provisioned@ on update. If you\'re not
    -- updating the amount of provisioned throughput for your file system, you
    -- don\'t need to provide this value in your request.
    provisionedThroughputInMibps :: Prelude.Maybe Prelude.Double,
    -- | The ID of the file system that you want to update.
    fileSystemId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateFileSystem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'throughputMode', 'updateFileSystem_throughputMode' - (Optional) The throughput mode that you want your file system to use. If
-- you\'re not updating your throughput mode, you don\'t need to provide
-- this value in your request. If you are changing the @ThroughputMode@ to
-- @provisioned@, you must also set a value for
-- @ProvisionedThroughputInMibps@.
--
-- 'provisionedThroughputInMibps', 'updateFileSystem_provisionedThroughputInMibps' - (Optional) The amount of throughput, in MiB\/s, that you want to
-- provision for your file system. Valid values are 1-1024. Required if
-- @ThroughputMode@ is changed to @provisioned@ on update. If you\'re not
-- updating the amount of provisioned throughput for your file system, you
-- don\'t need to provide this value in your request.
--
-- 'fileSystemId', 'updateFileSystem_fileSystemId' - The ID of the file system that you want to update.
newUpdateFileSystem ::
  -- | 'fileSystemId'
  Prelude.Text ->
  UpdateFileSystem
newUpdateFileSystem pFileSystemId_ =
  UpdateFileSystem'
    { throughputMode = Prelude.Nothing,
      provisionedThroughputInMibps = Prelude.Nothing,
      fileSystemId = pFileSystemId_
    }

-- | (Optional) The throughput mode that you want your file system to use. If
-- you\'re not updating your throughput mode, you don\'t need to provide
-- this value in your request. If you are changing the @ThroughputMode@ to
-- @provisioned@, you must also set a value for
-- @ProvisionedThroughputInMibps@.
updateFileSystem_throughputMode :: Lens.Lens' UpdateFileSystem (Prelude.Maybe ThroughputMode)
updateFileSystem_throughputMode = Lens.lens (\UpdateFileSystem' {throughputMode} -> throughputMode) (\s@UpdateFileSystem' {} a -> s {throughputMode = a} :: UpdateFileSystem)

-- | (Optional) The amount of throughput, in MiB\/s, that you want to
-- provision for your file system. Valid values are 1-1024. Required if
-- @ThroughputMode@ is changed to @provisioned@ on update. If you\'re not
-- updating the amount of provisioned throughput for your file system, you
-- don\'t need to provide this value in your request.
updateFileSystem_provisionedThroughputInMibps :: Lens.Lens' UpdateFileSystem (Prelude.Maybe Prelude.Double)
updateFileSystem_provisionedThroughputInMibps = Lens.lens (\UpdateFileSystem' {provisionedThroughputInMibps} -> provisionedThroughputInMibps) (\s@UpdateFileSystem' {} a -> s {provisionedThroughputInMibps = a} :: UpdateFileSystem)

-- | The ID of the file system that you want to update.
updateFileSystem_fileSystemId :: Lens.Lens' UpdateFileSystem Prelude.Text
updateFileSystem_fileSystemId = Lens.lens (\UpdateFileSystem' {fileSystemId} -> fileSystemId) (\s@UpdateFileSystem' {} a -> s {fileSystemId = a} :: UpdateFileSystem)

instance Prelude.AWSRequest UpdateFileSystem where
  type Rs UpdateFileSystem = FileSystemDescription
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Prelude.eitherParseJSON x)

instance Prelude.Hashable UpdateFileSystem

instance Prelude.NFData UpdateFileSystem

instance Prelude.ToHeaders UpdateFileSystem where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON UpdateFileSystem where
  toJSON UpdateFileSystem' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ThroughputMode" Prelude..=)
              Prelude.<$> throughputMode,
            ("ProvisionedThroughputInMibps" Prelude..=)
              Prelude.<$> provisionedThroughputInMibps
          ]
      )

instance Prelude.ToPath UpdateFileSystem where
  toPath UpdateFileSystem' {..} =
    Prelude.mconcat
      [ "/2015-02-01/file-systems/",
        Prelude.toBS fileSystemId
      ]

instance Prelude.ToQuery UpdateFileSystem where
  toQuery = Prelude.const Prelude.mempty
