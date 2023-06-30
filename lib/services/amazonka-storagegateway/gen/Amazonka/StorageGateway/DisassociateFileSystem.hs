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
-- Module      : Amazonka.StorageGateway.DisassociateFileSystem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates an Amazon FSx file system from the specified gateway.
-- After the disassociation process finishes, the gateway can no longer
-- access the Amazon FSx file system. This operation is only supported in
-- the FSx File Gateway type.
module Amazonka.StorageGateway.DisassociateFileSystem
  ( -- * Creating a Request
    DisassociateFileSystem (..),
    newDisassociateFileSystem,

    -- * Request Lenses
    disassociateFileSystem_forceDelete,
    disassociateFileSystem_fileSystemAssociationARN,

    -- * Destructuring the Response
    DisassociateFileSystemResponse (..),
    newDisassociateFileSystemResponse,

    -- * Response Lenses
    disassociateFileSystemResponse_fileSystemAssociationARN,
    disassociateFileSystemResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StorageGateway.Types

-- | /See:/ 'newDisassociateFileSystem' smart constructor.
data DisassociateFileSystem = DisassociateFileSystem'
  { -- | If this value is set to true, the operation disassociates an Amazon FSx
    -- file system immediately. It ends all data uploads to the file system,
    -- and the file system association enters the @FORCE_DELETING@ status. If
    -- this value is set to false, the Amazon FSx file system does not
    -- disassociate until all data is uploaded.
    forceDelete :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the file system association to be
    -- deleted.
    fileSystemAssociationARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateFileSystem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'forceDelete', 'disassociateFileSystem_forceDelete' - If this value is set to true, the operation disassociates an Amazon FSx
-- file system immediately. It ends all data uploads to the file system,
-- and the file system association enters the @FORCE_DELETING@ status. If
-- this value is set to false, the Amazon FSx file system does not
-- disassociate until all data is uploaded.
--
-- 'fileSystemAssociationARN', 'disassociateFileSystem_fileSystemAssociationARN' - The Amazon Resource Name (ARN) of the file system association to be
-- deleted.
newDisassociateFileSystem ::
  -- | 'fileSystemAssociationARN'
  Prelude.Text ->
  DisassociateFileSystem
newDisassociateFileSystem pFileSystemAssociationARN_ =
  DisassociateFileSystem'
    { forceDelete =
        Prelude.Nothing,
      fileSystemAssociationARN =
        pFileSystemAssociationARN_
    }

-- | If this value is set to true, the operation disassociates an Amazon FSx
-- file system immediately. It ends all data uploads to the file system,
-- and the file system association enters the @FORCE_DELETING@ status. If
-- this value is set to false, the Amazon FSx file system does not
-- disassociate until all data is uploaded.
disassociateFileSystem_forceDelete :: Lens.Lens' DisassociateFileSystem (Prelude.Maybe Prelude.Bool)
disassociateFileSystem_forceDelete = Lens.lens (\DisassociateFileSystem' {forceDelete} -> forceDelete) (\s@DisassociateFileSystem' {} a -> s {forceDelete = a} :: DisassociateFileSystem)

-- | The Amazon Resource Name (ARN) of the file system association to be
-- deleted.
disassociateFileSystem_fileSystemAssociationARN :: Lens.Lens' DisassociateFileSystem Prelude.Text
disassociateFileSystem_fileSystemAssociationARN = Lens.lens (\DisassociateFileSystem' {fileSystemAssociationARN} -> fileSystemAssociationARN) (\s@DisassociateFileSystem' {} a -> s {fileSystemAssociationARN = a} :: DisassociateFileSystem)

instance Core.AWSRequest DisassociateFileSystem where
  type
    AWSResponse DisassociateFileSystem =
      DisassociateFileSystemResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DisassociateFileSystemResponse'
            Prelude.<$> (x Data..?> "FileSystemAssociationARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisassociateFileSystem where
  hashWithSalt _salt DisassociateFileSystem' {..} =
    _salt
      `Prelude.hashWithSalt` forceDelete
      `Prelude.hashWithSalt` fileSystemAssociationARN

instance Prelude.NFData DisassociateFileSystem where
  rnf DisassociateFileSystem' {..} =
    Prelude.rnf forceDelete
      `Prelude.seq` Prelude.rnf fileSystemAssociationARN

instance Data.ToHeaders DisassociateFileSystem where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StorageGateway_20130630.DisassociateFileSystem" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisassociateFileSystem where
  toJSON DisassociateFileSystem' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ForceDelete" Data..=) Prelude.<$> forceDelete,
            Prelude.Just
              ( "FileSystemAssociationARN"
                  Data..= fileSystemAssociationARN
              )
          ]
      )

instance Data.ToPath DisassociateFileSystem where
  toPath = Prelude.const "/"

instance Data.ToQuery DisassociateFileSystem where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateFileSystemResponse' smart constructor.
data DisassociateFileSystemResponse = DisassociateFileSystemResponse'
  { -- | The Amazon Resource Name (ARN) of the deleted file system association.
    fileSystemAssociationARN :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateFileSystemResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileSystemAssociationARN', 'disassociateFileSystemResponse_fileSystemAssociationARN' - The Amazon Resource Name (ARN) of the deleted file system association.
--
-- 'httpStatus', 'disassociateFileSystemResponse_httpStatus' - The response's http status code.
newDisassociateFileSystemResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateFileSystemResponse
newDisassociateFileSystemResponse pHttpStatus_ =
  DisassociateFileSystemResponse'
    { fileSystemAssociationARN =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the deleted file system association.
disassociateFileSystemResponse_fileSystemAssociationARN :: Lens.Lens' DisassociateFileSystemResponse (Prelude.Maybe Prelude.Text)
disassociateFileSystemResponse_fileSystemAssociationARN = Lens.lens (\DisassociateFileSystemResponse' {fileSystemAssociationARN} -> fileSystemAssociationARN) (\s@DisassociateFileSystemResponse' {} a -> s {fileSystemAssociationARN = a} :: DisassociateFileSystemResponse)

-- | The response's http status code.
disassociateFileSystemResponse_httpStatus :: Lens.Lens' DisassociateFileSystemResponse Prelude.Int
disassociateFileSystemResponse_httpStatus = Lens.lens (\DisassociateFileSystemResponse' {httpStatus} -> httpStatus) (\s@DisassociateFileSystemResponse' {} a -> s {httpStatus = a} :: DisassociateFileSystemResponse)

instance
  Prelude.NFData
    DisassociateFileSystemResponse
  where
  rnf DisassociateFileSystemResponse' {..} =
    Prelude.rnf fileSystemAssociationARN
      `Prelude.seq` Prelude.rnf httpStatus
