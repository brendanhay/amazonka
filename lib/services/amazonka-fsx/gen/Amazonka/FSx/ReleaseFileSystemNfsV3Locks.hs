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
-- Module      : Amazonka.FSx.ReleaseFileSystemNfsV3Locks
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Releases the file system lock from an Amazon FSx for OpenZFS file
-- system.
module Amazonka.FSx.ReleaseFileSystemNfsV3Locks
  ( -- * Creating a Request
    ReleaseFileSystemNfsV3Locks (..),
    newReleaseFileSystemNfsV3Locks,

    -- * Request Lenses
    releaseFileSystemNfsV3Locks_clientRequestToken,
    releaseFileSystemNfsV3Locks_fileSystemId,

    -- * Destructuring the Response
    ReleaseFileSystemNfsV3LocksResponse (..),
    newReleaseFileSystemNfsV3LocksResponse,

    -- * Response Lenses
    releaseFileSystemNfsV3LocksResponse_fileSystem,
    releaseFileSystemNfsV3LocksResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newReleaseFileSystemNfsV3Locks' smart constructor.
data ReleaseFileSystemNfsV3Locks = ReleaseFileSystemNfsV3Locks'
  { clientRequestToken :: Prelude.Maybe Prelude.Text,
    fileSystemId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReleaseFileSystemNfsV3Locks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'releaseFileSystemNfsV3Locks_clientRequestToken' - Undocumented member.
--
-- 'fileSystemId', 'releaseFileSystemNfsV3Locks_fileSystemId' - Undocumented member.
newReleaseFileSystemNfsV3Locks ::
  -- | 'fileSystemId'
  Prelude.Text ->
  ReleaseFileSystemNfsV3Locks
newReleaseFileSystemNfsV3Locks pFileSystemId_ =
  ReleaseFileSystemNfsV3Locks'
    { clientRequestToken =
        Prelude.Nothing,
      fileSystemId = pFileSystemId_
    }

-- | Undocumented member.
releaseFileSystemNfsV3Locks_clientRequestToken :: Lens.Lens' ReleaseFileSystemNfsV3Locks (Prelude.Maybe Prelude.Text)
releaseFileSystemNfsV3Locks_clientRequestToken = Lens.lens (\ReleaseFileSystemNfsV3Locks' {clientRequestToken} -> clientRequestToken) (\s@ReleaseFileSystemNfsV3Locks' {} a -> s {clientRequestToken = a} :: ReleaseFileSystemNfsV3Locks)

-- | Undocumented member.
releaseFileSystemNfsV3Locks_fileSystemId :: Lens.Lens' ReleaseFileSystemNfsV3Locks Prelude.Text
releaseFileSystemNfsV3Locks_fileSystemId = Lens.lens (\ReleaseFileSystemNfsV3Locks' {fileSystemId} -> fileSystemId) (\s@ReleaseFileSystemNfsV3Locks' {} a -> s {fileSystemId = a} :: ReleaseFileSystemNfsV3Locks)

instance Core.AWSRequest ReleaseFileSystemNfsV3Locks where
  type
    AWSResponse ReleaseFileSystemNfsV3Locks =
      ReleaseFileSystemNfsV3LocksResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ReleaseFileSystemNfsV3LocksResponse'
            Prelude.<$> (x Data..?> "FileSystem")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ReleaseFileSystemNfsV3Locks where
  hashWithSalt _salt ReleaseFileSystemNfsV3Locks' {..} =
    _salt
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` fileSystemId

instance Prelude.NFData ReleaseFileSystemNfsV3Locks where
  rnf ReleaseFileSystemNfsV3Locks' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf fileSystemId

instance Data.ToHeaders ReleaseFileSystemNfsV3Locks where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSSimbaAPIService_v20180301.ReleaseFileSystemNfsV3Locks" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ReleaseFileSystemNfsV3Locks where
  toJSON ReleaseFileSystemNfsV3Locks' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            Prelude.Just ("FileSystemId" Data..= fileSystemId)
          ]
      )

instance Data.ToPath ReleaseFileSystemNfsV3Locks where
  toPath = Prelude.const "/"

instance Data.ToQuery ReleaseFileSystemNfsV3Locks where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newReleaseFileSystemNfsV3LocksResponse' smart constructor.
data ReleaseFileSystemNfsV3LocksResponse = ReleaseFileSystemNfsV3LocksResponse'
  { fileSystem :: Prelude.Maybe FileSystem,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReleaseFileSystemNfsV3LocksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileSystem', 'releaseFileSystemNfsV3LocksResponse_fileSystem' - Undocumented member.
--
-- 'httpStatus', 'releaseFileSystemNfsV3LocksResponse_httpStatus' - The response's http status code.
newReleaseFileSystemNfsV3LocksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ReleaseFileSystemNfsV3LocksResponse
newReleaseFileSystemNfsV3LocksResponse pHttpStatus_ =
  ReleaseFileSystemNfsV3LocksResponse'
    { fileSystem =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
releaseFileSystemNfsV3LocksResponse_fileSystem :: Lens.Lens' ReleaseFileSystemNfsV3LocksResponse (Prelude.Maybe FileSystem)
releaseFileSystemNfsV3LocksResponse_fileSystem = Lens.lens (\ReleaseFileSystemNfsV3LocksResponse' {fileSystem} -> fileSystem) (\s@ReleaseFileSystemNfsV3LocksResponse' {} a -> s {fileSystem = a} :: ReleaseFileSystemNfsV3LocksResponse)

-- | The response's http status code.
releaseFileSystemNfsV3LocksResponse_httpStatus :: Lens.Lens' ReleaseFileSystemNfsV3LocksResponse Prelude.Int
releaseFileSystemNfsV3LocksResponse_httpStatus = Lens.lens (\ReleaseFileSystemNfsV3LocksResponse' {httpStatus} -> httpStatus) (\s@ReleaseFileSystemNfsV3LocksResponse' {} a -> s {httpStatus = a} :: ReleaseFileSystemNfsV3LocksResponse)

instance
  Prelude.NFData
    ReleaseFileSystemNfsV3LocksResponse
  where
  rnf ReleaseFileSystemNfsV3LocksResponse' {..} =
    Prelude.rnf fileSystem
      `Prelude.seq` Prelude.rnf httpStatus
