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
-- Module      : Amazonka.Nimble.GetStreamingSessionBackup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets @StreamingSessionBackup@ resource.
--
-- Invoke this operation to poll for a streaming session backup while
-- stopping a streaming session.
module Amazonka.Nimble.GetStreamingSessionBackup
  ( -- * Creating a Request
    GetStreamingSessionBackup (..),
    newGetStreamingSessionBackup,

    -- * Request Lenses
    getStreamingSessionBackup_backupId,
    getStreamingSessionBackup_studioId,

    -- * Destructuring the Response
    GetStreamingSessionBackupResponse (..),
    newGetStreamingSessionBackupResponse,

    -- * Response Lenses
    getStreamingSessionBackupResponse_streamingSessionBackup,
    getStreamingSessionBackupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Nimble.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetStreamingSessionBackup' smart constructor.
data GetStreamingSessionBackup = GetStreamingSessionBackup'
  { -- | The ID of the backup.
    backupId :: Prelude.Text,
    -- | The studio ID.
    studioId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetStreamingSessionBackup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupId', 'getStreamingSessionBackup_backupId' - The ID of the backup.
--
-- 'studioId', 'getStreamingSessionBackup_studioId' - The studio ID.
newGetStreamingSessionBackup ::
  -- | 'backupId'
  Prelude.Text ->
  -- | 'studioId'
  Prelude.Text ->
  GetStreamingSessionBackup
newGetStreamingSessionBackup pBackupId_ pStudioId_ =
  GetStreamingSessionBackup'
    { backupId = pBackupId_,
      studioId = pStudioId_
    }

-- | The ID of the backup.
getStreamingSessionBackup_backupId :: Lens.Lens' GetStreamingSessionBackup Prelude.Text
getStreamingSessionBackup_backupId = Lens.lens (\GetStreamingSessionBackup' {backupId} -> backupId) (\s@GetStreamingSessionBackup' {} a -> s {backupId = a} :: GetStreamingSessionBackup)

-- | The studio ID.
getStreamingSessionBackup_studioId :: Lens.Lens' GetStreamingSessionBackup Prelude.Text
getStreamingSessionBackup_studioId = Lens.lens (\GetStreamingSessionBackup' {studioId} -> studioId) (\s@GetStreamingSessionBackup' {} a -> s {studioId = a} :: GetStreamingSessionBackup)

instance Core.AWSRequest GetStreamingSessionBackup where
  type
    AWSResponse GetStreamingSessionBackup =
      GetStreamingSessionBackupResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetStreamingSessionBackupResponse'
            Prelude.<$> (x Data..?> "streamingSessionBackup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetStreamingSessionBackup where
  hashWithSalt _salt GetStreamingSessionBackup' {..} =
    _salt
      `Prelude.hashWithSalt` backupId
      `Prelude.hashWithSalt` studioId

instance Prelude.NFData GetStreamingSessionBackup where
  rnf GetStreamingSessionBackup' {..} =
    Prelude.rnf backupId `Prelude.seq`
      Prelude.rnf studioId

instance Data.ToHeaders GetStreamingSessionBackup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetStreamingSessionBackup where
  toPath GetStreamingSessionBackup' {..} =
    Prelude.mconcat
      [ "/2020-08-01/studios/",
        Data.toBS studioId,
        "/streaming-session-backups/",
        Data.toBS backupId
      ]

instance Data.ToQuery GetStreamingSessionBackup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetStreamingSessionBackupResponse' smart constructor.
data GetStreamingSessionBackupResponse = GetStreamingSessionBackupResponse'
  { -- | Information about the streaming session backup.
    streamingSessionBackup :: Prelude.Maybe StreamingSessionBackup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetStreamingSessionBackupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamingSessionBackup', 'getStreamingSessionBackupResponse_streamingSessionBackup' - Information about the streaming session backup.
--
-- 'httpStatus', 'getStreamingSessionBackupResponse_httpStatus' - The response's http status code.
newGetStreamingSessionBackupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetStreamingSessionBackupResponse
newGetStreamingSessionBackupResponse pHttpStatus_ =
  GetStreamingSessionBackupResponse'
    { streamingSessionBackup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the streaming session backup.
getStreamingSessionBackupResponse_streamingSessionBackup :: Lens.Lens' GetStreamingSessionBackupResponse (Prelude.Maybe StreamingSessionBackup)
getStreamingSessionBackupResponse_streamingSessionBackup = Lens.lens (\GetStreamingSessionBackupResponse' {streamingSessionBackup} -> streamingSessionBackup) (\s@GetStreamingSessionBackupResponse' {} a -> s {streamingSessionBackup = a} :: GetStreamingSessionBackupResponse)

-- | The response's http status code.
getStreamingSessionBackupResponse_httpStatus :: Lens.Lens' GetStreamingSessionBackupResponse Prelude.Int
getStreamingSessionBackupResponse_httpStatus = Lens.lens (\GetStreamingSessionBackupResponse' {httpStatus} -> httpStatus) (\s@GetStreamingSessionBackupResponse' {} a -> s {httpStatus = a} :: GetStreamingSessionBackupResponse)

instance
  Prelude.NFData
    GetStreamingSessionBackupResponse
  where
  rnf GetStreamingSessionBackupResponse' {..} =
    Prelude.rnf streamingSessionBackup `Prelude.seq`
      Prelude.rnf httpStatus
