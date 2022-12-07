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
-- Module      : Amazonka.GamesParks.ExportSnapshot
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Exports a game configuration snapshot.
module Amazonka.GamesParks.ExportSnapshot
  ( -- * Creating a Request
    ExportSnapshot (..),
    newExportSnapshot,

    -- * Request Lenses
    exportSnapshot_gameName,
    exportSnapshot_snapshotId,

    -- * Destructuring the Response
    ExportSnapshotResponse (..),
    newExportSnapshotResponse,

    -- * Response Lenses
    exportSnapshotResponse_s3Url,
    exportSnapshotResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GamesParks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newExportSnapshot' smart constructor.
data ExportSnapshot = ExportSnapshot'
  { -- | The name of the game.
    gameName :: Prelude.Text,
    -- | The identifier of the snapshot to export.
    snapshotId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gameName', 'exportSnapshot_gameName' - The name of the game.
--
-- 'snapshotId', 'exportSnapshot_snapshotId' - The identifier of the snapshot to export.
newExportSnapshot ::
  -- | 'gameName'
  Prelude.Text ->
  -- | 'snapshotId'
  Prelude.Text ->
  ExportSnapshot
newExportSnapshot pGameName_ pSnapshotId_ =
  ExportSnapshot'
    { gameName = pGameName_,
      snapshotId = pSnapshotId_
    }

-- | The name of the game.
exportSnapshot_gameName :: Lens.Lens' ExportSnapshot Prelude.Text
exportSnapshot_gameName = Lens.lens (\ExportSnapshot' {gameName} -> gameName) (\s@ExportSnapshot' {} a -> s {gameName = a} :: ExportSnapshot)

-- | The identifier of the snapshot to export.
exportSnapshot_snapshotId :: Lens.Lens' ExportSnapshot Prelude.Text
exportSnapshot_snapshotId = Lens.lens (\ExportSnapshot' {snapshotId} -> snapshotId) (\s@ExportSnapshot' {} a -> s {snapshotId = a} :: ExportSnapshot)

instance Core.AWSRequest ExportSnapshot where
  type
    AWSResponse ExportSnapshot =
      ExportSnapshotResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ExportSnapshotResponse'
            Prelude.<$> (x Data..?> "S3Url")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ExportSnapshot where
  hashWithSalt _salt ExportSnapshot' {..} =
    _salt `Prelude.hashWithSalt` gameName
      `Prelude.hashWithSalt` snapshotId

instance Prelude.NFData ExportSnapshot where
  rnf ExportSnapshot' {..} =
    Prelude.rnf gameName
      `Prelude.seq` Prelude.rnf snapshotId

instance Data.ToHeaders ExportSnapshot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ExportSnapshot where
  toPath ExportSnapshot' {..} =
    Prelude.mconcat
      [ "/game/",
        Data.toBS gameName,
        "/snapshot/",
        Data.toBS snapshotId,
        "/export"
      ]

instance Data.ToQuery ExportSnapshot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newExportSnapshotResponse' smart constructor.
data ExportSnapshotResponse = ExportSnapshotResponse'
  { -- | The presigned URL for the snapshot data.
    --
    -- This URL will be available for 10 minutes, and can be used to download
    -- the snapshot content. If the URL expires, a new one can be requested
    -- using the same operation.
    s3Url :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportSnapshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Url', 'exportSnapshotResponse_s3Url' - The presigned URL for the snapshot data.
--
-- This URL will be available for 10 minutes, and can be used to download
-- the snapshot content. If the URL expires, a new one can be requested
-- using the same operation.
--
-- 'httpStatus', 'exportSnapshotResponse_httpStatus' - The response's http status code.
newExportSnapshotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ExportSnapshotResponse
newExportSnapshotResponse pHttpStatus_ =
  ExportSnapshotResponse'
    { s3Url = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The presigned URL for the snapshot data.
--
-- This URL will be available for 10 minutes, and can be used to download
-- the snapshot content. If the URL expires, a new one can be requested
-- using the same operation.
exportSnapshotResponse_s3Url :: Lens.Lens' ExportSnapshotResponse (Prelude.Maybe Prelude.Text)
exportSnapshotResponse_s3Url = Lens.lens (\ExportSnapshotResponse' {s3Url} -> s3Url) (\s@ExportSnapshotResponse' {} a -> s {s3Url = a} :: ExportSnapshotResponse)

-- | The response's http status code.
exportSnapshotResponse_httpStatus :: Lens.Lens' ExportSnapshotResponse Prelude.Int
exportSnapshotResponse_httpStatus = Lens.lens (\ExportSnapshotResponse' {httpStatus} -> httpStatus) (\s@ExportSnapshotResponse' {} a -> s {httpStatus = a} :: ExportSnapshotResponse)

instance Prelude.NFData ExportSnapshotResponse where
  rnf ExportSnapshotResponse' {..} =
    Prelude.rnf s3Url
      `Prelude.seq` Prelude.rnf httpStatus
