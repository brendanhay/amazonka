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
-- Module      : Amazonka.QLDB.DescribeJournalS3Export
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a journal export job, including the ledger
-- name, export ID, creation time, current status, and the parameters of
-- the original export creation request.
--
-- This action does not return any expired export jobs. For more
-- information, see
-- <https://docs.aws.amazon.com/qldb/latest/developerguide/export-journal.request.html#export-journal.request.expiration Export job expiration>
-- in the /Amazon QLDB Developer Guide/.
--
-- If the export job with the given @ExportId@ doesn\'t exist, then throws
-- @ResourceNotFoundException@.
--
-- If the ledger with the given @Name@ doesn\'t exist, then throws
-- @ResourceNotFoundException@.
module Amazonka.QLDB.DescribeJournalS3Export
  ( -- * Creating a Request
    DescribeJournalS3Export (..),
    newDescribeJournalS3Export,

    -- * Request Lenses
    describeJournalS3Export_name,
    describeJournalS3Export_exportId,

    -- * Destructuring the Response
    DescribeJournalS3ExportResponse (..),
    newDescribeJournalS3ExportResponse,

    -- * Response Lenses
    describeJournalS3ExportResponse_httpStatus,
    describeJournalS3ExportResponse_exportDescription,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QLDB.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeJournalS3Export' smart constructor.
data DescribeJournalS3Export = DescribeJournalS3Export'
  { -- | The name of the ledger.
    name :: Prelude.Text,
    -- | The UUID (represented in Base62-encoded text) of the journal export job
    -- to describe.
    exportId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeJournalS3Export' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'describeJournalS3Export_name' - The name of the ledger.
--
-- 'exportId', 'describeJournalS3Export_exportId' - The UUID (represented in Base62-encoded text) of the journal export job
-- to describe.
newDescribeJournalS3Export ::
  -- | 'name'
  Prelude.Text ->
  -- | 'exportId'
  Prelude.Text ->
  DescribeJournalS3Export
newDescribeJournalS3Export pName_ pExportId_ =
  DescribeJournalS3Export'
    { name = pName_,
      exportId = pExportId_
    }

-- | The name of the ledger.
describeJournalS3Export_name :: Lens.Lens' DescribeJournalS3Export Prelude.Text
describeJournalS3Export_name = Lens.lens (\DescribeJournalS3Export' {name} -> name) (\s@DescribeJournalS3Export' {} a -> s {name = a} :: DescribeJournalS3Export)

-- | The UUID (represented in Base62-encoded text) of the journal export job
-- to describe.
describeJournalS3Export_exportId :: Lens.Lens' DescribeJournalS3Export Prelude.Text
describeJournalS3Export_exportId = Lens.lens (\DescribeJournalS3Export' {exportId} -> exportId) (\s@DescribeJournalS3Export' {} a -> s {exportId = a} :: DescribeJournalS3Export)

instance Core.AWSRequest DescribeJournalS3Export where
  type
    AWSResponse DescribeJournalS3Export =
      DescribeJournalS3ExportResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeJournalS3ExportResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "ExportDescription")
      )

instance Prelude.Hashable DescribeJournalS3Export where
  hashWithSalt _salt DescribeJournalS3Export' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` exportId

instance Prelude.NFData DescribeJournalS3Export where
  rnf DescribeJournalS3Export' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf exportId

instance Core.ToHeaders DescribeJournalS3Export where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeJournalS3Export where
  toPath DescribeJournalS3Export' {..} =
    Prelude.mconcat
      [ "/ledgers/",
        Core.toBS name,
        "/journal-s3-exports/",
        Core.toBS exportId
      ]

instance Core.ToQuery DescribeJournalS3Export where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeJournalS3ExportResponse' smart constructor.
data DescribeJournalS3ExportResponse = DescribeJournalS3ExportResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information about the journal export job returned by a
    -- @DescribeJournalS3Export@ request.
    exportDescription :: JournalS3ExportDescription
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeJournalS3ExportResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeJournalS3ExportResponse_httpStatus' - The response's http status code.
--
-- 'exportDescription', 'describeJournalS3ExportResponse_exportDescription' - Information about the journal export job returned by a
-- @DescribeJournalS3Export@ request.
newDescribeJournalS3ExportResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'exportDescription'
  JournalS3ExportDescription ->
  DescribeJournalS3ExportResponse
newDescribeJournalS3ExportResponse
  pHttpStatus_
  pExportDescription_ =
    DescribeJournalS3ExportResponse'
      { httpStatus =
          pHttpStatus_,
        exportDescription = pExportDescription_
      }

-- | The response's http status code.
describeJournalS3ExportResponse_httpStatus :: Lens.Lens' DescribeJournalS3ExportResponse Prelude.Int
describeJournalS3ExportResponse_httpStatus = Lens.lens (\DescribeJournalS3ExportResponse' {httpStatus} -> httpStatus) (\s@DescribeJournalS3ExportResponse' {} a -> s {httpStatus = a} :: DescribeJournalS3ExportResponse)

-- | Information about the journal export job returned by a
-- @DescribeJournalS3Export@ request.
describeJournalS3ExportResponse_exportDescription :: Lens.Lens' DescribeJournalS3ExportResponse JournalS3ExportDescription
describeJournalS3ExportResponse_exportDescription = Lens.lens (\DescribeJournalS3ExportResponse' {exportDescription} -> exportDescription) (\s@DescribeJournalS3ExportResponse' {} a -> s {exportDescription = a} :: DescribeJournalS3ExportResponse)

instance
  Prelude.NFData
    DescribeJournalS3ExportResponse
  where
  rnf DescribeJournalS3ExportResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf exportDescription
