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
-- Module      : Amazonka.Forecast.DescribeExplainabilityExport
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an Explainability export created using the
-- CreateExplainabilityExport operation.
module Amazonka.Forecast.DescribeExplainabilityExport
  ( -- * Creating a Request
    DescribeExplainabilityExport (..),
    newDescribeExplainabilityExport,

    -- * Request Lenses
    describeExplainabilityExport_explainabilityExportArn,

    -- * Destructuring the Response
    DescribeExplainabilityExportResponse (..),
    newDescribeExplainabilityExportResponse,

    -- * Response Lenses
    describeExplainabilityExportResponse_lastModificationTime,
    describeExplainabilityExportResponse_destination,
    describeExplainabilityExportResponse_explainabilityExportName,
    describeExplainabilityExportResponse_message,
    describeExplainabilityExportResponse_format,
    describeExplainabilityExportResponse_explainabilityExportArn,
    describeExplainabilityExportResponse_status,
    describeExplainabilityExportResponse_explainabilityArn,
    describeExplainabilityExportResponse_creationTime,
    describeExplainabilityExportResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Forecast.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeExplainabilityExport' smart constructor.
data DescribeExplainabilityExport = DescribeExplainabilityExport'
  { -- | The Amazon Resource Name (ARN) of the Explainability export.
    explainabilityExportArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeExplainabilityExport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'explainabilityExportArn', 'describeExplainabilityExport_explainabilityExportArn' - The Amazon Resource Name (ARN) of the Explainability export.
newDescribeExplainabilityExport ::
  -- | 'explainabilityExportArn'
  Prelude.Text ->
  DescribeExplainabilityExport
newDescribeExplainabilityExport
  pExplainabilityExportArn_ =
    DescribeExplainabilityExport'
      { explainabilityExportArn =
          pExplainabilityExportArn_
      }

-- | The Amazon Resource Name (ARN) of the Explainability export.
describeExplainabilityExport_explainabilityExportArn :: Lens.Lens' DescribeExplainabilityExport Prelude.Text
describeExplainabilityExport_explainabilityExportArn = Lens.lens (\DescribeExplainabilityExport' {explainabilityExportArn} -> explainabilityExportArn) (\s@DescribeExplainabilityExport' {} a -> s {explainabilityExportArn = a} :: DescribeExplainabilityExport)

instance Core.AWSRequest DescribeExplainabilityExport where
  type
    AWSResponse DescribeExplainabilityExport =
      DescribeExplainabilityExportResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeExplainabilityExportResponse'
            Prelude.<$> (x Core..?> "LastModificationTime")
            Prelude.<*> (x Core..?> "Destination")
            Prelude.<*> (x Core..?> "ExplainabilityExportName")
            Prelude.<*> (x Core..?> "Message")
            Prelude.<*> (x Core..?> "Format")
            Prelude.<*> (x Core..?> "ExplainabilityExportArn")
            Prelude.<*> (x Core..?> "Status")
            Prelude.<*> (x Core..?> "ExplainabilityArn")
            Prelude.<*> (x Core..?> "CreationTime")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeExplainabilityExport
  where
  hashWithSalt _salt DescribeExplainabilityExport' {..} =
    _salt
      `Prelude.hashWithSalt` explainabilityExportArn

instance Prelude.NFData DescribeExplainabilityExport where
  rnf DescribeExplainabilityExport' {..} =
    Prelude.rnf explainabilityExportArn

instance Core.ToHeaders DescribeExplainabilityExport where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonForecast.DescribeExplainabilityExport" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeExplainabilityExport where
  toJSON DescribeExplainabilityExport' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ExplainabilityExportArn"
                  Core..= explainabilityExportArn
              )
          ]
      )

instance Core.ToPath DescribeExplainabilityExport where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeExplainabilityExport where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeExplainabilityExportResponse' smart constructor.
data DescribeExplainabilityExportResponse = DescribeExplainabilityExportResponse'
  { -- | The last time the resource was modified. The timestamp depends on the
    -- status of the job:
    --
    -- -   @CREATE_PENDING@ - The @CreationTime@.
    --
    -- -   @CREATE_IN_PROGRESS@ - The current timestamp.
    --
    -- -   @CREATE_STOPPING@ - The current timestamp.
    --
    -- -   @CREATE_STOPPED@ - When the job stopped.
    --
    -- -   @ACTIVE@ or @CREATE_FAILED@ - When the job finished or failed.
    lastModificationTime :: Prelude.Maybe Core.POSIX,
    destination :: Prelude.Maybe DataDestination,
    -- | The name of the Explainability export.
    explainabilityExportName :: Prelude.Maybe Prelude.Text,
    -- | Information about any errors that occurred during the export.
    message :: Prelude.Maybe Prelude.Text,
    -- | The format of the exported data, CSV or PARQUET.
    format :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Explainability export.
    explainabilityExportArn :: Prelude.Maybe Prelude.Text,
    -- | The status of the Explainability export. States include:
    --
    -- -   @ACTIVE@
    --
    -- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
    --
    -- -   @CREATE_STOPPING@, @CREATE_STOPPED@
    --
    -- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
    status :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Explainability export.
    explainabilityArn :: Prelude.Maybe Prelude.Text,
    -- | When the Explainability export was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeExplainabilityExportResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModificationTime', 'describeExplainabilityExportResponse_lastModificationTime' - The last time the resource was modified. The timestamp depends on the
-- status of the job:
--
-- -   @CREATE_PENDING@ - The @CreationTime@.
--
-- -   @CREATE_IN_PROGRESS@ - The current timestamp.
--
-- -   @CREATE_STOPPING@ - The current timestamp.
--
-- -   @CREATE_STOPPED@ - When the job stopped.
--
-- -   @ACTIVE@ or @CREATE_FAILED@ - When the job finished or failed.
--
-- 'destination', 'describeExplainabilityExportResponse_destination' - Undocumented member.
--
-- 'explainabilityExportName', 'describeExplainabilityExportResponse_explainabilityExportName' - The name of the Explainability export.
--
-- 'message', 'describeExplainabilityExportResponse_message' - Information about any errors that occurred during the export.
--
-- 'format', 'describeExplainabilityExportResponse_format' - The format of the exported data, CSV or PARQUET.
--
-- 'explainabilityExportArn', 'describeExplainabilityExportResponse_explainabilityExportArn' - The Amazon Resource Name (ARN) of the Explainability export.
--
-- 'status', 'describeExplainabilityExportResponse_status' - The status of the Explainability export. States include:
--
-- -   @ACTIVE@
--
-- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
--
-- -   @CREATE_STOPPING@, @CREATE_STOPPED@
--
-- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
--
-- 'explainabilityArn', 'describeExplainabilityExportResponse_explainabilityArn' - The Amazon Resource Name (ARN) of the Explainability export.
--
-- 'creationTime', 'describeExplainabilityExportResponse_creationTime' - When the Explainability export was created.
--
-- 'httpStatus', 'describeExplainabilityExportResponse_httpStatus' - The response's http status code.
newDescribeExplainabilityExportResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeExplainabilityExportResponse
newDescribeExplainabilityExportResponse pHttpStatus_ =
  DescribeExplainabilityExportResponse'
    { lastModificationTime =
        Prelude.Nothing,
      destination = Prelude.Nothing,
      explainabilityExportName =
        Prelude.Nothing,
      message = Prelude.Nothing,
      format = Prelude.Nothing,
      explainabilityExportArn =
        Prelude.Nothing,
      status = Prelude.Nothing,
      explainabilityArn = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The last time the resource was modified. The timestamp depends on the
-- status of the job:
--
-- -   @CREATE_PENDING@ - The @CreationTime@.
--
-- -   @CREATE_IN_PROGRESS@ - The current timestamp.
--
-- -   @CREATE_STOPPING@ - The current timestamp.
--
-- -   @CREATE_STOPPED@ - When the job stopped.
--
-- -   @ACTIVE@ or @CREATE_FAILED@ - When the job finished or failed.
describeExplainabilityExportResponse_lastModificationTime :: Lens.Lens' DescribeExplainabilityExportResponse (Prelude.Maybe Prelude.UTCTime)
describeExplainabilityExportResponse_lastModificationTime = Lens.lens (\DescribeExplainabilityExportResponse' {lastModificationTime} -> lastModificationTime) (\s@DescribeExplainabilityExportResponse' {} a -> s {lastModificationTime = a} :: DescribeExplainabilityExportResponse) Prelude.. Lens.mapping Core._Time

-- | Undocumented member.
describeExplainabilityExportResponse_destination :: Lens.Lens' DescribeExplainabilityExportResponse (Prelude.Maybe DataDestination)
describeExplainabilityExportResponse_destination = Lens.lens (\DescribeExplainabilityExportResponse' {destination} -> destination) (\s@DescribeExplainabilityExportResponse' {} a -> s {destination = a} :: DescribeExplainabilityExportResponse)

-- | The name of the Explainability export.
describeExplainabilityExportResponse_explainabilityExportName :: Lens.Lens' DescribeExplainabilityExportResponse (Prelude.Maybe Prelude.Text)
describeExplainabilityExportResponse_explainabilityExportName = Lens.lens (\DescribeExplainabilityExportResponse' {explainabilityExportName} -> explainabilityExportName) (\s@DescribeExplainabilityExportResponse' {} a -> s {explainabilityExportName = a} :: DescribeExplainabilityExportResponse)

-- | Information about any errors that occurred during the export.
describeExplainabilityExportResponse_message :: Lens.Lens' DescribeExplainabilityExportResponse (Prelude.Maybe Prelude.Text)
describeExplainabilityExportResponse_message = Lens.lens (\DescribeExplainabilityExportResponse' {message} -> message) (\s@DescribeExplainabilityExportResponse' {} a -> s {message = a} :: DescribeExplainabilityExportResponse)

-- | The format of the exported data, CSV or PARQUET.
describeExplainabilityExportResponse_format :: Lens.Lens' DescribeExplainabilityExportResponse (Prelude.Maybe Prelude.Text)
describeExplainabilityExportResponse_format = Lens.lens (\DescribeExplainabilityExportResponse' {format} -> format) (\s@DescribeExplainabilityExportResponse' {} a -> s {format = a} :: DescribeExplainabilityExportResponse)

-- | The Amazon Resource Name (ARN) of the Explainability export.
describeExplainabilityExportResponse_explainabilityExportArn :: Lens.Lens' DescribeExplainabilityExportResponse (Prelude.Maybe Prelude.Text)
describeExplainabilityExportResponse_explainabilityExportArn = Lens.lens (\DescribeExplainabilityExportResponse' {explainabilityExportArn} -> explainabilityExportArn) (\s@DescribeExplainabilityExportResponse' {} a -> s {explainabilityExportArn = a} :: DescribeExplainabilityExportResponse)

-- | The status of the Explainability export. States include:
--
-- -   @ACTIVE@
--
-- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
--
-- -   @CREATE_STOPPING@, @CREATE_STOPPED@
--
-- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
describeExplainabilityExportResponse_status :: Lens.Lens' DescribeExplainabilityExportResponse (Prelude.Maybe Prelude.Text)
describeExplainabilityExportResponse_status = Lens.lens (\DescribeExplainabilityExportResponse' {status} -> status) (\s@DescribeExplainabilityExportResponse' {} a -> s {status = a} :: DescribeExplainabilityExportResponse)

-- | The Amazon Resource Name (ARN) of the Explainability export.
describeExplainabilityExportResponse_explainabilityArn :: Lens.Lens' DescribeExplainabilityExportResponse (Prelude.Maybe Prelude.Text)
describeExplainabilityExportResponse_explainabilityArn = Lens.lens (\DescribeExplainabilityExportResponse' {explainabilityArn} -> explainabilityArn) (\s@DescribeExplainabilityExportResponse' {} a -> s {explainabilityArn = a} :: DescribeExplainabilityExportResponse)

-- | When the Explainability export was created.
describeExplainabilityExportResponse_creationTime :: Lens.Lens' DescribeExplainabilityExportResponse (Prelude.Maybe Prelude.UTCTime)
describeExplainabilityExportResponse_creationTime = Lens.lens (\DescribeExplainabilityExportResponse' {creationTime} -> creationTime) (\s@DescribeExplainabilityExportResponse' {} a -> s {creationTime = a} :: DescribeExplainabilityExportResponse) Prelude.. Lens.mapping Core._Time

-- | The response's http status code.
describeExplainabilityExportResponse_httpStatus :: Lens.Lens' DescribeExplainabilityExportResponse Prelude.Int
describeExplainabilityExportResponse_httpStatus = Lens.lens (\DescribeExplainabilityExportResponse' {httpStatus} -> httpStatus) (\s@DescribeExplainabilityExportResponse' {} a -> s {httpStatus = a} :: DescribeExplainabilityExportResponse)

instance
  Prelude.NFData
    DescribeExplainabilityExportResponse
  where
  rnf DescribeExplainabilityExportResponse' {..} =
    Prelude.rnf lastModificationTime
      `Prelude.seq` Prelude.rnf destination
      `Prelude.seq` Prelude.rnf explainabilityExportName
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf format
      `Prelude.seq` Prelude.rnf explainabilityExportArn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf explainabilityArn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf httpStatus
