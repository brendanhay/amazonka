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
-- Module      : Amazonka.CloudTrail.StopImport
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a specified import.
module Amazonka.CloudTrail.StopImport
  ( -- * Creating a Request
    StopImport (..),
    newStopImport,

    -- * Request Lenses
    stopImport_importId,

    -- * Destructuring the Response
    StopImportResponse (..),
    newStopImportResponse,

    -- * Response Lenses
    stopImportResponse_createdTimestamp,
    stopImportResponse_destinations,
    stopImportResponse_endEventTime,
    stopImportResponse_importId,
    stopImportResponse_importSource,
    stopImportResponse_importStatistics,
    stopImportResponse_importStatus,
    stopImportResponse_startEventTime,
    stopImportResponse_updatedTimestamp,
    stopImportResponse_httpStatus,
  )
where

import Amazonka.CloudTrail.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopImport' smart constructor.
data StopImport = StopImport'
  { -- | The ID of the import.
    importId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopImport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'importId', 'stopImport_importId' - The ID of the import.
newStopImport ::
  -- | 'importId'
  Prelude.Text ->
  StopImport
newStopImport pImportId_ =
  StopImport' {importId = pImportId_}

-- | The ID of the import.
stopImport_importId :: Lens.Lens' StopImport Prelude.Text
stopImport_importId = Lens.lens (\StopImport' {importId} -> importId) (\s@StopImport' {} a -> s {importId = a} :: StopImport)

instance Core.AWSRequest StopImport where
  type AWSResponse StopImport = StopImportResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StopImportResponse'
            Prelude.<$> (x Data..?> "CreatedTimestamp")
            Prelude.<*> (x Data..?> "Destinations")
            Prelude.<*> (x Data..?> "EndEventTime")
            Prelude.<*> (x Data..?> "ImportId")
            Prelude.<*> (x Data..?> "ImportSource")
            Prelude.<*> (x Data..?> "ImportStatistics")
            Prelude.<*> (x Data..?> "ImportStatus")
            Prelude.<*> (x Data..?> "StartEventTime")
            Prelude.<*> (x Data..?> "UpdatedTimestamp")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopImport where
  hashWithSalt _salt StopImport' {..} =
    _salt `Prelude.hashWithSalt` importId

instance Prelude.NFData StopImport where
  rnf StopImport' {..} = Prelude.rnf importId

instance Data.ToHeaders StopImport where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.StopImport" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopImport where
  toJSON StopImport' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ImportId" Data..= importId)]
      )

instance Data.ToPath StopImport where
  toPath = Prelude.const "/"

instance Data.ToQuery StopImport where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopImportResponse' smart constructor.
data StopImportResponse = StopImportResponse'
  { -- | The timestamp of the import\'s creation.
    createdTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The ARN of the destination event data store.
    destinations :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | Used with @StartEventTime@ to bound a @StartImport@ request, and limit
    -- imported trail events to only those events logged within a specified
    -- time period.
    endEventTime :: Prelude.Maybe Data.POSIX,
    -- | The ID for the import.
    importId :: Prelude.Maybe Prelude.Text,
    -- | The source S3 bucket for the import.
    importSource :: Prelude.Maybe ImportSource,
    -- | Returns information on the stopped import.
    importStatistics :: Prelude.Maybe ImportStatistics,
    -- | The status of the import.
    importStatus :: Prelude.Maybe ImportStatus,
    -- | Used with @EndEventTime@ to bound a @StartImport@ request, and limit
    -- imported trail events to only those events logged within a specified
    -- time period.
    startEventTime :: Prelude.Maybe Data.POSIX,
    -- | The timestamp of the import\'s last update.
    updatedTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopImportResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdTimestamp', 'stopImportResponse_createdTimestamp' - The timestamp of the import\'s creation.
--
-- 'destinations', 'stopImportResponse_destinations' - The ARN of the destination event data store.
--
-- 'endEventTime', 'stopImportResponse_endEventTime' - Used with @StartEventTime@ to bound a @StartImport@ request, and limit
-- imported trail events to only those events logged within a specified
-- time period.
--
-- 'importId', 'stopImportResponse_importId' - The ID for the import.
--
-- 'importSource', 'stopImportResponse_importSource' - The source S3 bucket for the import.
--
-- 'importStatistics', 'stopImportResponse_importStatistics' - Returns information on the stopped import.
--
-- 'importStatus', 'stopImportResponse_importStatus' - The status of the import.
--
-- 'startEventTime', 'stopImportResponse_startEventTime' - Used with @EndEventTime@ to bound a @StartImport@ request, and limit
-- imported trail events to only those events logged within a specified
-- time period.
--
-- 'updatedTimestamp', 'stopImportResponse_updatedTimestamp' - The timestamp of the import\'s last update.
--
-- 'httpStatus', 'stopImportResponse_httpStatus' - The response's http status code.
newStopImportResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopImportResponse
newStopImportResponse pHttpStatus_ =
  StopImportResponse'
    { createdTimestamp =
        Prelude.Nothing,
      destinations = Prelude.Nothing,
      endEventTime = Prelude.Nothing,
      importId = Prelude.Nothing,
      importSource = Prelude.Nothing,
      importStatistics = Prelude.Nothing,
      importStatus = Prelude.Nothing,
      startEventTime = Prelude.Nothing,
      updatedTimestamp = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The timestamp of the import\'s creation.
stopImportResponse_createdTimestamp :: Lens.Lens' StopImportResponse (Prelude.Maybe Prelude.UTCTime)
stopImportResponse_createdTimestamp = Lens.lens (\StopImportResponse' {createdTimestamp} -> createdTimestamp) (\s@StopImportResponse' {} a -> s {createdTimestamp = a} :: StopImportResponse) Prelude.. Lens.mapping Data._Time

-- | The ARN of the destination event data store.
stopImportResponse_destinations :: Lens.Lens' StopImportResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
stopImportResponse_destinations = Lens.lens (\StopImportResponse' {destinations} -> destinations) (\s@StopImportResponse' {} a -> s {destinations = a} :: StopImportResponse) Prelude.. Lens.mapping Lens.coerced

-- | Used with @StartEventTime@ to bound a @StartImport@ request, and limit
-- imported trail events to only those events logged within a specified
-- time period.
stopImportResponse_endEventTime :: Lens.Lens' StopImportResponse (Prelude.Maybe Prelude.UTCTime)
stopImportResponse_endEventTime = Lens.lens (\StopImportResponse' {endEventTime} -> endEventTime) (\s@StopImportResponse' {} a -> s {endEventTime = a} :: StopImportResponse) Prelude.. Lens.mapping Data._Time

-- | The ID for the import.
stopImportResponse_importId :: Lens.Lens' StopImportResponse (Prelude.Maybe Prelude.Text)
stopImportResponse_importId = Lens.lens (\StopImportResponse' {importId} -> importId) (\s@StopImportResponse' {} a -> s {importId = a} :: StopImportResponse)

-- | The source S3 bucket for the import.
stopImportResponse_importSource :: Lens.Lens' StopImportResponse (Prelude.Maybe ImportSource)
stopImportResponse_importSource = Lens.lens (\StopImportResponse' {importSource} -> importSource) (\s@StopImportResponse' {} a -> s {importSource = a} :: StopImportResponse)

-- | Returns information on the stopped import.
stopImportResponse_importStatistics :: Lens.Lens' StopImportResponse (Prelude.Maybe ImportStatistics)
stopImportResponse_importStatistics = Lens.lens (\StopImportResponse' {importStatistics} -> importStatistics) (\s@StopImportResponse' {} a -> s {importStatistics = a} :: StopImportResponse)

-- | The status of the import.
stopImportResponse_importStatus :: Lens.Lens' StopImportResponse (Prelude.Maybe ImportStatus)
stopImportResponse_importStatus = Lens.lens (\StopImportResponse' {importStatus} -> importStatus) (\s@StopImportResponse' {} a -> s {importStatus = a} :: StopImportResponse)

-- | Used with @EndEventTime@ to bound a @StartImport@ request, and limit
-- imported trail events to only those events logged within a specified
-- time period.
stopImportResponse_startEventTime :: Lens.Lens' StopImportResponse (Prelude.Maybe Prelude.UTCTime)
stopImportResponse_startEventTime = Lens.lens (\StopImportResponse' {startEventTime} -> startEventTime) (\s@StopImportResponse' {} a -> s {startEventTime = a} :: StopImportResponse) Prelude.. Lens.mapping Data._Time

-- | The timestamp of the import\'s last update.
stopImportResponse_updatedTimestamp :: Lens.Lens' StopImportResponse (Prelude.Maybe Prelude.UTCTime)
stopImportResponse_updatedTimestamp = Lens.lens (\StopImportResponse' {updatedTimestamp} -> updatedTimestamp) (\s@StopImportResponse' {} a -> s {updatedTimestamp = a} :: StopImportResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
stopImportResponse_httpStatus :: Lens.Lens' StopImportResponse Prelude.Int
stopImportResponse_httpStatus = Lens.lens (\StopImportResponse' {httpStatus} -> httpStatus) (\s@StopImportResponse' {} a -> s {httpStatus = a} :: StopImportResponse)

instance Prelude.NFData StopImportResponse where
  rnf StopImportResponse' {..} =
    Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf destinations
      `Prelude.seq` Prelude.rnf endEventTime
      `Prelude.seq` Prelude.rnf importId
      `Prelude.seq` Prelude.rnf importSource
      `Prelude.seq` Prelude.rnf importStatistics
      `Prelude.seq` Prelude.rnf importStatus
      `Prelude.seq` Prelude.rnf startEventTime
      `Prelude.seq` Prelude.rnf updatedTimestamp
      `Prelude.seq` Prelude.rnf httpStatus
