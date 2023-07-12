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
-- Module      : Amazonka.CloudTrail.GetImport
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific import.
module Amazonka.CloudTrail.GetImport
  ( -- * Creating a Request
    GetImport (..),
    newGetImport,

    -- * Request Lenses
    getImport_importId,

    -- * Destructuring the Response
    GetImportResponse (..),
    newGetImportResponse,

    -- * Response Lenses
    getImportResponse_createdTimestamp,
    getImportResponse_destinations,
    getImportResponse_endEventTime,
    getImportResponse_importId,
    getImportResponse_importSource,
    getImportResponse_importStatistics,
    getImportResponse_importStatus,
    getImportResponse_startEventTime,
    getImportResponse_updatedTimestamp,
    getImportResponse_httpStatus,
  )
where

import Amazonka.CloudTrail.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetImport' smart constructor.
data GetImport = GetImport'
  { -- | The ID for the import.
    importId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetImport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'importId', 'getImport_importId' - The ID for the import.
newGetImport ::
  -- | 'importId'
  Prelude.Text ->
  GetImport
newGetImport pImportId_ =
  GetImport' {importId = pImportId_}

-- | The ID for the import.
getImport_importId :: Lens.Lens' GetImport Prelude.Text
getImport_importId = Lens.lens (\GetImport' {importId} -> importId) (\s@GetImport' {} a -> s {importId = a} :: GetImport)

instance Core.AWSRequest GetImport where
  type AWSResponse GetImport = GetImportResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetImportResponse'
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

instance Prelude.Hashable GetImport where
  hashWithSalt _salt GetImport' {..} =
    _salt `Prelude.hashWithSalt` importId

instance Prelude.NFData GetImport where
  rnf GetImport' {..} = Prelude.rnf importId

instance Data.ToHeaders GetImport where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.GetImport" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetImport where
  toJSON GetImport' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ImportId" Data..= importId)]
      )

instance Data.ToPath GetImport where
  toPath = Prelude.const "/"

instance Data.ToQuery GetImport where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetImportResponse' smart constructor.
data GetImportResponse = GetImportResponse'
  { -- | The timestamp of the import\'s creation.
    createdTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The ARN of the destination event data store.
    destinations :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | Used with @StartEventTime@ to bound a @StartImport@ request, and limit
    -- imported trail events to only those events logged within a specified
    -- time period.
    endEventTime :: Prelude.Maybe Data.POSIX,
    -- | The ID of the import.
    importId :: Prelude.Maybe Prelude.Text,
    -- | The source S3 bucket.
    importSource :: Prelude.Maybe ImportSource,
    -- | Provides statistics for the import. CloudTrail does not update import
    -- statistics in real-time. Returned values for parameters such as
    -- @EventsCompleted@ may be lower than the actual value, because CloudTrail
    -- updates statistics incrementally over the course of the import.
    importStatistics :: Prelude.Maybe ImportStatistics,
    -- | The status of the import.
    importStatus :: Prelude.Maybe ImportStatus,
    -- | Used with @EndEventTime@ to bound a @StartImport@ request, and limit
    -- imported trail events to only those events logged within a specified
    -- time period.
    startEventTime :: Prelude.Maybe Data.POSIX,
    -- | The timestamp of when the import was updated.
    updatedTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetImportResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdTimestamp', 'getImportResponse_createdTimestamp' - The timestamp of the import\'s creation.
--
-- 'destinations', 'getImportResponse_destinations' - The ARN of the destination event data store.
--
-- 'endEventTime', 'getImportResponse_endEventTime' - Used with @StartEventTime@ to bound a @StartImport@ request, and limit
-- imported trail events to only those events logged within a specified
-- time period.
--
-- 'importId', 'getImportResponse_importId' - The ID of the import.
--
-- 'importSource', 'getImportResponse_importSource' - The source S3 bucket.
--
-- 'importStatistics', 'getImportResponse_importStatistics' - Provides statistics for the import. CloudTrail does not update import
-- statistics in real-time. Returned values for parameters such as
-- @EventsCompleted@ may be lower than the actual value, because CloudTrail
-- updates statistics incrementally over the course of the import.
--
-- 'importStatus', 'getImportResponse_importStatus' - The status of the import.
--
-- 'startEventTime', 'getImportResponse_startEventTime' - Used with @EndEventTime@ to bound a @StartImport@ request, and limit
-- imported trail events to only those events logged within a specified
-- time period.
--
-- 'updatedTimestamp', 'getImportResponse_updatedTimestamp' - The timestamp of when the import was updated.
--
-- 'httpStatus', 'getImportResponse_httpStatus' - The response's http status code.
newGetImportResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetImportResponse
newGetImportResponse pHttpStatus_ =
  GetImportResponse'
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
getImportResponse_createdTimestamp :: Lens.Lens' GetImportResponse (Prelude.Maybe Prelude.UTCTime)
getImportResponse_createdTimestamp = Lens.lens (\GetImportResponse' {createdTimestamp} -> createdTimestamp) (\s@GetImportResponse' {} a -> s {createdTimestamp = a} :: GetImportResponse) Prelude.. Lens.mapping Data._Time

-- | The ARN of the destination event data store.
getImportResponse_destinations :: Lens.Lens' GetImportResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
getImportResponse_destinations = Lens.lens (\GetImportResponse' {destinations} -> destinations) (\s@GetImportResponse' {} a -> s {destinations = a} :: GetImportResponse) Prelude.. Lens.mapping Lens.coerced

-- | Used with @StartEventTime@ to bound a @StartImport@ request, and limit
-- imported trail events to only those events logged within a specified
-- time period.
getImportResponse_endEventTime :: Lens.Lens' GetImportResponse (Prelude.Maybe Prelude.UTCTime)
getImportResponse_endEventTime = Lens.lens (\GetImportResponse' {endEventTime} -> endEventTime) (\s@GetImportResponse' {} a -> s {endEventTime = a} :: GetImportResponse) Prelude.. Lens.mapping Data._Time

-- | The ID of the import.
getImportResponse_importId :: Lens.Lens' GetImportResponse (Prelude.Maybe Prelude.Text)
getImportResponse_importId = Lens.lens (\GetImportResponse' {importId} -> importId) (\s@GetImportResponse' {} a -> s {importId = a} :: GetImportResponse)

-- | The source S3 bucket.
getImportResponse_importSource :: Lens.Lens' GetImportResponse (Prelude.Maybe ImportSource)
getImportResponse_importSource = Lens.lens (\GetImportResponse' {importSource} -> importSource) (\s@GetImportResponse' {} a -> s {importSource = a} :: GetImportResponse)

-- | Provides statistics for the import. CloudTrail does not update import
-- statistics in real-time. Returned values for parameters such as
-- @EventsCompleted@ may be lower than the actual value, because CloudTrail
-- updates statistics incrementally over the course of the import.
getImportResponse_importStatistics :: Lens.Lens' GetImportResponse (Prelude.Maybe ImportStatistics)
getImportResponse_importStatistics = Lens.lens (\GetImportResponse' {importStatistics} -> importStatistics) (\s@GetImportResponse' {} a -> s {importStatistics = a} :: GetImportResponse)

-- | The status of the import.
getImportResponse_importStatus :: Lens.Lens' GetImportResponse (Prelude.Maybe ImportStatus)
getImportResponse_importStatus = Lens.lens (\GetImportResponse' {importStatus} -> importStatus) (\s@GetImportResponse' {} a -> s {importStatus = a} :: GetImportResponse)

-- | Used with @EndEventTime@ to bound a @StartImport@ request, and limit
-- imported trail events to only those events logged within a specified
-- time period.
getImportResponse_startEventTime :: Lens.Lens' GetImportResponse (Prelude.Maybe Prelude.UTCTime)
getImportResponse_startEventTime = Lens.lens (\GetImportResponse' {startEventTime} -> startEventTime) (\s@GetImportResponse' {} a -> s {startEventTime = a} :: GetImportResponse) Prelude.. Lens.mapping Data._Time

-- | The timestamp of when the import was updated.
getImportResponse_updatedTimestamp :: Lens.Lens' GetImportResponse (Prelude.Maybe Prelude.UTCTime)
getImportResponse_updatedTimestamp = Lens.lens (\GetImportResponse' {updatedTimestamp} -> updatedTimestamp) (\s@GetImportResponse' {} a -> s {updatedTimestamp = a} :: GetImportResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
getImportResponse_httpStatus :: Lens.Lens' GetImportResponse Prelude.Int
getImportResponse_httpStatus = Lens.lens (\GetImportResponse' {httpStatus} -> httpStatus) (\s@GetImportResponse' {} a -> s {httpStatus = a} :: GetImportResponse)

instance Prelude.NFData GetImportResponse where
  rnf GetImportResponse' {..} =
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
