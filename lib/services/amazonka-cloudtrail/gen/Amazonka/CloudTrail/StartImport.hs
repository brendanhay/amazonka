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
-- Module      : Amazonka.CloudTrail.StartImport
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an import of logged trail events from a source S3 bucket to a
-- destination event data store. By default, CloudTrail only imports events
-- contained in the S3 bucket\'s @CloudTrail@ prefix and the prefixes
-- inside the @CloudTrail@ prefix, and does not check prefixes for other
-- Amazon Web Services services. If you want to import CloudTrail events
-- contained in another prefix, you must include the prefix in the
-- @S3LocationUri@. For more considerations about importing trail events,
-- see
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-copy-trail-to-lake.html#cloudtrail-trail-copy-considerations Considerations>.
--
-- When you start a new import, the @Destinations@ and @ImportSource@
-- parameters are required. Before starting a new import, disable any
-- access control lists (ACLs) attached to the source S3 bucket. For more
-- information about disabling ACLs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/about-object-ownership.html Controlling ownership of objects and disabling ACLs for your bucket>.
--
-- When you retry an import, the @ImportID@ parameter is required.
module Amazonka.CloudTrail.StartImport
  ( -- * Creating a Request
    StartImport (..),
    newStartImport,

    -- * Request Lenses
    startImport_importSource,
    startImport_endEventTime,
    startImport_startEventTime,
    startImport_importId,
    startImport_destinations,

    -- * Destructuring the Response
    StartImportResponse (..),
    newStartImportResponse,

    -- * Response Lenses
    startImportResponse_importSource,
    startImportResponse_endEventTime,
    startImportResponse_createdTimestamp,
    startImportResponse_updatedTimestamp,
    startImportResponse_startEventTime,
    startImportResponse_importId,
    startImportResponse_importStatus,
    startImportResponse_destinations,
    startImportResponse_httpStatus,
  )
where

import Amazonka.CloudTrail.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartImport' smart constructor.
data StartImport = StartImport'
  { -- | The source S3 bucket for the import. Use this parameter for a new
    -- import.
    importSource :: Prelude.Maybe ImportSource,
    -- | Use with @StartEventTime@ to bound a @StartImport@ request, and limit
    -- imported trail events to only those events logged within a specified
    -- time period. When you specify a time range, CloudTrail checks the prefix
    -- and log file names to verify the names contain a date between the
    -- specified @StartEventTime@ and @EndEventTime@ before attempting to
    -- import events.
    endEventTime :: Prelude.Maybe Core.POSIX,
    -- | Use with @EndEventTime@ to bound a @StartImport@ request, and limit
    -- imported trail events to only those events logged within a specified
    -- time period. When you specify a time range, CloudTrail checks the prefix
    -- and log file names to verify the names contain a date between the
    -- specified @StartEventTime@ and @EndEventTime@ before attempting to
    -- import events.
    startEventTime :: Prelude.Maybe Core.POSIX,
    -- | The ID of the import. Use this parameter when you are retrying an
    -- import.
    importId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the destination event data store. Use this parameter for a
    -- new import.
    destinations :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartImport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'importSource', 'startImport_importSource' - The source S3 bucket for the import. Use this parameter for a new
-- import.
--
-- 'endEventTime', 'startImport_endEventTime' - Use with @StartEventTime@ to bound a @StartImport@ request, and limit
-- imported trail events to only those events logged within a specified
-- time period. When you specify a time range, CloudTrail checks the prefix
-- and log file names to verify the names contain a date between the
-- specified @StartEventTime@ and @EndEventTime@ before attempting to
-- import events.
--
-- 'startEventTime', 'startImport_startEventTime' - Use with @EndEventTime@ to bound a @StartImport@ request, and limit
-- imported trail events to only those events logged within a specified
-- time period. When you specify a time range, CloudTrail checks the prefix
-- and log file names to verify the names contain a date between the
-- specified @StartEventTime@ and @EndEventTime@ before attempting to
-- import events.
--
-- 'importId', 'startImport_importId' - The ID of the import. Use this parameter when you are retrying an
-- import.
--
-- 'destinations', 'startImport_destinations' - The ARN of the destination event data store. Use this parameter for a
-- new import.
newStartImport ::
  StartImport
newStartImport =
  StartImport'
    { importSource = Prelude.Nothing,
      endEventTime = Prelude.Nothing,
      startEventTime = Prelude.Nothing,
      importId = Prelude.Nothing,
      destinations = Prelude.Nothing
    }

-- | The source S3 bucket for the import. Use this parameter for a new
-- import.
startImport_importSource :: Lens.Lens' StartImport (Prelude.Maybe ImportSource)
startImport_importSource = Lens.lens (\StartImport' {importSource} -> importSource) (\s@StartImport' {} a -> s {importSource = a} :: StartImport)

-- | Use with @StartEventTime@ to bound a @StartImport@ request, and limit
-- imported trail events to only those events logged within a specified
-- time period. When you specify a time range, CloudTrail checks the prefix
-- and log file names to verify the names contain a date between the
-- specified @StartEventTime@ and @EndEventTime@ before attempting to
-- import events.
startImport_endEventTime :: Lens.Lens' StartImport (Prelude.Maybe Prelude.UTCTime)
startImport_endEventTime = Lens.lens (\StartImport' {endEventTime} -> endEventTime) (\s@StartImport' {} a -> s {endEventTime = a} :: StartImport) Prelude.. Lens.mapping Core._Time

-- | Use with @EndEventTime@ to bound a @StartImport@ request, and limit
-- imported trail events to only those events logged within a specified
-- time period. When you specify a time range, CloudTrail checks the prefix
-- and log file names to verify the names contain a date between the
-- specified @StartEventTime@ and @EndEventTime@ before attempting to
-- import events.
startImport_startEventTime :: Lens.Lens' StartImport (Prelude.Maybe Prelude.UTCTime)
startImport_startEventTime = Lens.lens (\StartImport' {startEventTime} -> startEventTime) (\s@StartImport' {} a -> s {startEventTime = a} :: StartImport) Prelude.. Lens.mapping Core._Time

-- | The ID of the import. Use this parameter when you are retrying an
-- import.
startImport_importId :: Lens.Lens' StartImport (Prelude.Maybe Prelude.Text)
startImport_importId = Lens.lens (\StartImport' {importId} -> importId) (\s@StartImport' {} a -> s {importId = a} :: StartImport)

-- | The ARN of the destination event data store. Use this parameter for a
-- new import.
startImport_destinations :: Lens.Lens' StartImport (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
startImport_destinations = Lens.lens (\StartImport' {destinations} -> destinations) (\s@StartImport' {} a -> s {destinations = a} :: StartImport) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest StartImport where
  type AWSResponse StartImport = StartImportResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartImportResponse'
            Prelude.<$> (x Core..?> "ImportSource")
            Prelude.<*> (x Core..?> "EndEventTime")
            Prelude.<*> (x Core..?> "CreatedTimestamp")
            Prelude.<*> (x Core..?> "UpdatedTimestamp")
            Prelude.<*> (x Core..?> "StartEventTime")
            Prelude.<*> (x Core..?> "ImportId")
            Prelude.<*> (x Core..?> "ImportStatus")
            Prelude.<*> (x Core..?> "Destinations")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartImport where
  hashWithSalt _salt StartImport' {..} =
    _salt `Prelude.hashWithSalt` importSource
      `Prelude.hashWithSalt` endEventTime
      `Prelude.hashWithSalt` startEventTime
      `Prelude.hashWithSalt` importId
      `Prelude.hashWithSalt` destinations

instance Prelude.NFData StartImport where
  rnf StartImport' {..} =
    Prelude.rnf importSource
      `Prelude.seq` Prelude.rnf endEventTime
      `Prelude.seq` Prelude.rnf startEventTime
      `Prelude.seq` Prelude.rnf importId
      `Prelude.seq` Prelude.rnf destinations

instance Core.ToHeaders StartImport where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.StartImport" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StartImport where
  toJSON StartImport' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ImportSource" Core..=) Prelude.<$> importSource,
            ("EndEventTime" Core..=) Prelude.<$> endEventTime,
            ("StartEventTime" Core..=)
              Prelude.<$> startEventTime,
            ("ImportId" Core..=) Prelude.<$> importId,
            ("Destinations" Core..=) Prelude.<$> destinations
          ]
      )

instance Core.ToPath StartImport where
  toPath = Prelude.const "/"

instance Core.ToQuery StartImport where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartImportResponse' smart constructor.
data StartImportResponse = StartImportResponse'
  { -- | The source S3 bucket for the import.
    importSource :: Prelude.Maybe ImportSource,
    -- | Used with @StartEventTime@ to bound a @StartImport@ request, and limit
    -- imported trail events to only those events logged within a specified
    -- time period.
    endEventTime :: Prelude.Maybe Core.POSIX,
    -- | The timestamp for the import\'s creation.
    createdTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The timestamp of the import\'s last update, if applicable.
    updatedTimestamp :: Prelude.Maybe Core.POSIX,
    -- | Used with @EndEventTime@ to bound a @StartImport@ request, and limit
    -- imported trail events to only those events logged within a specified
    -- time period.
    startEventTime :: Prelude.Maybe Core.POSIX,
    -- | The ID of the import.
    importId :: Prelude.Maybe Prelude.Text,
    -- | Shows the status of the import after a @StartImport@ request. An import
    -- finishes with a status of @COMPLETED@ if there were no failures, or
    -- @FAILED@ if there were failures.
    importStatus :: Prelude.Maybe ImportStatus,
    -- | The ARN of the destination event data store.
    destinations :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartImportResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'importSource', 'startImportResponse_importSource' - The source S3 bucket for the import.
--
-- 'endEventTime', 'startImportResponse_endEventTime' - Used with @StartEventTime@ to bound a @StartImport@ request, and limit
-- imported trail events to only those events logged within a specified
-- time period.
--
-- 'createdTimestamp', 'startImportResponse_createdTimestamp' - The timestamp for the import\'s creation.
--
-- 'updatedTimestamp', 'startImportResponse_updatedTimestamp' - The timestamp of the import\'s last update, if applicable.
--
-- 'startEventTime', 'startImportResponse_startEventTime' - Used with @EndEventTime@ to bound a @StartImport@ request, and limit
-- imported trail events to only those events logged within a specified
-- time period.
--
-- 'importId', 'startImportResponse_importId' - The ID of the import.
--
-- 'importStatus', 'startImportResponse_importStatus' - Shows the status of the import after a @StartImport@ request. An import
-- finishes with a status of @COMPLETED@ if there were no failures, or
-- @FAILED@ if there were failures.
--
-- 'destinations', 'startImportResponse_destinations' - The ARN of the destination event data store.
--
-- 'httpStatus', 'startImportResponse_httpStatus' - The response's http status code.
newStartImportResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartImportResponse
newStartImportResponse pHttpStatus_ =
  StartImportResponse'
    { importSource =
        Prelude.Nothing,
      endEventTime = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing,
      updatedTimestamp = Prelude.Nothing,
      startEventTime = Prelude.Nothing,
      importId = Prelude.Nothing,
      importStatus = Prelude.Nothing,
      destinations = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The source S3 bucket for the import.
startImportResponse_importSource :: Lens.Lens' StartImportResponse (Prelude.Maybe ImportSource)
startImportResponse_importSource = Lens.lens (\StartImportResponse' {importSource} -> importSource) (\s@StartImportResponse' {} a -> s {importSource = a} :: StartImportResponse)

-- | Used with @StartEventTime@ to bound a @StartImport@ request, and limit
-- imported trail events to only those events logged within a specified
-- time period.
startImportResponse_endEventTime :: Lens.Lens' StartImportResponse (Prelude.Maybe Prelude.UTCTime)
startImportResponse_endEventTime = Lens.lens (\StartImportResponse' {endEventTime} -> endEventTime) (\s@StartImportResponse' {} a -> s {endEventTime = a} :: StartImportResponse) Prelude.. Lens.mapping Core._Time

-- | The timestamp for the import\'s creation.
startImportResponse_createdTimestamp :: Lens.Lens' StartImportResponse (Prelude.Maybe Prelude.UTCTime)
startImportResponse_createdTimestamp = Lens.lens (\StartImportResponse' {createdTimestamp} -> createdTimestamp) (\s@StartImportResponse' {} a -> s {createdTimestamp = a} :: StartImportResponse) Prelude.. Lens.mapping Core._Time

-- | The timestamp of the import\'s last update, if applicable.
startImportResponse_updatedTimestamp :: Lens.Lens' StartImportResponse (Prelude.Maybe Prelude.UTCTime)
startImportResponse_updatedTimestamp = Lens.lens (\StartImportResponse' {updatedTimestamp} -> updatedTimestamp) (\s@StartImportResponse' {} a -> s {updatedTimestamp = a} :: StartImportResponse) Prelude.. Lens.mapping Core._Time

-- | Used with @EndEventTime@ to bound a @StartImport@ request, and limit
-- imported trail events to only those events logged within a specified
-- time period.
startImportResponse_startEventTime :: Lens.Lens' StartImportResponse (Prelude.Maybe Prelude.UTCTime)
startImportResponse_startEventTime = Lens.lens (\StartImportResponse' {startEventTime} -> startEventTime) (\s@StartImportResponse' {} a -> s {startEventTime = a} :: StartImportResponse) Prelude.. Lens.mapping Core._Time

-- | The ID of the import.
startImportResponse_importId :: Lens.Lens' StartImportResponse (Prelude.Maybe Prelude.Text)
startImportResponse_importId = Lens.lens (\StartImportResponse' {importId} -> importId) (\s@StartImportResponse' {} a -> s {importId = a} :: StartImportResponse)

-- | Shows the status of the import after a @StartImport@ request. An import
-- finishes with a status of @COMPLETED@ if there were no failures, or
-- @FAILED@ if there were failures.
startImportResponse_importStatus :: Lens.Lens' StartImportResponse (Prelude.Maybe ImportStatus)
startImportResponse_importStatus = Lens.lens (\StartImportResponse' {importStatus} -> importStatus) (\s@StartImportResponse' {} a -> s {importStatus = a} :: StartImportResponse)

-- | The ARN of the destination event data store.
startImportResponse_destinations :: Lens.Lens' StartImportResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
startImportResponse_destinations = Lens.lens (\StartImportResponse' {destinations} -> destinations) (\s@StartImportResponse' {} a -> s {destinations = a} :: StartImportResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
startImportResponse_httpStatus :: Lens.Lens' StartImportResponse Prelude.Int
startImportResponse_httpStatus = Lens.lens (\StartImportResponse' {httpStatus} -> httpStatus) (\s@StartImportResponse' {} a -> s {httpStatus = a} :: StartImportResponse)

instance Prelude.NFData StartImportResponse where
  rnf StartImportResponse' {..} =
    Prelude.rnf importSource
      `Prelude.seq` Prelude.rnf endEventTime
      `Prelude.seq` Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf updatedTimestamp
      `Prelude.seq` Prelude.rnf startEventTime
      `Prelude.seq` Prelude.rnf importId
      `Prelude.seq` Prelude.rnf importStatus
      `Prelude.seq` Prelude.rnf destinations
      `Prelude.seq` Prelude.rnf httpStatus
